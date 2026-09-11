# R/price_model.R
#
# The live limit order book used to run a simulation forward: apply one
# order-flow primitive at a time (new limit, new market, cancel) and mutate
# the book in place. Mid-price is never set directly -- book_best() always
# derives it from best_bid/best_ask, so it is a *consequence* of whatever
# orders have been applied, not an input. R/event_generator.R decides WHEN
# and WHAT arrives; this file decides what that arrival does to the book.
#
# Also provides generate_reference_random_walk(): a fully decoupled
# Brownian-motion price path with no connection to the book, for comparing
# the emergent price process against -- never fed into the simulation.

library(data.table)
library(bit64)

# ---------------------------------------------------------------------------
# Book construction
# ---------------------------------------------------------------------------

# A level is list(oids = character vector, qtys = numeric vector), a FIFO
# queue in price-time priority: oids[1]/qtys[1] is the front of the queue.
new_book <- function(tick_size = 0.01, initial_mid = 100, bootstrap_levels = 10,
                      bootstrap_depth = 300, start_ts_ns, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  book <- new.env(parent = emptyenv())
  book$tick_size <- tick_size
  book$bid_levels <- new.env(parent = emptyenv())
  book$ask_levels <- new.env(parent = emptyenv())
  book$orders <- new.env(parent = emptyenv()) # order_id -> list(side, price, remaining)
  book$best_bid <- NA_real_
  book$best_ask <- NA_real_
  book$total_bid_depth <- 0
  book$total_ask_depth <- 0
  book$next_order_id <- 0L
  book$next_seq <- 0L

  best_bid0 <- floor(initial_mid / tick_size) * tick_size
  best_ask0 <- best_bid0 + tick_size

  bootstrap_rows <- vector("list", 2 * bootstrap_levels)
  k <- 0L
  for (i in seq_len(bootstrap_levels)) {
    px_bid <- best_bid0 - (i - 1) * tick_size
    px_ask <- best_ask0 + (i - 1) * tick_size
    k <- k + 1L
    bootstrap_rows[[k]] <- book_apply_new_limit(book, "B", px_bid, bootstrap_depth, start_ts_ns)
    k <- k + 1L
    bootstrap_rows[[k]] <- book_apply_new_limit(book, "S", px_ask, bootstrap_depth, start_ts_ns)
  }

  list(book = book, bootstrap_events = data.table::rbindlist(bootstrap_rows))
}

# ---------------------------------------------------------------------------
# Internal level/price helpers
# ---------------------------------------------------------------------------

price_key <- function(book, p) sprintf("%d", as.integer(round(p / book$tick_size)))

rescan_best <- function(book, side) {
  env <- if (side == "B") book$bid_levels else book$ask_levels
  keys <- ls(env, all.names = TRUE)
  if (length(keys) == 0) {
    return(NA_real_)
  }
  prices <- as.numeric(keys) * book$tick_size
  if (side == "B") max(prices) else min(prices)
}

next_seq <- function(book) {
  book$next_seq <- book$next_seq + 1L
  book$next_seq
}

next_order_id <- function(book) {
  book$next_order_id <- book$next_order_id + 1L
  as.character(book$next_order_id)
}

side_label <- function(side_code) if (side_code == "B") "BUY" else "SELL"

make_row <- function(book, ts_ns, event, order_id, side_code, type, price, quantity) {
  list(
    seq = next_seq(book),
    ts_ns = ts_ns,
    event = event,
    order_id = order_id,
    side = side_label(side_code),
    type = type,
    price = if (is.na(price)) NA_real_ else round(price * 10000),
    quantity = quantity
  )
}

# Public accessors -----------------------------------------------------------

book_best <- function(book) {
  list(
    best_bid = book$best_bid, best_ask = book$best_ask,
    mid = if (is.na(book$best_bid) || is.na(book$best_ask)) NA_real_ else (book$best_bid + book$best_ask) / 2,
    spread = if (is.na(book$best_bid) || is.na(book$best_ask)) NA_real_ else book$best_ask - book$best_bid
  )
}

book_depth <- function(book) {
  list(bid = book$total_bid_depth, ask = book$total_ask_depth)
}

# ---------------------------------------------------------------------------
# Order-flow primitives. Each mutates `book` in place and returns a
# data.table of one or more interchange-schema rows (R/types.R).
# ---------------------------------------------------------------------------

book_apply_new_limit <- function(book, side, price, qty, ts_ns) {
  oid <- next_order_id(book)
  env <- if (side == "B") book$bid_levels else book$ask_levels
  key <- price_key(book, price)

  if (exists(key, envir = env, inherits = FALSE)) {
    lvl <- get(key, envir = env, inherits = FALSE)
    lvl$oids <- c(lvl$oids, oid)
    lvl$qtys <- c(lvl$qtys, qty)
  } else {
    lvl <- list(oids = oid, qtys = qty)
  }
  assign(key, lvl, envir = env)
  assign(oid, list(side = side, price = price, remaining = qty), envir = book$orders)

  if (side == "B") {
    book$total_bid_depth <- book$total_bid_depth + qty
    if (is.na(book$best_bid) || price > book$best_bid) book$best_bid <- price
  } else {
    book$total_ask_depth <- book$total_ask_depth + qty
    if (is.na(book$best_ask) || price < book$best_ask) book$best_ask <- price
  }

  data.table::rbindlist(list(make_row(book, ts_ns, "NEW", oid, side, "LIMIT", price, qty)))
}

book_apply_market <- function(book, side, qty, ts_ns) {
  oid <- next_order_id(book)
  rows <- list(make_row(book, ts_ns, "NEW", oid, side, "MARKET", NA_real_, qty))

  opp_side <- if (side == "B") "S" else "B"
  env <- if (opp_side == "B") book$bid_levels else book$ask_levels
  remaining <- qty

  while (remaining > 1e-9) {
    best <- if (opp_side == "B") book$best_bid else book$best_ask
    if (is.na(best)) break # opposite side exhausted; nothing left to match against

    key <- price_key(book, best)
    lvl <- get(key, envir = env, inherits = FALSE)

    while (remaining > 1e-9 && length(lvl$oids) > 0) {
      front_oid <- lvl$oids[1]
      front_qty <- lvl$qtys[1]
      fill <- min(remaining, front_qty)
      remaining <- remaining - fill
      left <- front_qty - fill

      o <- get(front_oid, envir = book$orders, inherits = FALSE)
      if (opp_side == "B") book$total_bid_depth <- book$total_bid_depth - fill else book$total_ask_depth <- book$total_ask_depth - fill

      if (left <= 1e-9) {
        lvl$oids <- lvl$oids[-1]
        lvl$qtys <- lvl$qtys[-1]
        rm(list = front_oid, envir = book$orders)
        rows[[length(rows) + 1]] <- make_row(book, ts_ns, "CANCEL", front_oid, opp_side, "LIMIT", best, front_qty)
      } else {
        lvl$qtys[1] <- left
        o$remaining <- left
        assign(front_oid, o, envir = book$orders)
        rows[[length(rows) + 1]] <- make_row(book, ts_ns, "REDUCE", front_oid, opp_side, "LIMIT", best, left)
      }
    }

    if (length(lvl$oids) == 0) {
      rm(list = key, envir = env)
      if (opp_side == "B") book$best_bid <- rescan_best(book, "B") else book$best_ask <- rescan_best(book, "S")
    } else {
      assign(key, lvl, envir = env)
    }
  }

  data.table::rbindlist(rows)
}

# Pick one resting order on `side`, with probability proportional to its own
# remaining size (equivalently: a uniformly random resting *share*). Two-stage
# sampling (level weighted by its total size, then order within the level
# weighted by its own size) is exactly equivalent to flat weighted sampling
# over every resting order, and much cheaper.
book_pick_random_order <- function(book, side) {
  env <- if (side == "B") book$bid_levels else book$ask_levels
  keys <- ls(env, all.names = TRUE)
  if (length(keys) == 0) {
    return(NULL)
  }
  level_sizes <- vapply(keys, function(k) sum(get(k, envir = env, inherits = FALSE)$qtys), numeric(1))
  chosen_key <- if (length(keys) == 1) keys else sample(keys, 1, prob = level_sizes)
  lvl <- get(chosen_key, envir = env, inherits = FALSE)
  chosen_idx <- if (length(lvl$oids) == 1) 1L else sample(seq_along(lvl$oids), 1, prob = lvl$qtys)
  list(
    price = as.numeric(chosen_key) * book$tick_size, order_id = lvl$oids[chosen_idx],
    qty = lvl$qtys[chosen_idx], level_key = chosen_key, idx = chosen_idx
  )
}

book_apply_cancel <- function(book, side, ts_ns) {
  picked <- book_pick_random_order(book, side)
  if (is.null(picked)) {
    return(data.table::data.table())
  }

  env <- if (side == "B") book$bid_levels else book$ask_levels
  lvl <- get(picked$level_key, envir = env, inherits = FALSE)
  lvl$oids <- lvl$oids[-picked$idx]
  lvl$qtys <- lvl$qtys[-picked$idx]
  if (length(lvl$oids) == 0) {
    rm(list = picked$level_key, envir = env)
  } else {
    assign(picked$level_key, lvl, envir = env)
  }
  rm(list = picked$order_id, envir = book$orders)

  # Compare price *keys* (exact integer ticks), not floating-point prices:
  # picked$price and book$best_bid/best_ask can reach the "same" price via
  # different arithmetic paths (key*tick_size vs. direct subtraction), and
  # a `==` on the resulting doubles can silently fail on the last bit,
  # skipping a needed rescan and leaving best_bid/best_ask stale.
  level_gone <- !exists(picked$level_key, envir = env, inherits = FALSE)
  if (side == "B") {
    book$total_bid_depth <- book$total_bid_depth - picked$qty
    if (level_gone && !is.na(book$best_bid) && price_key(book, book$best_bid) == picked$level_key) {
      book$best_bid <- rescan_best(book, "B")
    }
  } else {
    book$total_ask_depth <- book$total_ask_depth - picked$qty
    if (level_gone && !is.na(book$best_ask) && price_key(book, book$best_ask) == picked$level_key) {
      book$best_ask <- rescan_best(book, "S")
    }
  }

  data.table::rbindlist(list(make_row(book, ts_ns, "CANCEL", picked$order_id, side, "LIMIT", picked$price, picked$qty)))
}

# ---------------------------------------------------------------------------
# Reference random walk -- deliberately NOT connected to the book above.
# Provided only so a simulation's emergent mid-price can be compared against
# a naive exogenous alternative, per the instruction not to feed one in.
# ---------------------------------------------------------------------------

generate_reference_random_walk <- function(duration_s, dt_s = 1, mid0 = 100,
                                            sigma_per_sqrt_s = 0.01, start_ts_ns,
                                            seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_steps <- max(1L, round(duration_s / dt_s))
  increments <- stats::rnorm(n_steps, mean = 0, sd = sigma_per_sqrt_s * sqrt(dt_s))
  mid <- mid0 + cumsum(c(0, increments))
  ts_ns <- start_ts_ns + bit64::as.integer64(round(seq(0, n_steps) * dt_s * 1e9))
  data.table::data.table(ts_ns = ts_ns, mid = mid)
}
