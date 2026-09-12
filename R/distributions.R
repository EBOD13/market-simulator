# R/distributions.R
#
# Calibrate order-flow distributions from one ITCH CSV for one symbol-day
# (the output of R/data_loader.R's load_itch_csv()). Everything here is
# *estimation*, not modeling: it measures what the data actually did and
# says plainly where a simple model (Exponential arrivals) fails, rather
# than silently picking a distribution that fits badly.
#
# Entry point: calibrate_distributions(dt, tick_size = 0.01)

library(data.table)
library(bit64)
library(survival)

# Reference event mix measured on 20190730.BX_ITCH_50, all 8849 symbols,
# 24,074,237 events. ADD combines ITCH's A (ADD_ORDER) and F
# (ADD_ORDER_MPID); EXECUTED combines E (EXECUTED) and C
# (EXECUTED_WITH_PRICE) -- same as this file's classify_event().
REFERENCE_EVENT_MIX <- c(
  ADD = 0.440,
  DELETE = 0.422,
  REPLACE = 0.085,
  EXECUTED = 0.028,
  CANCEL_PARTIAL = 0.012,
  TRADE_NON_CROSS = 0.010
)

# ---------------------------------------------------------------------------
# Event classification
# ---------------------------------------------------------------------------

# ITCH 5.0 message codes -> canonical event classes. A/F and E/C are
# collapsed because they are the same book-level event, differing only in
# whether an MPID attribution or an off-price execution is attached.
classify_event <- function(msg) {
  out <- rep(NA_character_, length(msg))
  out[msg %in% c("A", "F")] <- "ADD"
  out[msg == "D"] <- "DELETE"
  out[msg == "U"] <- "REPLACE"
  out[msg %in% c("E", "C")] <- "EXECUTED"
  out[msg == "X"] <- "CANCEL_PARTIAL"
  out[msg == "P"] <- "TRADE_NON_CROSS"
  out
}

# ---------------------------------------------------------------------------
# Event mix
# ---------------------------------------------------------------------------

calibrate_event_mix <- function(dt) {
  event <- classify_event(dt$msg)
  n_total <- length(event)
  counts <- table(factor(event, levels = names(REFERENCE_EVENT_MIX)))
  observed <- as.numeric(counts) / n_total
  names(observed) <- names(REFERENCE_EVENT_MIX)

  diff_pp <- (observed - REFERENCE_EVENT_MIX) * 100
  flagged <- abs(diff_pp) > 5 # more than 5 percentage points off

  note <- if (any(flagged)) {
    paste0(
      "Event mix disagrees with the reference by >5 percentage points for: ",
      paste(names(REFERENCE_EVENT_MIX)[flagged], collapse = ", "),
      ". This is expected, not a bug: the reference mix averages all 8849 ",
      "BX symbols that day (mostly thin/illiquid names), while a single ",
      "liquid symbol's event composition need not match the market-wide ",
      "average."
    )
  } else {
    "Event mix matches the reference mix within 5 percentage points."
  }

  list(
    n_events = n_total,
    n_unclassified = sum(is.na(event)),
    observed = observed,
    reference = REFERENCE_EVENT_MIX,
    diff_pp = diff_pp,
    flagged = flagged,
    note = note
  )
}

# ---------------------------------------------------------------------------
# Interarrival times per event class: Exponential MLE + KS test
# ---------------------------------------------------------------------------

fit_exponential_interarrival <- function(ts_ns, class_name, alpha = 0.05) {
  ts_ns <- sort(ts_ns)
  if (length(ts_ns) < 10) {
    return(list(
      class = class_name, n_events = length(ts_ns),
      note = "Too few events to fit a meaningful interarrival distribution."
    ))
  }

  gaps_ns <- ts_ns[-1] - ts_ns[-length(ts_ns)]
  gaps_s <- as.numeric(gaps_ns) / 1e9
  gaps_s <- gaps_s[gaps_s > 0] # simultaneous (same-ns) events give a zero gap

  rate_hat <- 1 / mean(gaps_s) # Exponential MLE: lambda_hat = 1 / mean(gap)
  ks <- ks.test(gaps_s, "pexp", rate = rate_hat, exact = FALSE)
  rejected <- ks$p.value < alpha

  verdict <- paste0(
    class_name, ": Exponential(rate=", signif(rate_hat, 4), "/s) ",
    if (rejected) "REJECTED" else "not rejected",
    " at alpha=", alpha,
    " (KS D=", round(unname(ks$statistic), 4), ", p=", signif(ks$p.value, 3), ").",
    if (rejected) {
      paste0(
        " Real order flow clusters in time (self-exciting bursts) that a ",
        "memoryless Exponential cannot represent -- this is the motivation ",
        "for a Hawkes-process model of arrivals."
      )
    } else {
      ""
    }
  )

  list(
    class = class_name,
    n_events = length(ts_ns),
    n_gaps = length(gaps_s),
    mean_gap_s = mean(gaps_s),
    rate_hat_per_s = rate_hat,
    ks_statistic = unname(ks$statistic),
    ks_p_value = ks$p.value,
    exponential_rejected = rejected,
    verdict = verdict
  )
}

calibrate_interarrival <- function(dt, classes = c("ADD", "DELETE", "REPLACE", "CANCEL_PARTIAL"),
                                    alpha = 0.05) {
  event <- classify_event(dt$msg)
  out <- lapply(classes, function(cls) {
    fit_exponential_interarrival(dt$ts_ns[!is.na(event) & event == cls], cls, alpha = alpha)
  })
  names(out) <- classes
  out
}

# ---------------------------------------------------------------------------
# Order size distribution per event class
# ---------------------------------------------------------------------------

calibrate_order_size <- function(dt, classes = names(REFERENCE_EVENT_MIX)) {
  event <- classify_event(dt$msg)
  out <- lapply(classes, function(cls) {
    sizes <- dt$shares[!is.na(event) & event == cls & !is.na(dt$shares)]
    if (length(sizes) == 0) {
      note <- if (cls == "DELETE") {
        paste0(
          "ITCH's DELETE message carries no shares field (the order's full ",
          "remaining size is implied, not stated); size is not observable ",
          "for this class directly."
        )
      } else {
        "No non-missing shares values observed for this class."
      }
      return(list(class = cls, n = 0, note = note))
    }
    list(
      class = cls,
      n = length(sizes),
      mean = mean(sizes),
      sd = sd(sizes),
      quantiles = quantile(sizes, probs = c(0.05, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
    )
  })
  names(out) <- classes
  out
}

# ---------------------------------------------------------------------------
# Single-pass order-book reconstruction.
#
# Produces two tables:
#   $add_offsets  -- one row per ADD/ADD_ORDER_MPID event that had a two-
#                    sided book at the time: price offset from mid, in
#                    ticks (signed so that *larger* always means *further
#                    from the touch*, for both sides).
#   $lifecycle    -- one row per order that ever rested in the book
#                    (from a literal ADD, or newly created by a REPLACE),
#                    with its resting duration, terminal reason, and the
#                    queue (shares) already resting ahead of it at its own
#                    price level when it joined.
#
# Terminal reasons: DELETE (withdrawn, including a CANCEL_PARTIAL that
# exhausts the remainder), EXECUTED_FULL (filled away), REPLACED (reissued
# under a new order_ref), CENSORED_EOD (still resting when the file ends).
# ---------------------------------------------------------------------------

reconstruct_book <- function(dt, tick_size = 0.01, grid_interval_s = 1) {
  n <- nrow(dt)
  ts_ns <- dt$ts_ns
  msg <- dt$msg
  order_ref <- as.character(dt$order_ref)
  new_order_ref <- as.character(dt$new_order_ref)
  side_col <- dt$side
  shares <- dt$shares
  price <- dt$price
  match_number <- dt$match_number

  orders <- new.env(parent = emptyenv()) # order_ref -> list(side, price, remaining, lc_index)
  bid_levels <- new.env(parent = emptyenv()) # price*100 (key) -> resting size
  ask_levels <- new.env(parent = emptyenv())

  best_bid <- NA_real_
  best_ask <- NA_real_

  price_key <- function(p) sprintf("%d", as.integer(round(p * 100)))

  level_get <- function(side, p) {
    env <- if (side == "B") bid_levels else ask_levels
    key <- price_key(p)
    if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else 0
  }

  # Sum of resting size across every price on one side, not just the touch.
  # This must be the same depth measure book_depth() (R/price_model.R)
  # reports during simulation, since theta is calibrated against whichever
  # measure is captured here and then multiplied against book_depth()'s
  # measure at generation time -- see the theta fix in event_generator.R.
  level_total <- function(side) {
    env <- if (side == "B") bid_levels else ask_levels
    keys <- ls(env, all.names = TRUE)
    if (length(keys) == 0) {
      return(0)
    }
    sum(vapply(keys, function(k) get(k, envir = env, inherits = FALSE), numeric(1)))
  }

  level_adjust <- function(side, p, delta) {
    env <- if (side == "B") bid_levels else ask_levels
    key <- price_key(p)
    new_val <- level_get(side, p) + delta
    if (new_val <= 1e-9) {
      if (exists(key, envir = env, inherits = FALSE)) rm(list = key, envir = env)
    } else {
      assign(key, new_val, envir = env)
    }
    new_val
  }

  rescan_best <- function(side) {
    env <- if (side == "B") bid_levels else ask_levels
    keys <- ls(env, all.names = TRUE)
    if (length(keys) == 0) {
      return(NA_real_)
    }
    prices <- as.numeric(keys) / 100
    if (side == "B") max(prices) else min(prices)
  }

  n_lc_max <- n
  lc_order_ref <- character(n_lc_max)
  lc_side <- character(n_lc_max)
  lc_price <- numeric(n_lc_max)
  lc_shares_initial <- numeric(n_lc_max)
  lc_add_ts <- bit64::integer64(n_lc_max)
  lc_end_ts <- rep(bit64::NA_integer64_, n_lc_max)
  lc_end_reason <- rep("CENSORED_EOD", n_lc_max)
  lc_queue_ahead <- numeric(n_lc_max)
  lc_origin <- character(n_lc_max)
  n_lc <- 0L

  n_add_max <- n
  add_order_ref <- character(n_add_max)
  add_side <- character(n_add_max)
  add_price <- numeric(n_add_max)
  add_shares <- numeric(n_add_max)
  add_ts <- bit64::integer64(n_add_max)
  add_mid <- numeric(n_add_max)
  add_offset_ticks <- numeric(n_add_max)
  add_queue_ahead <- numeric(n_add_max)
  add_best_bid <- numeric(n_add_max)
  add_best_ask <- numeric(n_add_max)
  n_add <- 0L

  # One row per EXECUTED/EXECUTED_WITH_PRICE message, with the book state
  # (best bid/ask and top-of-book size on each side) as it stood
  # *immediately before* this fill was applied -- i.e. the state the
  # incoming aggressor actually faced. Used by R/aggressors.R.
  n_exec_max <- n
  exec_ts_ns <- bit64::integer64(n_exec_max)
  exec_resting_side <- character(n_exec_max)
  exec_price <- numeric(n_exec_max)
  exec_shares <- numeric(n_exec_max)
  exec_match_number <- bit64::integer64(n_exec_max)
  exec_best_bid <- numeric(n_exec_max)
  exec_best_ask <- numeric(n_exec_max)
  exec_bid_top_size <- numeric(n_exec_max)
  exec_ask_top_size <- numeric(n_exec_max)
  n_exec <- 0L

  # A regular time grid of book state (best bid/ask, top-of-book size),
  # sampled every grid_interval_s, for binning arrival counts against
  # prevailing spread/imbalance in R/aggressors.R's intensity fit.
  grid_interval_ns <- bit64::as.integer64(round(grid_interval_s * 1e9))
  session_span_ns <- as.numeric(ts_ns[n] - ts_ns[1])
  n_grid_max <- as.integer(ceiling(session_span_ns / 1e9 / grid_interval_s)) + 2L
  grid_ts_ns <- bit64::integer64(n_grid_max)
  grid_best_bid <- numeric(n_grid_max)
  grid_best_ask <- numeric(n_grid_max)
  grid_bid_top_size <- numeric(n_grid_max)
  grid_ask_top_size <- numeric(n_grid_max)
  grid_bid_total_size <- numeric(n_grid_max)
  grid_ask_total_size <- numeric(n_grid_max)
  n_grid <- 0L
  next_grid_ts <- ts_ns[1]

  for (i in seq_len(n)) {
    while (ts_ns[i] >= next_grid_ts) {
      n_grid <- n_grid + 1L
      grid_ts_ns[n_grid] <- next_grid_ts
      grid_best_bid[n_grid] <- best_bid
      grid_best_ask[n_grid] <- best_ask
      grid_bid_top_size[n_grid] <- if (is.na(best_bid)) NA_real_ else level_get("B", best_bid)
      grid_ask_top_size[n_grid] <- if (is.na(best_ask)) NA_real_ else level_get("S", best_ask)
      grid_bid_total_size[n_grid] <- level_total("B")
      grid_ask_total_size[n_grid] <- level_total("S")
      next_grid_ts <- next_grid_ts + grid_interval_ns
    }

    m <- msg[i]

    if (m == "A" || m == "F") {
      oref <- order_ref[i]
      sd <- side_col[i]
      px <- price[i]
      sz <- shares[i]
      qa <- level_get(sd, px)
      level_adjust(sd, px, sz)
      if (sd == "B") {
        if (is.na(best_bid) || px > best_bid) best_bid <- px
      } else {
        if (is.na(best_ask) || px < best_ask) best_ask <- px
      }

      n_lc <- n_lc + 1L
      lc_order_ref[n_lc] <- oref
      lc_side[n_lc] <- sd
      lc_price[n_lc] <- px
      lc_shares_initial[n_lc] <- sz
      lc_add_ts[n_lc] <- ts_ns[i]
      lc_queue_ahead[n_lc] <- qa
      lc_origin[n_lc] <- "ADD"
      assign(oref, list(side = sd, price = px, remaining = sz, lc_index = n_lc), envir = orders)

      if (!is.na(best_bid) && !is.na(best_ask)) {
        mid <- (best_bid + best_ask) / 2
        offset <- if (sd == "B") (mid - px) else (px - mid)
        n_add <- n_add + 1L
        add_order_ref[n_add] <- oref
        add_side[n_add] <- sd
        add_price[n_add] <- px
        add_shares[n_add] <- sz
        add_ts[n_add] <- ts_ns[i]
        add_mid[n_add] <- mid
        add_offset_ticks[n_add] <- offset / tick_size
        add_queue_ahead[n_add] <- qa
        add_best_bid[n_add] <- best_bid
        add_best_ask[n_add] <- best_ask
      }
    } else if (m == "D" || m == "X" || m == "E" || m == "C") {
      oref <- order_ref[i]
      if (exists(oref, envir = orders, inherits = FALSE)) {
        o <- get(oref, envir = orders, inherits = FALSE)
        remove_amount <- if (m == "D") o$remaining else shares[i]
        reason <- if (m == "D" || m == "X") "DELETE" else "EXECUTED_FULL"

        if (m == "E" || m == "C") {
          n_exec <- n_exec + 1L
          exec_ts_ns[n_exec] <- ts_ns[i]
          exec_resting_side[n_exec] <- o$side
          # 'C' (EXECUTED_WITH_PRICE) states the actual trade price, which
          # can differ from the resting order's displayed price (e.g. a
          # hidden/midpoint order); plain 'E' has no price field, so the
          # trade happened at the order's own displayed price.
          exec_price[n_exec] <- if (m == "C") price[i] else o$price
          exec_shares[n_exec] <- shares[i]
          exec_match_number[n_exec] <- match_number[i]
          exec_best_bid[n_exec] <- best_bid
          exec_best_ask[n_exec] <- best_ask
          exec_bid_top_size[n_exec] <- if (is.na(best_bid)) NA_real_ else level_get("B", best_bid)
          exec_ask_top_size[n_exec] <- if (is.na(best_ask)) NA_real_ else level_get("S", best_ask)
        }

        level_adjust(o$side, o$price, -remove_amount)
        new_remaining <- o$remaining - remove_amount

        if (new_remaining <= 1e-9) {
          lc_end_ts[o$lc_index] <- ts_ns[i]
          lc_end_reason[o$lc_index] <- reason
          rm(list = oref, envir = orders)
          touched_best <- if (o$side == "B") {
            !is.na(best_bid) && o$price == best_bid
          } else {
            !is.na(best_ask) && o$price == best_ask
          }
          if (touched_best && level_get(o$side, o$price) <= 0) {
            if (o$side == "B") best_bid <- rescan_best("B") else best_ask <- rescan_best("S")
          }
        } else {
          o$remaining <- new_remaining
          assign(oref, o, envir = orders)
        }
      }
    } else if (m == "U") {
      oref <- order_ref[i]
      if (exists(oref, envir = orders, inherits = FALSE)) {
        o <- get(oref, envir = orders, inherits = FALSE)
        sd <- o$side

        level_adjust(sd, o$price, -o$remaining)
        lc_end_ts[o$lc_index] <- ts_ns[i]
        lc_end_reason[o$lc_index] <- "REPLACED"
        rm(list = oref, envir = orders)
        touched_best <- if (sd == "B") {
          !is.na(best_bid) && o$price == best_bid
        } else {
          !is.na(best_ask) && o$price == best_ask
        }
        if (touched_best && level_get(sd, o$price) <= 0) {
          if (sd == "B") best_bid <- rescan_best("B") else best_ask <- rescan_best("S")
        }

        new_oref <- new_order_ref[i]
        new_px <- price[i]
        new_sz <- shares[i]
        qa <- level_get(sd, new_px)
        level_adjust(sd, new_px, new_sz)
        if (sd == "B") {
          if (is.na(best_bid) || new_px > best_bid) best_bid <- new_px
        } else {
          if (is.na(best_ask) || new_px < best_ask) best_ask <- new_px
        }

        n_lc <- n_lc + 1L
        lc_order_ref[n_lc] <- new_oref
        lc_side[n_lc] <- sd
        lc_price[n_lc] <- new_px
        lc_shares_initial[n_lc] <- new_sz
        lc_add_ts[n_lc] <- ts_ns[i]
        lc_queue_ahead[n_lc] <- qa
        lc_origin[n_lc] <- "REPLACE"
        assign(new_oref, list(side = sd, price = new_px, remaining = new_sz, lc_index = n_lc), envir = orders)
      }
    }
    # SYSTEM_EVENT, TRADING_ACTION, TRADE_NON_CROSS do not touch the book.
  }

  idx_lc <- seq_len(n_lc)
  idx_add <- seq_len(n_add)
  idx_exec <- seq_len(n_exec)
  idx_grid <- seq_len(n_grid)

  list(
    lifecycle = data.table::data.table(
      order_ref = lc_order_ref[idx_lc], side = lc_side[idx_lc], price = lc_price[idx_lc],
      shares_initial = lc_shares_initial[idx_lc], add_ts_ns = lc_add_ts[idx_lc],
      end_ts_ns = lc_end_ts[idx_lc], end_reason = lc_end_reason[idx_lc],
      queue_ahead_shares = lc_queue_ahead[idx_lc], origin = lc_origin[idx_lc]
    ),
    add_offsets = data.table::data.table(
      order_ref = add_order_ref[idx_add], side = add_side[idx_add], price = add_price[idx_add],
      shares = add_shares[idx_add], ts_ns = add_ts[idx_add], mid = add_mid[idx_add],
      offset_ticks = add_offset_ticks[idx_add], queue_ahead_shares = add_queue_ahead[idx_add],
      best_bid = add_best_bid[idx_add], best_ask = add_best_ask[idx_add]
    ),
    executed_events = data.table::data.table(
      ts_ns = exec_ts_ns[idx_exec], resting_side = exec_resting_side[idx_exec],
      price = exec_price[idx_exec], shares = exec_shares[idx_exec],
      match_number = exec_match_number[idx_exec],
      best_bid = exec_best_bid[idx_exec], best_ask = exec_best_ask[idx_exec],
      bid_top_size = exec_bid_top_size[idx_exec], ask_top_size = exec_ask_top_size[idx_exec]
    ),
    state_grid = data.table::data.table(
      ts_ns = grid_ts_ns[idx_grid], best_bid = grid_best_bid[idx_grid],
      best_ask = grid_best_ask[idx_grid], bid_top_size = grid_bid_top_size[idx_grid],
      ask_top_size = grid_ask_top_size[idx_grid],
      bid_total_size = grid_bid_total_size[idx_grid], ask_total_size = grid_ask_total_size[idx_grid]
    ),
    n_add_no_two_sided_book = sum(msg %in% c("A", "F")) - n_add
  )
}

# ---------------------------------------------------------------------------
# ADD price offset from mid, in ticks
# ---------------------------------------------------------------------------

calibrate_add_offset <- function(add_offsets) {
  x <- add_offsets$offset_ticks
  if (length(x) == 0) {
    return(list(n = 0, note = "No ADD events had a two-sided book to measure offset against."))
  }
  list(
    n = length(x),
    mean_ticks = mean(x),
    sd_ticks = sd(x),
    quantiles_ticks = quantile(x, probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)),
    pct_at_or_through_mid = mean(x <= 0) * 100,
    note = paste0(
      "Offset is signed by side so larger always means farther from the ",
      "touch: (mid - price) for bids, (price - mid) for asks. Because mid ",
      "sits strictly between the best bid and ask, an order joining ",
      "*exactly at the touch* still has offset = half the spread (> 0), ",
      "not 0 -- only an order priced at-or-through the mid itself (rare, ",
      "aggressive) gets offset <= 0. pct_at_or_through_mid reports that ",
      "rarer condition, not 'joined at the best price'."
    )
  )
}

# ---------------------------------------------------------------------------
# Cancel hazard: as a function of time resting, and of queue position
# ---------------------------------------------------------------------------

calibrate_cancel_hazard <- function(lifecycle, dt) {
  lc <- data.table::copy(lifecycle)
  session_end_ns <- max(dt$ts_ns)

  censored <- is.na(lc$end_ts_ns)
  lc[censored, end_ts_ns := session_end_ns]
  lc[, duration_s := as.numeric(end_ts_ns - add_ts_ns) / 1e9]
  lc <- lc[duration_s > 0]
  # status = 1 only for a true withdrawal (DELETE); EXECUTED_FULL, REPLACED,
  # and still-resting-at-EOD are treated as censoring for *this*
  # cause-specific (cancel) hazard, per standard competing-risks practice.
  lc[, status := as.integer(end_reason == "DELETE")]

  if (nrow(lc) < 20 || sum(lc$status) < 10) {
    return(list(note = "Too few orders/cancellations to fit a hazard model."))
  }

  # Hazard as a function of time resting: piecewise-constant life-table
  # hazard (events / person-time at risk) over deciles of observed duration.
  breaks <- unique(stats::quantile(lc$duration_s, probs = seq(0, 1, 0.1)))
  by_bin <- lc[, .(
    n_at_risk = .N,
    n_cancelled = sum(status),
    person_time_s = sum(duration_s)
  ), by = .(bin = cut(duration_s, breaks = breaks, include.lowest = TRUE))]
  by_bin[, hazard_per_s := n_cancelled / person_time_s]
  data.table::setorder(by_bin, bin)

  # Hazard as a function of queue position: Cox proportional-hazards
  # regression on log1p(shares resting ahead at the order's own price
  # level when it joined the book).
  lc[, log_queue_ahead := log1p(queue_ahead_shares)]
  cox_fit <- tryCatch(
    survival::coxph(survival::Surv(duration_s, status) ~ log_queue_ahead, data = lc),
    error = function(e) NULL
  )

  queue_effect <- if (is.null(cox_fit)) {
    list(note = "Cox model on queue position failed to fit.")
  } else {
    s <- summary(cox_fit)
    coef <- unname(s$coefficients["log_queue_ahead", "coef"])
    hr <- unname(s$coefficients["log_queue_ahead", "exp(coef)"])
    p_value <- unname(s$coefficients["log_queue_ahead", "Pr(>|z|)"])
    list(
      coef = coef,
      hazard_ratio_per_log_unit = hr,
      p_value = p_value,
      note = paste0(
        "Hazard ratio ", signif(hr, 3), " per unit increase in log1p(shares ahead): ",
        if (hr > 1 && p_value < 0.05) {
          "more queue ahead is associated with a HIGHER cancel hazard (impatience with a long queue)."
        } else if (hr < 1 && p_value < 0.05) {
          "more queue ahead is associated with a LOWER cancel hazard (orders that join deep queues are held longer, e.g. resting/passive strategies)."
        } else {
          "no significant association at alpha=0.05."
        }
      )
    )
  }

  list(
    n_orders = nrow(lc),
    n_cancelled = sum(lc$status),
    n_censored = sum(1 - lc$status),
    hazard_by_duration = by_bin,
    queue_position_effect = queue_effect
  )
}

# ---------------------------------------------------------------------------
# Top-level entry point
# ---------------------------------------------------------------------------

calibrate_distributions <- function(dt, tick_size = 0.01, alpha = 0.05, verbose = TRUE, book = NULL) {
  event_mix <- calibrate_event_mix(dt)
  interarrival <- calibrate_interarrival(dt, alpha = alpha)
  order_size <- calibrate_order_size(dt)

  if (is.null(book)) book <- reconstruct_book(dt, tick_size = tick_size)
  add_offset <- calibrate_add_offset(book$add_offsets)
  cancel_hazard <- calibrate_cancel_hazard(book$lifecycle, dt)

  result <- list(
    event_mix = event_mix,
    interarrival = interarrival,
    add_offset_ticks = add_offset,
    add_offsets_raw = book$add_offsets,
    order_size = order_size,
    cancel_hazard = cancel_hazard,
    lifecycle = book$lifecycle
  )

  if (verbose) {
    cat(event_mix$note, "\n\n")
    for (cls in names(interarrival)) {
      v <- interarrival[[cls]]$verdict
      if (!is.null(v)) {
        cat(v, "\n")
      } else {
        cat(cls, ": ", interarrival[[cls]]$note,
          " (n=", interarrival[[cls]]$n_events, ")\n",
          sep = ""
        )
      }
    }
    cat("\n")
    if (!is.null(cancel_hazard$queue_position_effect$note)) {
      cat("Cancel hazard vs. queue position: ", cancel_hazard$queue_position_effect$note, "\n")
    }
  }

  result
}

# ---------------------------------------------------------------------------
# Exponential-kernel Hawkes process: MLE fit against real event arrivals
#
#   lambda(t) = mu + sum_{t_i < t} alpha * exp(-beta * (t - t_i))
#
# Fit by maximum likelihood using Ozaki's (1979) O(n) recursive
# log-likelihood for the exponential kernel -- no numerical integration is
# needed, so this stays fast even at tens of thousands of events.
#
# Parameterized as (mu, branching_ratio = alpha/beta, beta) via a log/logit
# transform, rather than (mu, alpha, beta) directly, so optim() searches an
# unconstrained space while branching_ratio in (0, 1) is enforced
# automatically. branching_ratio >= 1 is a non-stationary (explosive)
# process -- never a legitimate fit for a real, finite market session -- and
# is exactly the region an unconstrained (mu, alpha, beta) fit can wander
# into and silently return as though it meant something.
# ---------------------------------------------------------------------------

hawkes_loglik <- function(par, event_times_s, T_obs_s) {
  mu <- exp(par[1])
  branching_ratio <- stats::plogis(par[2])
  beta <- exp(par[3])
  alpha <- branching_ratio * beta

  n <- length(event_times_s)
  if (n == 0) {
    return(-mu * T_obs_s)
  }

  r <- numeric(n) # r[i] = sum_{t_j < t_i} exp(-beta * (t_i - t_j)), built recursively
  if (n > 1) {
    dt <- diff(event_times_s)
    for (i in 2:n) {
      r[i] <- exp(-beta * dt[i - 1]) * (1 + r[i - 1])
    }
  }

  compensator <- (alpha / beta) * sum(1 - exp(-beta * (T_obs_s - event_times_s)))
  sum(log(mu + alpha * r)) - mu * T_obs_s - compensator
}

#' Fits an exponential-kernel Hawkes process to real event arrival times by
#' maximum likelihood, in place of assuming a branching ratio.
#'
#' @param event_times_s Arrival times in seconds from the start of the
#'   observation window; sorted ascending internally if not already.
#' @param T_obs_s Total observation window length, in seconds; must be >=
#'   the last event time.
#' @return A list with mu (baseline rate/s), alpha, beta (decay rate/s),
#'   branching_ratio (alpha/beta), convergence diagnostics, and a note
#'   describing the fit or, with too few events, explaining the fallback.
fit_hawkes_exponential <- function(event_times_s, T_obs_s, decay_init_per_s = 1) {
  event_times_s <- sort(event_times_s)
  n <- length(event_times_s)

  if (n < 20) {
    return(list(
      mu = NA_real_, alpha = NA_real_, beta = NA_real_, branching_ratio = NA_real_,
      converged = FALSE,
      note = sprintf("Only %d events -- too few to fit a Hawkes process; caller should fall back to an assumed branching ratio.", n)
    ))
  }

  # Baseline guess at half the raw average rate; excitation is assumed to
  # explain roughly the other half. Only a starting point for optim(), not
  # a claim about the true split.
  mu_init <- (n / T_obs_s) * 0.5
  par0 <- c(log(mu_init), stats::qlogis(0.3), log(decay_init_per_s))

  fit <- stats::optim(
    par0, hawkes_loglik, event_times_s = event_times_s, T_obs_s = T_obs_s,
    method = "Nelder-Mead", control = list(fnscale = -1, maxit = 2000)
  )

  mu <- exp(fit$par[1])
  branching_ratio <- stats::plogis(fit$par[2])
  beta <- exp(fit$par[3])
  alpha <- branching_ratio * beta

  list(
    mu = mu, alpha = alpha, beta = beta, branching_ratio = branching_ratio,
    loglik = fit$value, converged = fit$convergence == 0,
    note = sprintf(
      "Fit by MLE on %d real event arrivals over %.1fs: branching_ratio=%.3f, decay=%.3f/s (mean excitation lifetime %.2fs).%s",
      n, T_obs_s, branching_ratio, beta, 1 / beta,
      if (fit$convergence != 0) " optim() did not report convergence -- treat this fit with suspicion." else ""
    )
  )
}
