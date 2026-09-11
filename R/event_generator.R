# R/event_generator.R
#
# Simulates order-flow arrivals and drives them through R/price_model.R's
# live book, emitting events in R/types.R's interchange schema. Two arrival
# models share one interface (simulate_market(..., model = "poisson" |
# "hawkes")) so they can be A/B compared:
#
#   poisson (Cont-Stoikov-Talreja baseline): limit orders arrive as
#     independent homogeneous Poisson processes at each distance from the
#     OPPOSITE best quote; market orders arrive as an independent
#     homogeneous Poisson process; cancellations arrive at a rate
#     proportional to currently resting depth (this depth-dependence is
#     intrinsic to the CSTT baseline itself, not a Hawkes addition -- the
#     "independent" part is that none of these rates depend on *history*,
#     only on the current state).
#
#   hawkes: identical limit-order arrivals, but market-order and
#     cancellation intensity gain a mutually-exciting term: each trade adds
#     a short-lived, exponentially-decaying boost to the intensity of
#     future trades (self-excitation) and cancellations (cross-excitation).
#
# The mid-price is never generated directly by either model -- it is
# whatever R/price_model.R's book_best() derives after each applied event.
#
# Both models are driven by the same event loop (a standard
# Gillespie/stochastic-simulation-algorithm step: draw the waiting time to
# the next event from the CURRENT total rate across all channels, pick which
# channel fired proportional to its share of that total, apply it, repeat).
# This is also the standard way to simulate an exponential-kernel Hawkes
# process (Ogata thinning collapses to exact sampling here because the
# intensity is piecewise-constant between events and known in closed form).

library(data.table)
library(bit64)

# ---------------------------------------------------------------------------
# Calibration -> simulation parameters
#
# Pulls from the bundle built by calibrate_distributions() (R/distributions.R)
# and recover_aggressors()/fit_aggressor_intensity() (R/aggressors.R). See
# scripts/build_calibration_cache.R for how that bundle is assembled --
# reconstructing the real order book is the expensive step (~5 min for one
# symbol-day), so it is cached rather than repeated per simulation run.
# ---------------------------------------------------------------------------

build_arrival_params <- function(calibration, tick_size = 0.01, n_levels = 15) {
  dist <- calibration$dist
  agg_events <- calibration$aggressors$events
  session_duration_s <- as.numeric(calibration$session_end_ns - calibration$session_start_ns) / 1e9

  ao <- dist$add_offsets_raw
  # Distance from the OPPOSITE best quote, in ticks -- CSTT's own reference
  # point. NOT the same as offset_ticks (measured from mid); recovering it
  # needs the prevailing best_bid/best_ask at each ADD, which distributions.R
  # captures alongside offset_ticks for exactly this purpose.
  distance_ticks <- ifelse(
    ao$side == "B",
    (ao$best_ask - ao$price) / tick_size,
    (ao$price - ao$best_bid) / tick_size
  )
  distance_ticks <- pmax(1, pmin(n_levels, round(distance_ticks)))

  total_add_rate <- nrow(ao) / session_duration_s
  level_shape <- as.numeric(table(factor(distance_ticks, levels = seq_len(n_levels))))
  level_shape <- level_shape / sum(level_shape)
  lambda_i <- total_add_rate * level_shape

  mu_buy <- sum(agg_events$aggressor_side == "BUY") / session_duration_s
  mu_sell <- sum(agg_events$aggressor_side == "SELL") / session_duration_s

  # theta: cancel rate per unit of resting depth (CSTT's depth-proportional
  # cancellation). Approximated from the full-withdrawal (DELETE) rate over
  # average TOP-OF-BOOK depth, since this pipeline's state_grid tracks only
  # the touch, not the full book -- a real theta would use total book depth.
  delete_rate <- dist$interarrival$DELETE$rate_hat_per_s
  avg_top_depth <- mean(calibration$state_grid$bid_top_size + calibration$state_grid$ask_top_size, na.rm = TRUE)
  theta <- delete_rate / avg_top_depth

  list(
    n_levels = n_levels, tick_size = tick_size, session_duration_s = session_duration_s,
    lambda_i = lambda_i, mu_buy = mu_buy, mu_sell = mu_sell, theta = theta,
    size_pool_limit = ao$shares, size_pool_market = agg_events$quantity
  )
}

# ---------------------------------------------------------------------------
# Shared simulation driver
# ---------------------------------------------------------------------------

# start_ts_ns defaults to 1, not 0: Leka's Timestamp class reserves 0 as an
# invalid sentinel, so a bootstrap order stamped at ts_ns=0 would be rejected
# by MatchingEngine::processEvent when this output is replayed there.
simulate_market <- function(calibration, duration_s, model = c("poisson", "hawkes"),
                             tick_size = 0.01, n_levels = 15, initial_mid = 100,
                             bootstrap_levels = 10, bootstrap_depth = 300,
                             start_ts_ns = bit64::as.integer64(1), seed = NULL,
                             hawkes_branching_ratio = 0.3, hawkes_decay_per_s = 1,
                             hawkes_cancel_excite_frac = 0.5) {
  model <- match.arg(model)
  if (!is.null(seed)) set.seed(seed)

  params <- build_arrival_params(calibration, tick_size = tick_size, n_levels = n_levels)

  bb <- new_book(
    tick_size = tick_size, initial_mid = initial_mid, bootstrap_levels = bootstrap_levels,
    bootstrap_depth = bootstrap_depth, start_ts_ns = start_ts_ns
  )
  book <- bb$book

  all_rows <- list(bb$bootstrap_events)
  price_path <- list(data.table::data.table(ts_ns = start_ts_ns, mid = book_best(book)$mid))

  end_ts_ns <- start_ts_ns + bit64::as.integer64(round(duration_s * 1e9))
  t_ns <- start_ts_ns
  t_s <- 0

  # Hawkes self/cross-excitation state (decayed and read, but never
  # populated, when model == "poisson" -- harmless no-op in that case).
  excite_market <- 0
  excite_cancel <- 0
  mu_market_total <- params$mu_buy + params$mu_sell
  frac_buy <- if (mu_market_total > 0) params$mu_buy / mu_market_total else 0.5
  mu_market_baseline <- if (model == "hawkes") mu_market_total * (1 - hawkes_branching_ratio) else mu_market_total
  alpha_mm <- hawkes_branching_ratio * hawkes_decay_per_s
  alpha_mc <- hawkes_cancel_excite_frac * alpha_mm

  limit_names <- c(paste0("limit_B_", seq_len(n_levels)), paste0("limit_S_", seq_len(n_levels)))

  n_events_emitted <- 0L

  channel_rates <- function(depth, ex_market, ex_cancel) {
    rate_market_total <- mu_market_baseline + ex_market
    r <- c(
      stats::setNames(params$lambda_i, paste0("limit_B_", seq_len(n_levels))),
      stats::setNames(params$lambda_i, paste0("limit_S_", seq_len(n_levels))),
      market_B = rate_market_total * frac_buy,
      market_S = rate_market_total * (1 - frac_buy),
      cancel_B = params$theta * depth$bid + ex_cancel * (depth$bid / max(1, depth$bid + depth$ask)),
      cancel_S = params$theta * depth$ask + ex_cancel * (depth$ask / max(1, depth$bid + depth$ask))
    )
    r[r < 0 | is.na(r)] <- 0
    r
  }

  repeat {
    # Book state (and hence every channel's rate) is constant until the
    # next event actually fires, so it is safe to hold `depth` fixed for
    # the whole thinning sub-loop below.
    depth <- book_depth(book)

    # For the Poisson baseline this inner loop always accepts on the first
    # draw (rates are genuinely constant, so the proposal *is* exact). For
    # Hawkes, rates_now (at the current excitation level) is a valid upper
    # bound going forward -- excitation only decays between jumps -- so this
    # is standard Ogata thinning: propose from the upper bound, accept with
    # probability true_rate/proposal_rate, else advance and re-propose.
    rates <- NULL
    repeat {
      rates_now <- channel_rates(depth, excite_market, excite_cancel)
      total_now <- sum(rates_now)
      if (total_now <= 1e-12) {
        t_s <- Inf
        break
      }

      gap_s <- stats::rexp(1, rate = total_now)
      cand_t_s <- t_s + gap_s
      cand_ts_ns <- start_ts_ns + bit64::as.integer64(round(cand_t_s * 1e9))
      if (cand_ts_ns > end_ts_ns) {
        t_s <- Inf
        break
      }

      if (model == "poisson") {
        t_s <- cand_t_s
        rates <- rates_now
        break
      }

      decay_step <- exp(-hawkes_decay_per_s * gap_s)
      em_cand <- excite_market * decay_step
      ec_cand <- excite_cancel * decay_step
      rates_true <- channel_rates(depth, em_cand, ec_cand)
      total_true <- sum(rates_true)

      t_s <- cand_t_s
      excite_market <- em_cand
      excite_cancel <- ec_cand
      if (stats::runif(1) <= total_true / total_now) {
        rates <- rates_true
        break
      }
      # else: rejected -- loop again, thinning forward from the new t_s
    }

    if (!is.finite(t_s)) break
    t_ns <- start_ts_ns + bit64::as.integer64(round(t_s * 1e9))

    chosen <- sample(names(rates), 1, prob = rates)
    new_rows <- NULL

    if (chosen %in% limit_names) {
      i <- as.integer(sub("^limit_[BS]_", "", chosen))
      side <- if (startsWith(chosen, "limit_B_")) "B" else "S"
      # Priced at distance i from the OPPOSITE best (CSTT's reference
      # point). If that side has been fully exhausted, anchor off this
      # order's own side instead of a stale initial_mid -- the book may
      # have drifted far from its starting price by now.
      price <- if (side == "B") {
        if (!is.na(book$best_ask)) {
          book$best_ask - i * tick_size
        } else if (!is.na(book$best_bid)) {
          book$best_bid + i * tick_size
        } else {
          initial_mid - i * tick_size
        }
      } else {
        if (!is.na(book$best_bid)) {
          book$best_bid + i * tick_size
        } else if (!is.na(book$best_ask)) {
          book$best_ask - i * tick_size
        } else {
          initial_mid + i * tick_size
        }
      }
      qty <- sample(params$size_pool_limit, 1)
      new_rows <- book_apply_new_limit(book, side, price, qty, t_ns)
    } else if (chosen == "market_B" || chosen == "market_S") {
      side <- substr(chosen, nchar(chosen), nchar(chosen))
      qty <- sample(params$size_pool_market, 1)
      new_rows <- book_apply_market(book, side, qty, t_ns)
      if (model == "hawkes") {
        excite_market <- excite_market + alpha_mm
        excite_cancel <- excite_cancel + alpha_mc
      }
    } else if (chosen == "cancel_B" || chosen == "cancel_S") {
      side <- substr(chosen, nchar(chosen), nchar(chosen))
      new_rows <- book_apply_cancel(book, side, t_ns)
    }

    if (!is.null(new_rows) && nrow(new_rows) > 0) {
      all_rows[[length(all_rows) + 1]] <- new_rows
      n_events_emitted <- n_events_emitted + nrow(new_rows)
    }
    price_path[[length(price_path) + 1]] <- data.table::data.table(ts_ns = t_ns, mid = book_best(book)$mid)
  }

  events <- data.table::rbindlist(all_rows)
  events[, seq := seq_len(.N)] # re-sequence: bootstrap + generated rows interleave via append order, already time-ordered

  list(
    events = events,
    price_path = data.table::rbindlist(price_path),
    model = model, params = params, book = book,
    n_events_emitted = n_events_emitted
  )
}

# ---------------------------------------------------------------------------
# Baseline-vs-calibration sanity checks ("get it working and validated").
# Not a statistical test suite -- a direct, readable comparison of what the
# simulator produced against the calibration it was built from.
# ---------------------------------------------------------------------------

validate_simulation <- function(sim, calibration) {
  ev <- sim$events
  duration_s <- as.numeric(max(ev$ts_ns) - min(ev$ts_ns)) / 1e9

  schema_ok <- tryCatch({
    validate_interchange(ev)
    TRUE
  }, error = function(e) {
    message("Schema validation FAILED: ", conditionMessage(e))
    FALSE
  })

  mix <- prop.table(table(ev$event))
  sim_total_rate <- nrow(ev) / duration_s
  cal_total_rate <- with(sim$params, sum(lambda_i) + mu_buy + mu_sell) # cancel rate is state-dependent, excluded from this static comparison

  no_crossed_book <- {
    bb <- book_best(sim$book)
    is.na(bb$spread) || bb$spread >= 0
  }

  list(
    schema_valid = schema_ok,
    n_events = nrow(ev),
    duration_s = duration_s,
    event_mix = mix,
    sim_total_rate_per_s = sim_total_rate,
    calibrated_arrival_rate_per_s = cal_total_rate,
    book_never_crossed = no_crossed_book,
    final_best = book_best(sim$book),
    note = paste0(
      "Schema valid: ", schema_ok,
      ". Book crossed: ", !no_crossed_book,
      ". Simulated total event rate ", signif(sim_total_rate, 4),
      "/s vs. calibrated limit+market arrival rate ", signif(cal_total_rate, 4),
      "/s (cancels excluded from this comparison since their rate is ",
      "depth-dependent, not a fixed calibrated constant)."
    )
  )
}
