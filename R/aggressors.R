# R/aggressors.R
#
# ITCH never publishes a market order as an order -- it only appears as the
# execution footprint it leaves on the resting orders it consumes. This file
# recovers those incoming aggressor orders from EXECUTED / EXECUTED_WITH_PRICE
# rows, and fits how their arrival intensity depends on prevailing spread and
# top-of-book queue imbalance.
#
# Depends on R/distributions.R for reconstruct_book(), which produces the two
# inputs this file needs: $executed_events (one row per fill, with book state
# captured *before* that fill was applied) and $state_grid (a regular time
# series of book state, for binning arrivals against prevailing conditions).
#
# Entry point: calibrate_aggressors(dt, tick_size = 0.01)

library(data.table)
library(bit64)

# ---------------------------------------------------------------------------
# Recover aggressor orders from executed_events
#
# A single incoming marketable order can sweep multiple resting orders in one
# atomic match; ITCH stamps every fill from that sweep with the identical
# ts_ns and (necessarily) the same resting side. match_number is USELESS for
# this: it is assigned per fill, one-to-one, so grouping by it recovers
# nothing -- verified below (n_executions == n_distinct_match_numbers).
#
# For each (ts_ns, resting_side) group:
#   aggressor_side = opposite of the resting side that got hit
#   quantity       = sum of shares filled in the group
#   price          = worst price the aggressor received: max for a buy
#                    (highest price paid), min for a sell (lowest price
#                    received) -- an observed lower/upper bound on wherever
#                    their own limit (if any) actually was.
# ---------------------------------------------------------------------------

recover_aggressors <- function(executed_events) {
  ee <- data.table::copy(executed_events)
  n_executions <- nrow(ee)
  n_distinct_match_numbers <- data.table::uniqueN(ee$match_number)

  if (n_executions == 0) {
    return(list(
      events = ee, n_executions = 0, n_distinct_match_numbers = 0,
      n_recovered = 0, pct_single_price_level = NA_real_,
      note = "No executions in this file."
    ))
  }

  grouped <- ee[, .(
    aggressor_side = if (resting_side[1] == "B") "SELL" else "BUY",
    quantity = sum(shares),
    price = if (resting_side[1] == "B") min(price) else max(price),
    n_price_levels = data.table::uniqueN(price),
    n_fills = .N,
    best_bid = best_bid[1],
    best_ask = best_ask[1],
    bid_top_size = bid_top_size[1],
    ask_top_size = ask_top_size[1]
  ), by = .(ts_ns, resting_side)]
  data.table::setorder(grouped, ts_ns)

  pct_single_level <- mean(grouped$n_price_levels == 1) * 100

  list(
    events = grouped,
    n_executions = n_executions,
    n_distinct_match_numbers = n_distinct_match_numbers,
    n_recovered = nrow(grouped),
    pct_single_price_level = pct_single_level,
    note = paste0(
      "match_number check: ", n_executions, " executions, ",
      n_distinct_match_numbers, " distinct match numbers -- ",
      if (n_executions == n_distinct_match_numbers) {
        "confirmed 1:1, so it carries no grouping information (grouped by ts_ns + resting side instead)."
      } else {
        "NOT 1:1 -- match_number may carry grouping information after all; re-examine before trusting the ts_ns+side grouping alone."
      }
    )
  )
}

# ---------------------------------------------------------------------------
# Fit aggressor arrival intensity conditional on spread and queue imbalance
#
# Bins the session into fixed windows (state_grid's own granularity) and
# regresses the count of recovered aggressor arrivals per bin on the
# spread and top-of-book imbalance prevailing at the *start* of that bin
# (a standard piecewise-constant-intensity Poisson-GLM construction: log
# E[count] = log(bin width) + b0 + b1*spread + b2*imbalance).
#
# Fit with quasipoisson rather than poisson: if arrivals cluster (which is
# exactly what a Hawkes-motivated view of this market predicts -- see
# R/distributions.R's interarrival rejection of the Exponential), the count
# variance will exceed the Poisson mean=variance assumption. quasipoisson
# keeps the same point estimates but inflates standard errors to reflect
# that, rather than reporting falsely confident p-values.
# ---------------------------------------------------------------------------

fit_aggressor_intensity <- function(aggressor_events, state_grid, tick_size = 0.01) {
  if (nrow(state_grid) < 3) {
    return(list(note = "state_grid has too few points to bin against."))
  }
  bin_s <- as.numeric(state_grid$ts_ns[2] - state_grid$ts_ns[1]) / 1e9

  grid <- data.table::copy(state_grid)
  grid[, spread_ticks := (best_ask - best_bid) / tick_size]
  grid[, imbalance := (bid_top_size - ask_top_size) / (bid_top_size + ask_top_size)]

  if (nrow(aggressor_events) < 20) {
    return(list(note = "Too few recovered aggressor events to fit an intensity model."))
  }

  bin_idx <- findInterval(as.numeric(aggressor_events$ts_ns), as.numeric(grid$ts_ns))
  bin_idx <- bin_idx[bin_idx >= 1 & bin_idx <= nrow(grid)]
  counts <- tabulate(bin_idx, nbins = nrow(grid))

  grid[, count := counts]
  grid <- grid[!is.na(spread_ticks) & !is.na(imbalance)]

  if (nrow(grid) < 30) {
    return(list(note = "Too few bins with a two-sided book to fit an intensity model."))
  }

  model <- stats::glm(
    count ~ spread_ticks + imbalance,
    family = stats::quasipoisson(link = "log"),
    offset = rep(log(bin_s), nrow(grid)),
    data = grid
  )
  s <- summary(model)
  co <- s$coefficients

  interpret <- function(term, label) {
    if (!(term %in% rownames(co))) {
      return(paste0(label, ": not estimated."))
    }
    est <- co[term, "Estimate"]
    p <- co[term, "Pr(>|t|)"]
    irr <- exp(est)
    direction <- if (p >= 0.05) {
      "no significant effect at alpha=0.05"
    } else if (est > 0) {
      paste0("HIGHER arrival intensity (x", signif(irr, 3), " per unit)")
    } else {
      paste0("LOWER arrival intensity (x", signif(irr, 3), " per unit)")
    }
    paste0(label, ": coef=", signif(est, 3), ", p=", signif(p, 3), " -> ", direction)
  }

  list(
    bin_s = bin_s,
    n_bins = nrow(grid),
    n_events_binned = sum(grid$count),
    model = model,
    dispersion = s$dispersion,
    coefficients = co,
    note = paste0(
      interpret("spread_ticks", "Spread"), "\n",
      interpret("imbalance", "Queue imbalance"), "\n",
      "Dispersion = ", signif(s$dispersion, 3),
      if (s$dispersion > 2) {
        paste0(
          " -- well above 1, i.e. arrivals are overdispersed relative to a ",
          "simple Poisson process (they cluster in time beyond what spread/",
          "imbalance alone explain). Consistent with the Exponential-",
          "interarrival rejection elsewhere in this pipeline: aggressive ",
          "flow is state-dependent AND self-exciting, not a clean ",
          "state-dependent Poisson process. Treating it as an independent, ",
          "memoryless stream -- even a state-dependent one -- will still ",
          "understate clustering/burstiness."
        )
      } else {
        " -- close to 1, no strong evidence of extra clustering beyond the fitted state-dependence."
      }
    )
  )
}

# ---------------------------------------------------------------------------
# Top-level entry point
# ---------------------------------------------------------------------------

calibrate_aggressors <- function(dt, tick_size = 0.01, grid_interval_s = 1, verbose = TRUE) {
  book <- reconstruct_book(dt, tick_size = tick_size, grid_interval_s = grid_interval_s)
  recovered <- recover_aggressors(book$executed_events)
  intensity <- if (recovered$n_recovered > 0) {
    fit_aggressor_intensity(recovered$events, book$state_grid, tick_size = tick_size)
  } else {
    list(note = "No aggressor events recovered; skipping intensity fit.")
  }

  if (verbose) {
    cat(recovered$note, "\n")
    cat(
      "Recovered ", recovered$n_recovered, " aggressor orders from ",
      recovered$n_executions, " executions (", signif(recovered$pct_single_price_level, 4),
      "% consume a single price level).\n",
      sep = ""
    )
    cat("\n", intensity$note, "\n", sep = "")
  }

  list(
    recovered = recovered,
    intensity = intensity,
    executed_events = book$executed_events,
    state_grid = book$state_grid
  )
}
