# scripts/analyze.R
#
# Replays a real ITCH day and a synthetic day through the SAME
# book-reconstruction code and compares seven distributional properties.
# "Same code" means both are first put into the interchange schema
# (R/types.R) -- real ITCH via translate_itch_to_interchange() below,
# synthetic via R/event_generator.R's simulate_market() -- and from there
# both are walked through the identical replay_interchange() engine in this
# file. Nothing downstream of that point knows or cares which source an
# event came from.
#
# Run from the repo root: Rscript scripts/analyze.R

suppressMessages({
  library(data.table)
  library(bit64)
  library(survival)
})
source("R/data_loader.R")
source("R/types.R")
source("R/distributions.R")
source("R/aggressors.R")
source("R/price_model.R")
source("R/event_generator.R")

# ---------------------------------------------------------------------------
# Real ITCH -> interchange schema
#
# Mirrors the Leka repo's tools/itch/itch_replay.cpp event mapping exactly
# (same file independently arrived at the same rules, which is a useful
# cross-check): ADD->NEW, DELETE->CANCEL, CANCEL_PARTIAL->REDUCE (or CANCEL
# if it exhausts the order), EXECUTED(_WITH_PRICE) partial->REDUCE / full->
# CANCEL, REPLACE->CANCEL(old)+NEW(new). Executions carry no order of their
# own in ITCH -- recover_aggressors() (R/aggressors.R) reconstructs the
# incoming aggressor as its own NEW MARKET row, timestamped identically to
# the CANCEL/REDUCE rows it causes, exactly as R/price_model.R's own
# book_apply_market() structures simulated output. That shared timestamp is
# also how replay_interchange() below tells a fill-driven exit from a
# voluntary cancel apart, despite the interchange schema itself not
# recording *why* an order left (Leka's OrderBook doesn't need to know).
# ---------------------------------------------------------------------------

translate_itch_to_interchange <- function(dt, aggressor_events, synthetic_id_base = 900000000000) {
  n <- nrow(dt)
  msg <- dt$msg
  ts_ns <- dt$ts_ns
  order_ref <- as.character(dt$order_ref)
  new_order_ref <- as.character(dt$new_order_ref)
  side_col <- dt$side
  shares <- dt$shares
  price <- dt$price

  remaining <- new.env(parent = emptyenv())
  side_of <- new.env(parent = emptyenv())
  price_of <- new.env(parent = emptyenv())

  n_max <- 2L * n
  out_ts <- bit64::integer64(n_max)
  out_event <- character(n_max)
  out_oid <- character(n_max)
  out_side <- character(n_max)
  out_type <- character(n_max)
  out_price <- numeric(n_max)
  out_qty <- numeric(n_max)
  k <- 0L

  emit <- function(ts, event, oid, side, type, px, qty) {
    k <<- k + 1L
    out_ts[k] <<- ts
    out_event[k] <<- event
    out_oid[k] <<- oid
    out_side[k] <<- if (side == "B") "BUY" else "SELL"
    out_type[k] <<- type
    out_price[k] <<- round(px * 10000) # dollars -> interchange schema's 1/10000-unit integer
    out_qty[k] <<- qty
  }

  for (i in seq_len(n)) {
    m <- msg[i]
    if (m == "A" || m == "F") {
      oref <- order_ref[i]
      sd <- side_col[i]
      px <- price[i]
      sz <- shares[i]
      assign(oref, sz, envir = remaining)
      assign(oref, sd, envir = side_of)
      assign(oref, px, envir = price_of)
      emit(ts_ns[i], "NEW", oref, sd, "LIMIT", px, sz)
    } else if (m == "D") {
      oref <- order_ref[i]
      if (exists(oref, envir = remaining, inherits = FALSE)) {
        rem <- get(oref, envir = remaining, inherits = FALSE)
        emit(ts_ns[i], "CANCEL", oref, get(oref, envir = side_of, inherits = FALSE), "LIMIT",
          get(oref, envir = price_of, inherits = FALSE), rem)
        rm(list = oref, envir = remaining)
        rm(list = oref, envir = side_of)
        rm(list = oref, envir = price_of)
      }
    } else if (m == "X" || m == "E" || m == "C") {
      oref <- order_ref[i]
      if (exists(oref, envir = remaining, inherits = FALSE)) {
        rem <- get(oref, envir = remaining, inherits = FALSE)
        sd <- get(oref, envir = side_of, inherits = FALSE)
        px <- get(oref, envir = price_of, inherits = FALSE)
        new_rem <- rem - shares[i]
        if (new_rem <= 1e-9) {
          emit(ts_ns[i], "CANCEL", oref, sd, "LIMIT", px, rem)
          rm(list = oref, envir = remaining)
          rm(list = oref, envir = side_of)
          rm(list = oref, envir = price_of)
        } else {
          assign(oref, new_rem, envir = remaining)
          emit(ts_ns[i], "REDUCE", oref, sd, "LIMIT", px, new_rem)
        }
      }
    } else if (m == "U") {
      oref <- order_ref[i]
      if (exists(oref, envir = remaining, inherits = FALSE)) {
        sd <- get(oref, envir = side_of, inherits = FALSE)
        rem <- get(oref, envir = remaining, inherits = FALSE)
        emit(ts_ns[i], "CANCEL", oref, sd, "LIMIT", get(oref, envir = price_of, inherits = FALSE), rem)
        rm(list = oref, envir = remaining)
        rm(list = oref, envir = side_of)
        rm(list = oref, envir = price_of)

        new_oref <- new_order_ref[i]
        new_px <- price[i]
        new_sz <- shares[i]
        assign(new_oref, new_sz, envir = remaining)
        assign(new_oref, sd, envir = side_of)
        assign(new_oref, new_px, envir = price_of)
        emit(ts_ns[i], "NEW", new_oref, sd, "LIMIT", new_px, new_sz)
      }
    }
    # E/C are folded into the X/E/C branch above for the resting side's own
    # book-quantity effect; the aggressor's own NEW MARKET row is injected
    # separately below, from recover_aggressors()'s already-validated output.
  }

  idx <- seq_len(k)
  itch_part <- data.table::data.table(
    ts_ns = out_ts[idx], event = out_event[idx], order_id = out_oid[idx],
    side = out_side[idx], type = out_type[idx], price = out_price[idx], quantity = out_qty[idx]
  )

  n_agg <- nrow(aggressor_events)
  agg_part <- data.table::data.table(
    ts_ns = aggressor_events$ts_ns, event = "NEW",
    order_id = as.character(synthetic_id_base + seq_len(n_agg)),
    side = aggressor_events$aggressor_side, type = "MARKET",
    price = NA_real_, quantity = aggressor_events$quantity
  )

  combined <- data.table::rbindlist(list(itch_part, agg_part))
  data.table::setorder(combined, ts_ns)
  combined[, seq := seq_len(.N)]
  data.table::setcolorder(combined, interchange_columns)
  combined[]
}

# ---------------------------------------------------------------------------
# Shared book-reconstruction / replay engine -- the code both datasets run
# through. Consumes only the interchange schema, so it cannot tell a real
# event from a synthetic one.
# ---------------------------------------------------------------------------

replay_interchange <- function(events, tick_size = 0.01) {
  n <- nrow(events)
  ts_ns <- events$ts_ns
  ev <- events$event
  oid <- events$order_id
  side_col <- ifelse(events$side == "BUY", "B", "S")
  type_col <- events$type
  price_col <- events$price / 10000
  qty_col <- events$quantity

  bid_levels <- new.env(parent = emptyenv())
  ask_levels <- new.env(parent = emptyenv())
  orders <- new.env(parent = emptyenv()) # oid -> list(side, price, remaining, add_ts, queue_ahead)
  best_bid <- NA_real_
  best_ask <- NA_real_

  price_key <- function(p) sprintf("%d", as.integer(round(p / tick_size)))
  level_get <- function(side, p) {
    env <- if (side == "B") bid_levels else ask_levels
    key <- price_key(p)
    if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env, inherits = FALSE) else 0
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
  }
  rescan_best <- function(side) {
    env <- if (side == "B") bid_levels else ask_levels
    keys <- ls(env, all.names = TRUE)
    if (length(keys) == 0) {
      return(NA_real_)
    }
    prices <- as.numeric(keys) * tick_size
    if (side == "B") max(prices) else min(prices)
  }

  # Spread/mid sampled after every applied event.
  spread_ts <- numeric(n)
  mid_ts <- numeric(n)
  # Depth by distance-in-ticks-from-touch, snapshotted periodically (every
  # sample_every events) rather than every event, since this is an O(levels)
  # scan; the profile changes slowly relative to event-to-event noise anyway.
  n_depth_levels <- 20
  depth_bid_acc <- numeric(n_depth_levels)
  depth_ask_acc <- numeric(n_depth_levels)
  n_depth_samples <- 0L
  sample_every <- 50L

  # Order lifecycle, for lifetime and fill-probability-by-queue-position.
  n_lc_max <- n
  lc_add_ts <- bit64::integer64(n_lc_max)
  lc_end_ts <- rep(bit64::NA_integer64_, n_lc_max)
  lc_queue_ahead <- numeric(n_lc_max)
  lc_filled <- logical(n_lc_max) # TRUE = left via a fill, FALSE = voluntary cancel
  n_lc <- 0L

  trade_sizes <- numeric(0)

  # A CANCEL/REDUCE is fill-driven iff it shares its ts_ns with a NEW MARKET
  # row (see this file's header comment for why that convention is valid for
  # both translated-real and simulated data).
  market_ts_ns <- unique(ts_ns[type_col == "MARKET" & ev == "NEW"])
  is_market_ts <- new.env(parent = emptyenv())
  for (t in as.character(market_ts_ns)) assign(t, TRUE, envir = is_market_ts)

  progress_every <- 20000L
  t_progress <- Sys.time()
  for (i in seq_len(n)) {
    if (i %% progress_every == 0) {
      cat(
        "replay_interchange: ", i, "/", n, " (", round(100 * i / n), "%), ",
        round(as.numeric(Sys.time() - t_progress, units = "secs"), 1), "s for last ", progress_every, " rows\n",
        sep = ""
      )
      t_progress <- Sys.time()
    }
    e <- ev[i]
    if (e == "NEW" && type_col[i] == "LIMIT") {
      sd <- side_col[i]
      px <- price_col[i]
      sz <- qty_col[i]
      qa <- level_get(sd, px)
      level_adjust(sd, px, sz)
      if (sd == "B") {
        if (is.na(best_bid) || px > best_bid) best_bid <- px
      } else {
        if (is.na(best_ask) || px < best_ask) best_ask <- px
      }
      n_lc <- n_lc + 1L
      lc_add_ts[n_lc] <- ts_ns[i]
      lc_queue_ahead[n_lc] <- qa
      assign(oid[i], list(side = sd, price = px, remaining = sz, lc_index = n_lc), envir = orders)
    } else if (e == "NEW" && type_col[i] == "MARKET") {
      trade_sizes[length(trade_sizes) + 1L] <- qty_col[i]
    } else if (e == "CANCEL" || e == "REDUCE") {
      o <- if (exists(oid[i], envir = orders, inherits = FALSE)) get(oid[i], envir = orders, inherits = FALSE) else NULL
      if (!is.null(o)) {
        filled <- exists(as.character(ts_ns[i]), envir = is_market_ts, inherits = FALSE)
        if (e == "CANCEL") {
          level_adjust(o$side, o$price, -o$remaining)
          lc_end_ts[o$lc_index] <- ts_ns[i]
          lc_filled[o$lc_index] <- filled
          rm(list = oid[i], envir = orders)
          touched <- if (o$side == "B") (!is.na(best_bid) && o$price == best_bid) else (!is.na(best_ask) && o$price == best_ask)
          if (touched && level_get(o$side, o$price) <= 0) {
            if (o$side == "B") best_bid <- rescan_best("B") else best_ask <- rescan_best("S")
          }
        } else { # REDUCE: quantity is the new absolute remaining (Leka semantics)
          delta <- qty_col[i] - o$remaining
          level_adjust(o$side, o$price, delta)
          o$remaining <- qty_col[i]
          assign(oid[i], o, envir = orders)
        }
      }
    }

    spread_ts[i] <- if (is.na(best_bid) || is.na(best_ask)) NA_real_ else best_ask - best_bid
    mid_ts[i] <- if (is.na(best_bid) || is.na(best_ask)) NA_real_ else (best_bid + best_ask) / 2

    if (i %% sample_every == 0 && !is.na(best_bid) && !is.na(best_ask)) {
      for (lvl in seq_len(n_depth_levels)) {
        depth_bid_acc[lvl] <- depth_bid_acc[lvl] + level_get("B", best_bid - (lvl - 1) * tick_size)
        depth_ask_acc[lvl] <- depth_ask_acc[lvl] + level_get("S", best_ask + (lvl - 1) * tick_size)
      }
      n_depth_samples <- n_depth_samples + 1L
    }
  }

  idx_lc <- seq_len(n_lc)
  lifecycle <- data.table::data.table(
    add_ts_ns = lc_add_ts[idx_lc], end_ts_ns = lc_end_ts[idx_lc],
    queue_ahead = lc_queue_ahead[idx_lc], filled = lc_filled[idx_lc]
  )

  list(
    spread = spread_ts[!is.na(spread_ts)],
    mid = data.table::data.table(ts_ns = ts_ns, mid = mid_ts),
    depth_profile = if (n_depth_samples > 0) {
      data.table::data.table(
        level = seq_len(n_depth_levels),
        bid_depth = depth_bid_acc / n_depth_samples,
        ask_depth = depth_ask_acc / n_depth_samples
      )
    } else {
      data.table::data.table(level = integer(0), bid_depth = numeric(0), ask_depth = numeric(0))
    },
    lifecycle = lifecycle,
    trade_sizes = trade_sizes
  )
}

# ---------------------------------------------------------------------------
# The seven comparisons
# ---------------------------------------------------------------------------

compare_distributions <- function(real, synth, label_real = "real", label_synth = "synthetic") {
  results <- list()

  # 1. Spread distribution
  ks_spread <- suppressWarnings(stats::ks.test(real$spread, synth$spread))
  results$spread <- list(ks_stat = unname(ks_spread$statistic), ks_p = ks_spread$p.value)

  # 2. Depth profile by level (RMSE of the mean depth-by-distance profile,
  # each side normalized by its own total so shape is compared, not scale).
  dp_r <- real$depth_profile
  dp_s <- synth$depth_profile
  n_lvl <- min(nrow(dp_r), nrow(dp_s))
  norm <- function(x) if (sum(x) > 0) x / sum(x) else x
  depth_rmse <- if (n_lvl > 0) {
    sqrt(mean((norm(dp_r$bid_depth[seq_len(n_lvl)]) - norm(dp_s$bid_depth[seq_len(n_lvl)]))^2, na.rm = TRUE))
  } else {
    NA_real_
  }
  results$depth_profile <- list(rmse_normalized = depth_rmse)

  # 3. Order lifetime / time-to-cancel
  lifetime_of <- function(lc) {
    censored <- is.na(lc$end_ts_ns)
    end <- ifelse(censored, max(lc$add_ts_ns, na.rm = TRUE), lc$end_ts_ns)
    as.numeric(end - lc$add_ts_ns) / 1e9
  }
  lt_r <- lifetime_of(real$lifecycle)
  lt_s <- lifetime_of(synth$lifecycle)
  ks_lifetime <- suppressWarnings(stats::ks.test(lt_r, lt_s))
  results$lifetime <- list(ks_stat = unname(ks_lifetime$statistic), ks_p = ks_lifetime$p.value)

  # 4. Fill probability by queue position at entry (deciled by queue_ahead)
  fill_prob_by_decile <- function(lc) {
    lc <- lc[!is.na(lc$queue_ahead)]
    if (nrow(lc) < 20) {
      return(data.table::data.table(decile = integer(0), fill_prob = numeric(0)))
    }
    breaks <- unique(stats::quantile(lc$queue_ahead, probs = seq(0, 1, 0.1)))
    lc$decile <- cut(lc$queue_ahead, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    agg <- lc[, .(fill_prob = mean(filled)), by = decile]
    data.table::setorder(agg, decile)
    agg
  }
  fp_r <- fill_prob_by_decile(real$lifecycle)
  fp_s <- fill_prob_by_decile(synth$lifecycle)
  n_dec <- min(nrow(fp_r), nrow(fp_s))
  fill_rmse <- if (n_dec > 0) sqrt(mean((fp_r$fill_prob[seq_len(n_dec)] - fp_s$fill_prob[seq_len(n_dec)])^2)) else NA_real_
  results$fill_probability <- list(rmse = fill_rmse, real = fp_r, synth = fp_s)

  # 5 & 6. Return autocorrelation (lag 1, should be ~0) and volatility
  # clustering (ACF of |returns|, should be positive).
  returns_of <- function(mid_dt) {
    m <- mid_dt$mid[!is.na(mid_dt$mid)]
    diff(m) / head(m, -1)
  }
  ret_r <- returns_of(real$mid)
  ret_s <- returns_of(synth$mid)
  acf1 <- function(x) stats::acf(x[is.finite(x)], lag.max = 1, plot = FALSE)$acf[2]
  acf1_abs <- function(x) stats::acf(abs(x[is.finite(x)]), lag.max = 1, plot = FALSE)$acf[2]
  results$return_acf_lag1 <- list(real = acf1(ret_r), synth = acf1(ret_s))
  results$volatility_clustering_acf_lag1 <- list(real = acf1_abs(ret_r), synth = acf1_abs(ret_s))

  # 7. Trade size distribution
  ks_trade <- suppressWarnings(stats::ks.test(real$trade_sizes, synth$trade_sizes))
  results$trade_size <- list(ks_stat = unname(ks_trade$statistic), ks_p = ks_trade$p.value)

  results
}

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------

plot_comparison <- function(real, synth, results, out_dir, tag) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  grDevices::png(file.path(out_dir, paste0(tag, "_spread.png")), width = 800, height = 500)
  plot(stats::density(real$spread), main = "Spread distribution", xlab = "spread ($)", col = "black", lwd = 2)
  graphics::lines(stats::density(synth$spread), col = "red", lwd = 2)
  graphics::legend("topright", c("real", "synthetic"), col = c("black", "red"), lwd = 2)
  graphics::mtext(sprintf("KS D=%.4f", results$spread$ks_stat), side = 3)
  grDevices::dev.off()

  grDevices::png(file.path(out_dir, paste0(tag, "_depth_profile.png")), width = 800, height = 500)
  graphics::plot(real$depth_profile$level, real$depth_profile$bid_depth,
    type = "l", col = "black", lwd = 2, xlab = "level (ticks from touch)", ylab = "mean resting size",
    main = "Depth profile by level (bid side)",
    ylim = range(c(real$depth_profile$bid_depth, synth$depth_profile$bid_depth), na.rm = TRUE)
  )
  graphics::lines(synth$depth_profile$level, synth$depth_profile$bid_depth, col = "red", lwd = 2)
  graphics::legend("topright", c("real", "synthetic"), col = c("black", "red"), lwd = 2)
  graphics::mtext(sprintf("normalized RMSE=%.4f", results$depth_profile$rmse_normalized), side = 3)
  grDevices::dev.off()

  lt_r <- as.numeric(real$lifecycle$end_ts_ns - real$lifecycle$add_ts_ns) / 1e9
  lt_s <- as.numeric(synth$lifecycle$end_ts_ns - synth$lifecycle$add_ts_ns) / 1e9
  grDevices::png(file.path(out_dir, paste0(tag, "_lifetime.png")), width = 800, height = 500)
  graphics::plot(stats::ecdf(lt_r[is.finite(lt_r)]),
    main = "Order lifetime ECDF", xlab = "seconds resting", col = "black", lwd = 2, do.points = FALSE
  )
  graphics::lines(stats::ecdf(lt_s[is.finite(lt_s)]), col = "red", lwd = 2, do.points = FALSE)
  graphics::legend("bottomright", c("real", "synthetic"), col = c("black", "red"), lwd = 2)
  graphics::mtext(sprintf("KS D=%.4f", results$lifetime$ks_stat), side = 3)
  grDevices::dev.off()

  grDevices::png(file.path(out_dir, paste0(tag, "_fill_probability.png")), width = 800, height = 500)
  fp_r <- results$fill_probability$real
  fp_s <- results$fill_probability$synth
  graphics::plot(fp_r$decile, fp_r$fill_prob,
    type = "b", col = "black", lwd = 2, ylim = c(0, 1),
    xlab = "queue-position decile at entry (1=front)", ylab = "P(filled)",
    main = "Fill probability by queue position"
  )
  graphics::lines(fp_s$decile, fp_s$fill_prob, type = "b", col = "red", lwd = 2)
  graphics::legend("topright", c("real", "synthetic"), col = c("black", "red"), lwd = 2)
  grDevices::dev.off()

  acf_plot <- function(x, main, path) {
    grDevices::png(path, width = 800, height = 500)
    stats::acf(x[is.finite(x)], lag.max = 10, main = main)
    grDevices::dev.off()
  }
  ret_r <- diff(real$mid$mid[!is.na(real$mid$mid)]) / head(real$mid$mid[!is.na(real$mid$mid)], -1)
  ret_s <- diff(synth$mid$mid[!is.na(synth$mid$mid)]) / head(synth$mid$mid[!is.na(synth$mid$mid)], -1)
  acf_plot(ret_r, "Return ACF, real", file.path(out_dir, paste0(tag, "_return_acf_real.png")))
  acf_plot(ret_s, "Return ACF, synthetic", file.path(out_dir, paste0(tag, "_return_acf_synth.png")))
  acf_plot(abs(ret_r), "|Return| ACF (vol. clustering), real", file.path(out_dir, paste0(tag, "_volclustering_acf_real.png")))
  acf_plot(abs(ret_s), "|Return| ACF (vol. clustering), synthetic", file.path(out_dir, paste0(tag, "_volclustering_acf_synth.png")))

  grDevices::png(file.path(out_dir, paste0(tag, "_trade_size.png")), width = 800, height = 500)
  graphics::plot(stats::ecdf(real$trade_sizes), main = "Trade size ECDF", xlab = "shares", col = "black", lwd = 2, do.points = FALSE)
  graphics::lines(stats::ecdf(synth$trade_sizes), col = "red", lwd = 2, do.points = FALSE)
  graphics::legend("bottomright", c("real", "synthetic"), col = c("black", "red"), lwd = 2)
  graphics::mtext(sprintf("KS D=%.4f", results$trade_size$ks_stat), side = 3)
  grDevices::dev.off()
}

report <- function(results, model_label) {
  cat("\n=== ", model_label, " vs. real ===\n", sep = "")
  cat(sprintf("1. Spread distribution:        KS D=%.4f (p=%.3g)\n", results$spread$ks_stat, results$spread$ks_p))
  cat(sprintf("2. Depth profile by level:     normalized RMSE=%.4f\n", results$depth_profile$rmse_normalized))
  cat(sprintf("3. Order lifetime:             KS D=%.4f (p=%.3g)\n", results$lifetime$ks_stat, results$lifetime$ks_p))
  cat(sprintf("4. Fill prob. by queue pos.:   RMSE=%.4f\n", results$fill_probability$rmse))
  cat(sprintf(
    "5. Return ACF (lag 1, want ~0): real=%.4f  synthetic=%.4f\n",
    results$return_acf_lag1$real, results$return_acf_lag1$synth
  ))
  cat(sprintf(
    "6. |Return| ACF (lag 1, vol. clustering, want >0): real=%.4f  synthetic=%.4f\n",
    results$volatility_clustering_acf_lag1$real, results$volatility_clustering_acf_lag1$synth
  ))
  cat(sprintf("7. Trade size distribution:     KS D=%.4f (p=%.3g)\n", results$trade_size$ks_stat, results$trade_size$ks_p))

  vc_real <- results$volatility_clustering_acf_lag1$real
  vc_synth <- results$volatility_clustering_acf_lag1$synth
  if (vc_real > 0.05 && vc_synth < 0.02) {
    cat(
      "-> Volatility clustering present in real data but absent from ", model_label,
      ": expected for a pure Poisson model, and the argument for Hawkes.\n",
      sep = ""
    )
  } else if (vc_real > 0.05 && vc_synth > 0.05) {
    cat("-> Volatility clustering present in both: ", model_label, " reproduces this property.\n", sep = "")
  }
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
# Run from the repo root, one symbol at a time (default AAPL):
#   Rscript scripts/analyze.R AAPL
#   Rscript scripts/analyze.R MSFT

args <- commandArgs(trailingOnly = TRUE)
symbol <- if (length(args) >= 1) args[1] else "AAPL"

itch_interchange_path <- sprintf("data/processed/itch_interchange_%s_BX_20190730.rds", symbol)
itch_csv_path <- sprintf("data/raw/20190730.BX.%s.csv", symbol)

if (file.exists(itch_interchange_path)) {
  real_events <- readRDS(itch_interchange_path)
  cat("Loaded cached real-data interchange translation for", symbol, "\n")
} else {
  dt <- load_itch_csv(itch_csv_path)
  book <- reconstruct_book(dt, tick_size = 0.01, grid_interval_s = 1)
  recovered <- recover_aggressors(book$executed_events)
  real_events <- translate_itch_to_interchange(dt, recovered$events)
  validate_interchange(real_events)
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  saveRDS(real_events, itch_interchange_path)
  cat("Translated and cached real-data interchange stream (", nrow(real_events), " rows).\n", sep = "")
}

calibration <- readRDS(sprintf("data/processed/calibration_%s_BX_20190730.rds", symbol))
# The synthetic comparison doesn't need to match the real session's full
# ~16-hour span -- these are distribution-shape comparisons (KS tests,
# normalized profiles, ACF), which don't require equal sample sizes, and a
# few thousand synthetic events already gets there. Simulating the full
# span would mean hundreds of thousands of Gillespie-loop events, which is
# what made this step the accidental multi-hour bottleneck before this fix.
synth_duration_s <- 600
real_duration_s <- as.numeric(max(real_events$ts_ns) - min(real_events$ts_ns)) / 1e9

real_replay <- replay_interchange(real_events, tick_size = 0.01)

for (m in c("poisson", "hawkes")) {
  sim <- simulate_market(calibration, duration_s = synth_duration_s, model = m, seed = 1)
  synth_replay <- replay_interchange(sim$events, tick_size = 0.01)
  results <- compare_distributions(real_replay, synth_replay)
  report(results, paste0(symbol, "/", m))
  tag <- paste0(symbol, "_", m)
  plot_comparison(real_replay, synth_replay, results, "results/plots", tag)
  cat("Plots written to results/plots/", tag, "_*.png\n", sep = "")
}
