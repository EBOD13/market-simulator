# scripts/simulate.R
#
# Runs the CSTT-baseline and Hawkes-upgrade simulators (R/event_generator.R,
# R/price_model.R) side by side from a cached calibration (see
# scripts/build_calibration_cache.R), validates each against the schema and
# the calibration it came from, and demonstrates that the emergent mid-price
# behaves differently from an exogenous random walk (which is generated only
# for this comparison -- it is never fed into either simulation).
#
# Run from the repo root, one symbol at a time (default AAPL):
#   Rscript scripts/simulate.R AAPL
#   Rscript scripts/simulate.R MSFT

suppressMessages({
  library(data.table)
  library(bit64)
  library(survival)
})
source("R/types.R")
source("R/price_model.R")
source("R/event_generator.R")

args <- commandArgs(trailingOnly = TRUE)
symbol <- if (length(args) >= 1) args[1] else "AAPL"

calibration_path <- sprintf("data/processed/calibration_%s_BX_20190730.rds", symbol)
if (!file.exists(calibration_path)) {
  stop(
    "No cached calibration at ", calibration_path,
    " -- run: Rscript scripts/build_calibration_cache.R ", symbol
  )
}
calibration <- readRDS(calibration_path)
cat("Calibration loaded:", calibration$symbol, calibration$venue, calibration$date, "\n")

duration_s <- 600 # 10 simulated minutes

sim_p <- simulate_market(calibration, duration_s = duration_s, model = "poisson", seed = 1)
val_p <- validate_simulation(sim_p, calibration)

sim_h <- simulate_market(calibration, duration_s = duration_s, model = "hawkes", seed = 1)
val_h <- validate_simulation(sim_h, calibration)

cat("\n=== POISSON (CSTT baseline) ===\n")
cat(val_p$note, "\n")
print(val_p$event_mix)
cat("n_events:", nrow(sim_p$events), "\n")

cat("\n=== HAWKES ===\n")
cat(val_h$note, "\n")
print(val_h$event_mix)
cat("n_events:", nrow(sim_h$events), "\n")

# Emergent mid-price series: recorded straight from the live book after
# every applied event, never sampled as an independent process.
pp_p <- sim_p$price_path
pp_h <- sim_h$price_path
ret_p <- diff(pp_p$mid) / head(pp_p$mid, -1)
ret_h <- diff(pp_h$mid) / head(pp_h$mid, -1)

cat("\n=== Emergent price stats ===\n")
cat(
  "Poisson: n_ticks=", nrow(pp_p), " mid range=[", min(pp_p$mid, na.rm = TRUE), ",",
  max(pp_p$mid, na.rm = TRUE), "] return sd=", sd(ret_p, na.rm = TRUE), "\n",
  sep = ""
)
cat(
  "Hawkes:  n_ticks=", nrow(pp_h), " mid range=[", min(pp_h$mid, na.rm = TRUE), ",",
  max(pp_h$mid, na.rm = TRUE), "] return sd=", sd(ret_h, na.rm = TRUE), "\n",
  sep = ""
)

# Autocorrelation of squared returns is the classic volatility-clustering
# signature: an iid-increment random walk cannot have it by construction,
# so this is a direct check that the emergent price is NOT a random walk.
acf_sq_p <- stats::acf(ret_p[is.finite(ret_p)]^2, lag.max = 5, plot = FALSE)$acf[-1]
acf_sq_h <- stats::acf(ret_h[is.finite(ret_h)]^2, lag.max = 5, plot = FALSE)$acf[-1]
cat("ACF of squared returns (lags 1-5), Poisson:", paste(round(acf_sq_p, 3), collapse = ", "), "\n")
cat("ACF of squared returns (lags 1-5), Hawkes: ", paste(round(acf_sq_h, 3), collapse = ", "), "\n")

# Reference random walk: decoupled comparison only, never fed into either sim.
ref <- generate_reference_random_walk(
  duration_s = duration_s, dt_s = 1, mid0 = pp_p$mid[1],
  sigma_per_sqrt_s = sd(ret_p, na.rm = TRUE) * sqrt(mean(diff(as.numeric(pp_p$ts_ns))) / 1e9),
  start_ts_ns = pp_p$ts_ns[1], seed = 7
)
ret_ref <- diff(ref$mid) / head(ref$mid, -1)
acf_sq_ref <- stats::acf(ret_ref[is.finite(ret_ref)]^2, lag.max = 5, plot = FALSE)$acf[-1]
cat("ACF of squared returns (lags 1-5), reference RW:", paste(round(acf_sq_ref, 3), collapse = ", "), "\n")

# Emit to the interchange schema, one file per model, one set per symbol.
poisson_path <- sprintf("data/processed/sim_events_poisson_%s.csv", symbol)
hawkes_path <- sprintf("data/processed/sim_events_hawkes_%s.csv", symbol)
write_interchange_csv(sim_p$events, poisson_path)
write_interchange_csv(sim_h$events, hawkes_path)
cat("\nWrote", poisson_path, "and", hawkes_path, "(schema-validated).\n")
