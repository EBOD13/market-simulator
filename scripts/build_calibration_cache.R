# scripts/build_calibration_cache.R
#
# Reconstructing the real order book (R/distributions.R's reconstruct_book())
# is the expensive step in this pipeline -- roughly 5 minutes for one
# symbol-day. This script runs it exactly once, bundles everything
# R/event_generator.R needs to calibrate a simulation from it, and caches the
# result to data/processed/calibration_<SYMBOL>_<VENUE>_<DATE>.rds so later
# work (scripts/simulate.R, ad hoc analysis) never has to repeat it.
#
# Run from the repo root: Rscript scripts/build_calibration_cache.R

suppressMessages({
  library(data.table)
  library(bit64)
  library(survival)
})
source("R/data_loader.R")
source("R/distributions.R")
source("R/aggressors.R")

itch_csv <- "data/raw/20190730.BX.AAPL.csv"
out_path <- "data/processed/calibration_AAPL_BX_20190730.rds"

dt <- load_itch_csv(itch_csv)
book <- reconstruct_book(dt, tick_size = 0.01, grid_interval_s = 1)

dist_cal <- calibrate_distributions(dt, tick_size = 0.01, verbose = FALSE, book = book)
recovered <- recover_aggressors(book$executed_events)
intensity <- fit_aggressor_intensity(recovered$events, book$state_grid, tick_size = 0.01)

bundle <- list(
  symbol = "AAPL", venue = "BX", date = "2019-07-30", tick_size = 0.01,
  session_start_ns = dt$ts_ns[1], session_end_ns = dt$ts_ns[nrow(dt)],
  dist = dist_cal, aggressors = recovered, aggressor_intensity = intensity,
  state_grid = book$state_grid
)
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(bundle, out_path)
cat("Saved calibration bundle to", out_path, "\n")
