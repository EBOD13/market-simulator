# scripts/build_calibration_cache.R
#
# Reconstructing the real order book (R/distributions.R's reconstruct_book())
# is the expensive step in this pipeline -- several minutes per symbol-day.
# This script runs it exactly once per symbol, bundles everything
# R/event_generator.R needs to calibrate a simulation from it, and caches the
# result to data/processed/calibration_<SYMBOL>_<VENUE>_<DATE>.rds so later
# work (scripts/simulate.R, scripts/analyze.R, ad hoc analysis) never has to
# repeat it.
#
# Run from the repo root, for one symbol at a time (each is its own several-
# minute pass, so this is intentionally not "build every symbol in one go"):
#   Rscript scripts/build_calibration_cache.R AAPL
#   Rscript scripts/build_calibration_cache.R MSFT
# With no argument, defaults to AAPL. Expects a raw per-symbol ITCH CSV
# already decoded at data/raw/20190730.BX.<SYMBOL>.csv -- see the Leka
# repo's tools/itch/itch_to_csv (--symbol SYM, repeatable) for decoding
# additional symbols out of the same session file this project already has.

suppressMessages({
  library(data.table)
  library(bit64)
  library(survival)
})
source("R/data_loader.R")
source("R/distributions.R")
source("R/aggressors.R")

args <- commandArgs(trailingOnly = TRUE)
symbol <- if (length(args) >= 1) args[1] else "AAPL"
venue <- "BX"
date <- "2019-07-30"
date_compact <- "20190730"

itch_csv <- sprintf("data/raw/%s.%s.%s.csv", date_compact, venue, symbol)
out_path <- sprintf("data/processed/calibration_%s_%s_%s.rds", symbol, venue, date_compact)

if (!file.exists(itch_csv)) {
  stop(
    "No raw ITCH CSV at ", itch_csv, " -- decode it first, e.g. from the Leka repo:\n",
    "  ./build/itch_to_csv --input data/sample/", date_compact, ".", venue, "_ITCH_50 --symbol ", symbol,
    " --output <path>, then split/copy it to ", itch_csv
  )
}

cat("Calibrating", symbol, venue, date, "from", itch_csv, "...\n")
dt <- load_itch_csv(itch_csv)
book <- reconstruct_book(dt, tick_size = 0.01, grid_interval_s = 1)

dist_cal <- calibrate_distributions(dt, tick_size = 0.01, verbose = FALSE, book = book)
recovered <- recover_aggressors(book$executed_events)
intensity <- fit_aggressor_intensity(recovered$events, book$state_grid, tick_size = 0.01)

bundle <- list(
  symbol = symbol, venue = venue, date = date, tick_size = 0.01,
  session_start_ns = dt$ts_ns[1], session_end_ns = dt$ts_ns[nrow(dt)],
  dist = dist_cal, aggressors = recovered, aggressor_intensity = intensity,
  state_grid = book$state_grid
)
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(bundle, out_path)
cat("Saved calibration bundle to", out_path, "\n")
