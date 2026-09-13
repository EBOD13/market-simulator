# scripts/benchmark.R
#
# Runs the Leka repo's tools/benchmark/benchmark_interchange.cpp against the
# real (translated) ITCH day and both synthetic models, then compares
# per-event-type latency percentiles across sources. That tool times only
# the MatchingEngine::processEvent() call itself (materializing every event
# from the CSV into memory first, so parsing is never part of what's
# measured) and reports p50/p90/p99/p99.9/max per category -- never a mean.
#
# If real and synthetic latency diverge, that is a finding about this
# repo's simulated book shape (depth-per-level, price range, event mix),
# not about Leka: the same C++ code replays both.
#
# Run from the repo root, one symbol at a time (default AAPL):
#   Rscript scripts/benchmark.R AAPL
#   Rscript scripts/benchmark.R MSFT
# Requires: the Leka repo checked out as a sibling directory (override with
# the LEKA_REPO_PATH env var), with benchmark_interchange built (this script
# will (re)build it via cmake if missing). Also requires
# scripts/analyze.R <symbol> to have run first, for the cached real-data
# interchange translation.

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

args <- commandArgs(trailingOnly = TRUE)
symbol <- if (length(args) >= 1) args[1] else "AAPL"

leka_path <- Sys.getenv("LEKA_REPO_PATH", unset = normalizePath("../leka", mustWork = FALSE))
if (!dir.exists(leka_path)) {
  stop(
    "Leka repo not found at '", leka_path, "'. Set LEKA_REPO_PATH to its location."
  )
}

tool_path <- file.path(leka_path, "build", "benchmark_interchange")
if (!file.exists(tool_path)) {
  cat("benchmark_interchange not built yet; building it now...\n")
  system2("cmake", c("-S", shQuote(leka_path), "-B", shQuote(file.path(leka_path, "build"))))
  build_status <- system2(
    "cmake",
    c("--build", shQuote(file.path(leka_path, "build")), "--target", "benchmark_interchange", "-j4")
  )
  if (build_status != 0 || !file.exists(tool_path)) {
    stop("Failed to build benchmark_interchange at ", tool_path)
  }
}

# ---------------------------------------------------------------------------
# Ensure the three interchange CSVs exist: real (translated from ITCH,
# cached -- see scripts/analyze.R for the translation) and both synthetic
# models (regenerated fresh here so this always benchmarks the current
# calibration and code, not a stale file).
# ---------------------------------------------------------------------------

itch_interchange_rds <- sprintf("data/processed/itch_interchange_%s_BX_20190730.rds", symbol)
itch_interchange_csv <- sprintf("data/processed/itch_interchange_%s_BX_20190730.csv", symbol)

if (!file.exists(itch_interchange_rds)) {
  stop(
    "No cached real-data interchange translation at ", itch_interchange_rds,
    " -- run: Rscript scripts/analyze.R ", symbol, " first (it builds this as a side effect)."
  )
}
if (!file.exists(itch_interchange_csv)) {
  real_events <- readRDS(itch_interchange_rds)
  write_interchange_csv(real_events, itch_interchange_csv)
  cat("Wrote", itch_interchange_csv, "\n")
}

calibration <- readRDS(sprintf("data/processed/calibration_%s_BX_20190730.rds", symbol))
real_events_for_duration <- readRDS(itch_interchange_rds)
duration_s <- as.numeric(max(real_events_for_duration$ts_ns) - min(real_events_for_duration$ts_ns)) / 1e9

poisson_csv <- sprintf("data/processed/sim_events_poisson_%s.csv", symbol)
hawkes_csv <- sprintf("data/processed/sim_events_hawkes_%s.csv", symbol)
sim_p <- simulate_market(calibration, duration_s = min(duration_s, 600), model = "poisson", seed = 1)
sim_h <- simulate_market(calibration, duration_s = min(duration_s, 600), model = "hawkes", seed = 1)
write_interchange_csv(sim_p$events, poisson_csv)
write_interchange_csv(sim_h$events, hawkes_csv)

# ---------------------------------------------------------------------------
# Run the C++ benchmark against each CSV and parse its percentile report.
# tick=100 is this schema's 1-cent tick (price is raw 1/10000-dollar units).
# ---------------------------------------------------------------------------

run_benchmark <- function(csv_path, tick = 100) {
  out <- system2(tool_path, c("--input", shQuote(csv_path), "--tick", tick), stdout = TRUE, stderr = TRUE)
  lines <- grep("^\\s+\\S+\\s+n=", out, value = TRUE)
  parsed <- lapply(lines, function(l) {
    m <- regmatches(l, regexec(
      "^\\s*(\\S+)\\s+n=(\\d+)\\s+p50=(\\d+)\\s+p90=(\\d+)\\s+p99=(\\d+)\\s+p99\\.9=(\\d+)\\s+max=(\\d+)", l
    ))[[1]]
    if (length(m) == 0) {
      return(NULL)
    }
    data.table::data.table(
      category = m[2], n = as.numeric(m[3]), p50 = as.numeric(m[4]), p90 = as.numeric(m[5]),
      p99 = as.numeric(m[6]), p999 = as.numeric(m[7]), max = as.numeric(m[8])
    )
  })
  list(raw = out, table = data.table::rbindlist(parsed[!sapply(parsed, is.null)]))
}

cat("Running benchmark_interchange against real ITCH day (", symbol, ")...\n", sep = "")
real_bench <- run_benchmark(itch_interchange_csv)
cat("Running benchmark_interchange against synthetic (Poisson)...\n")
poisson_bench <- run_benchmark(poisson_csv)
cat("Running benchmark_interchange against synthetic (Hawkes)...\n")
hawkes_bench <- run_benchmark(hawkes_csv)

cat("\n=== Real ITCH day (", symbol, ") ===\n", sep = "")
print(real_bench$table)
cat("\n=== Synthetic (Poisson) ===\n")
print(poisson_bench$table)
cat("\n=== Synthetic (Hawkes) ===\n")
print(hawkes_bench$table)

# ---------------------------------------------------------------------------
# Compare: for each category present in both, report the ratio of p99s.
# ---------------------------------------------------------------------------

compare_latency <- function(real_table, synth_table, synth_label) {
  cat("\n=== real vs. ", synth_label, " (p99 ratio, >1 = real slower) ===\n", sep = "")
  merged <- merge(real_table, synth_table, by = "category", suffixes = c("_real", "_synth"))
  if (nrow(merged) == 0) {
    cat("No overlapping event-type categories to compare.\n")
    return(invisible(NULL))
  }
  merged[, p99_ratio := p99_real / p99_synth]
  print(merged[, .(category, n_real, n_synth, p99_real, p99_synth, p99_ratio)])

  diverging <- merged[p99_ratio > 2 | p99_ratio < 0.5]
  if (nrow(diverging) > 0) {
    cat(
      "\nDiverging by >2x for: ", paste(diverging$category, collapse = ", "),
      ". Both runs go through the identical C++ matching code, so a gap this\n",
      "large points at a difference in book *shape* between the two event\n",
      "streams (price range, depth-per-level, how often orders land near the\n",
      "touch vs. deep in the book) -- a finding about the simulator's realism,\n",
      "not about Leka's matching engine.\n",
      sep = ""
    )
  } else {
    cat("\nNo category diverges by more than 2x: latency is consistent across real and synthetic flow.\n")
  }
}

compare_latency(real_bench$table, poisson_bench$table, "synthetic (Poisson)")
compare_latency(real_bench$table, hawkes_bench$table, "synthetic (Hawkes)")
