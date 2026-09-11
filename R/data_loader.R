# R/data_loader.R
# Load data to be parsed and processed for analysis. This function reads in the raw data files, performs necessary cleaning and transformations, and returns a structured data frame ready for further analysis.

load_market_data <- function(file_path) {
    read.csv(file_path)
}

# Loader for the per-symbol CSVs produced by decoding a Nasdaq TotalView-ITCH
# 5.0 session file with the Leka repo's tools/itch/itch_to_csv.cpp (see
# scripts/fetch_itch.sh for how to obtain the raw session file it reads).
#
# ts_ns/epoch_ns/order_ref/new_order_ref/match_number are typed as
# bit64::integer64: epoch_ns is a full nanosecond epoch timestamp (~1.5e18),
# and order references can run into the billions on a busy main-Nasdaq day --
# both exceed a double's exact-integer range (~2^53), so reading them as
# ordinary numeric would silently corrupt them.
library(data.table)
library(bit64)

itch_csv_colClasses <- c(
  seq = "integer",
  ts_ns = "integer64",
  epoch_ns = "integer64",
  clock_et = "character",
  msg = "character",
  name = "character",
  symbol = "character",
  order_ref = "integer64",
  side = "character",
  shares = "integer",
  price = "numeric",
  new_order_ref = "integer64",
  match_number = "integer64",
  printable = "character",
  attribution = "character"
)

load_itch_csv <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("ITCH CSV not found:", file_path))
  }

  dt <- data.table::fread(
    file_path,
    colClasses = itch_csv_colClasses,
    na.strings = ""
  )

  missing_cols <- setdiff(names(itch_csv_colClasses), names(dt))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "ITCH CSV is missing expected column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Was this file produced by tools/itch/itch_to_csv.cpp?"
    ))
  }

  data.table::setcolorder(dt, names(itch_csv_colClasses))
  dt[]
}