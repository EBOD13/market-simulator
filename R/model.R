# R/model.R
# Statistical modeling and calibration layer for processed market data.
# This file is intentionally minimal, and is meant to be fed by the Python-cleaned dataset.

load_processed_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE)
}

summarize_event_counts <- function(data) {
  if ("event_type" %in% colnames(data)) {
    return(table(data$event_type))
  }

  stop("event_type column is missing from the processed data.")
}

summarize_side_counts <- function(data) {
  if ("side" %in% colnames(data)) {
    return(table(data$side))
  }

  stop("side column is missing from the processed data.")
}

summarize_interarrival_ns <- function(data) {
  if ("event_time_ns" %in% colnames(data)) {
    event_time_ns <- as.numeric(data$event_time_ns)
    dt <- diff(event_time_ns)
    dt <- dt[is.finite(dt)]
    return(summary(dt))
  }

  stop("event_time_ns column is missing from the processed data.")
}
