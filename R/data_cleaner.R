# R/data_cleaner.R

clean_market_data <- function(data) {
  # Define the fields that every market-event row must contain.
  # These are the core columns used by the simulator and downstream analytics.
  required_columns <- c(
    "ts_recv",
    "ts_event",
    "rtype",
    "instrument_id",
    "action",
    "side",
    "price",
    "size",
    "sequence",
    "symbol"
  )

  # Compare expected columns against the actual columns in the incoming dataset.
  # If anything is missing, stop early so downstream code does not fail unpredictably.
  missing_columns <- setdiff(required_columns, colnames(data))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Remove rows where the event timestamp is missing, because ordering and timing logic depends on it.
  data <- data[!is.na(data$ts_event), ]

  # Drop rows with no action code; actions tell us whether the event is add, cancel, modify, etc.
  data <- data[!is.na(data$action), ]

  # Drop rows with no side information; side identifies whether the event is buy or sell related.
  data <- data[!is.na(data$side), ]

  # Keep only non-negative trade sizes.
  # Negative values are usually invalid for this market data schema and would distort analytics.
  data <- data[data$size >= 0, ]

  # Keep only valid event action codes.
  # The allowed values represent the message types the simulator expects to process.
  data <- data[data$action %in% c("A", "C", "M", "R", "T"), ]

  # Keep only valid side codes.
  # These values correspond to ask/bid/neutral-type market states used in the model.
  data <- data[data$side %in% c("A", "B", "N"), ]

  # Sort rows by event time and sequence number so the event stream is in a consistent order.
  # This is important because market events are time-sensitive and sequence-based ordering matters.
  data <- data[order(data$ts_event, data$sequence), ]

  # Reset row names to keep the dataframe clean after filtering and reordering.
  rownames(data) <- NULL

  # Return the cleaned dataset ready for the rest of the simulation pipeline.
  return(data)
}