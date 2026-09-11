# R/data_transform.R

library(bit64)

# Convert ISO-8601 timestamps with nanosecond precision into integer nanoseconds.
# We keep the original ts_event strings unchanged for readability and auditability,
# but store timing features in nanoseconds so the simulator can work in a single canonical unit.
format_ns_timestamp <- function(ts_vec) {
  clean_ts <- sub("Z$", "", ts_vec)

  sec_part <- as.numeric(as.POSIXct(
    sub("\\.[0-9]+$", "", clean_ts),
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  ))

  frac_part <- sub("^.*\\.([0-9]+)$", "\\1", clean_ts)
  frac_part <- ifelse(is.na(frac_part) | frac_part == "", "0", frac_part)
  frac_part <- ifelse(
    nchar(frac_part) > 9,
    substr(frac_part, 1, 9),
    paste0(strrep("0", 9 - nchar(frac_part)), frac_part)
  )

  as.integer64(sec_part * 1e9 + as.integer(frac_part))
}

add_event_features <- function(data) {

  data$event_type <- NA_character_

  data$event_type[data$action == "A"] <- "ADD"
  data$event_type[data$action == "C"] <- "CANCEL"
  data$event_type[data$action == "M"] <- "MODIFY"
  data$event_type[data$action == "R"] <- "RESET"
  data$event_type[data$action == "T"] <- "TRADE"

  data$side_type <- NA_character_

  data$side_type[data$side == "B"] <- "BUY"
  data$side_type[data$side == "A"] <- "SELL"
  data$side_type[data$side == "N"] <- "NONE"

  return(data)
}

add_interarrival_times <- function(data) {

  event_time_ns <- format_ns_timestamp(data$ts_event)

  data$interarrival_time_ns <- c(
    NA_integer64_,
    diff(event_time_ns)
  )

  return(data)
}

transform_market_data <- function(data) {

  data <- add_event_features(data)

  data <- add_interarrival_times(data)

  return(data)
}