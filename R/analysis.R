# Ensure the helper functions used by this analysis file are loaded.
# This allows the script to run with a single source() call in a fresh R session.
script_dir <- if (!is.null(sys.frames()[[1]]$ofile)) {
  dirname(normalizePath(sys.frames()[[1]]$ofile))
} else {
  getwd()
}

source(file.path(script_dir, "data_loader.R"), encoding = "UTF-8")
source(file.path(script_dir, "data_cleaner.R"), encoding = "UTF-8")
source(file.path(script_dir, "data_transform.R"), encoding = "UTF-8")

# Load and process the Microsoft MBO dataset.
msft_mbo <- load_market_data("data/raw/msft_mbo/mbo_dataset_1.csv")
msft_mbo <- clean_market_data(msft_mbo)
msft_mbo <- transform_market_data(msft_mbo)

msft_mbp <- load_market_data("data/raw/msft_mbp/mbp_dataset_2.csv")
msft_mbp <- clean_market_data(msft_mbp)
msft_mbp <- transform_market_data(msft_mbp)

es_mbo <- load_market_data("data/raw/es_mbo/mbo_dataset_3.csv")
es_mbo <- clean_market_data(es_mbo)
es_mbo <- transform_market_data(es_mbo)

# print(table(msft_mbo$event_type))
# print(table(msft_mbp$event_type))
# print(table(es_mbo$event_type))

# print(table(msft_mbo$event_type, msft_mbo$side_type))
# print(table(msft_mbp$event_type, msft_mbp$side_type))
# print(table(es_mbo$event_type, es_mbo$side_type))

# Question: What empirical event/side behavior is consistent across markets and data representations?
# MSFT MBO → individual order-level events
# MSFT MBP → market-by-price/top-of-book updates
# ESZ2 MBO → individual futures order-level events

# Raw LOBSTER-style dataset profiling:
# Print the event counts and side counts from the normalized CSV so the dataset composition is visible
# immediately when the analysis script is sourced.
profile_dataset <- function(df, label) {
  print(paste0("=== ", label, " ==="))

  if ("event_type" %in% names(df)) {
    print("event_type counts:")
    print(table(df$event_type, useNA = "ifany"))
  }

  if ("side" %in% names(df)) {
    print("side counts:")
    print(table(df$side, useNA = "ifany"))
  }

  if ("event_type" %in% names(df) && "side" %in% names(df)) {
    print("event_type x side counts:")
    print(table(df$event_type, df$side, useNA = "ifany"))
  }

  print("available event values:")
  print(if ("event_type" %in% names(df)) sort(unique(df$event_type)) else character(0))

  print("available side values:")
  print(if ("side" %in% names(df)) sort(unique(df$side)) else character(0))

  print("")
}

if (file.exists("data/raw/data.csv")) {
  lobster_df <- read.csv("data/raw/data.csv", stringsAsFactors = FALSE)
  profile_dataset(lobster_df, "Current normalized CSV")
}

# Canonical simulator categories we want to support.
# These are the event, order type, and side states that should be represented at the Leka layer.
canonical_events <- c("NEW", "CANCEL", "MODIFY")
canonical_types <- c("LIMIT", "MARKET")
canonical_sides <- c("ASK", "BID")

print("Canonical simulator categories:")
print(list(
  event = canonical_events,
  type = canonical_types,
  side = canonical_sides
))

print("Current msft_mbo event_type values available:")
print(sort(unique(msft_mbo$event_type)))
print("Current msft_mbo side_type values available:")
print(sort(unique(msft_mbo$side_type)))

if (file.exists("data/raw/data.csv")) {
  print("Current lobster_df event_type values available:")
  print(sort(unique(lobster_df$event_type)))
  print("Current lobster_df side values available:")
  print(sort(unique(lobster_df$side)))
}

# Detect the raw LOBSTER files that contain message- and book-level data.
message_files <- list.files(
  "data",
  pattern = "_message_.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
book_files <- list.files(
  "data",
  pattern = "_orderbook_.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

print("Detected message files:")
print(message_files)
print("Detected orderbook files:")
print(book_files)

if (length(message_files) > 0) {
  message_df <- read.csv(message_files[1], stringsAsFactors = FALSE)
  profile_dataset(message_df, "Detected message file")
}

if (length(book_files) > 0) {
  book_df <- read.csv(book_files[1], stringsAsFactors = FALSE)
  profile_dataset(book_df, "Detected orderbook file")
} else {
  print("No true LOBSTER orderbook CSV found. The current CSV is a normalized book-update stream, not a raw message stream.")
}

print("Availability check:")
print(list(
  new_available = "NEW" %in% c(unique(msft_mbo$event_type), if (file.exists("data/raw/data.csv")) unique(lobster_df$event_type) else character(0)),
  cancel_available = "CANCEL" %in% c(unique(msft_mbo$event_type), if (file.exists("data/raw/data.csv")) unique(lobster_df$event_type) else character(0)),
  modify_available = "MODIFY" %in% c(unique(msft_mbo$event_type), if (file.exists("data/raw/data.csv")) unique(lobster_df$event_type) else character(0)),
  limit_available = "LIMIT" %in% c(unique(msft_mbo$event_type), if (file.exists("data/raw/data.csv")) unique(lobster_df$event_type) else character(0)),
  market_available = "MARKET" %in% c(unique(msft_mbo$event_type), if (file.exists("data/raw/data.csv")) unique(lobster_df$event_type) else character(0)),
  ask_available = "ASK" %in% c(unique(msft_mbo$side_type), if (file.exists("data/raw/data.csv")) unique(lobster_df$side) else character(0)),
  bid_available = "BID" %in% c(unique(msft_mbo$side_type), if (file.exists("data/raw/data.csv")) unique(lobster_df$side) else character(0))
))