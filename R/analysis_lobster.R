# R/analysis_lobster.R
# Load a cleaned LOBSTER-style dataset and print the exact canonical categories.

load_and_profile_lobster <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  print("Columns:")
  print(names(df))

  print("Event counts:")
  if ("event_type" %in% names(df)) {
    print(table(df$event_type, useNA = "ifany"))
  } else {
    print("No event_type column present.")
  }

  print("Side counts:")
  if ("side" %in% names(df)) {
    print(table(df$side, useNA = "ifany"))
  } else {
    print("No side column present.")
  }

  print("Canonical simulator categories:")
  print(list(
    event = c("NEW", "CANCEL", "MODIFY"),
    type = c("LIMIT", "MARKET"),
    side = c("ASK", "BID")
  ))

  if ("event_type" %in% names(df)) {
    print("Available event values:")
    print(sort(unique(df$event_type)))
  }

  if ("side" %in% names(df)) {
    print("Available side values:")
    print(sort(unique(df$side)))
  }

  if ("event_type" %in% names(df) && "side" %in% names(df)) {
    print("Event frequencies:")
    print(prop.table(table(df$event_type)))

    print("Event x side counts:")
    print(table(df$event_type, df$side))

    print("Conditional side probabilities (P(side | event_type)):")
    print(prop.table(table(df$event_type, df$side), margin = 1))
  }

  return(df)
}

load_and_profile_lobster_book <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  print("Columns:")
  print(names(df))

  book_cols <- c("spread", "mid_price", "imbalance_l1", "bid_depth", "ask_depth", "depth_imbalance")
  missing_cols <- setdiff(book_cols, names(df))
  if (length(missing_cols) > 0) {
    print(paste("Missing expected book feature columns:", paste(missing_cols, collapse = ", ")))
    return(df)
  }

  print("Book feature summary:")
  print(summary(df[, book_cols]))

  if ("event_type" %in% names(df)) {
    print("Mean book state by event type (state just before/at that event):")
    print(aggregate(df[, book_cols], by = list(event_type = df$event_type), FUN = mean, na.rm = TRUE))
  }

  return(df)
}

analyze_order_lifecycles <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  df <- df[order(df$event_time_ns), ]

  print(paste("Total rows:", nrow(df)))
  print(paste("Rows with order_id == 0 (hidden liquidity, no tracked order):", sum(df$order_id == 0)))
  print("Event types seen for order_id == 0:")
  print(table(df$event_type[df$order_id == 0]))

  real <- df[df$order_id != 0, ]
  print(paste("Distinct tracked order_ids (order_id != 0):", length(unique(real$order_id))))

  first_event <- tapply(real$event_type, real$order_id, function(x) x[1])
  print("First event type per order_id (expected: entirely NEW):")
  print(table(first_event))

  seqs <- tapply(real$event_type, real$order_id, function(x) paste(x, collapse = " -> "))
  seq_counts <- sort(table(seqs), decreasing = TRUE)
  print("Most common order_id event sequences (top 15):")
  print(head(seq_counts, 15))
  print(paste(
    "Order_ids with only a single event (still resting at end of sample, or lone NEW):",
    sum(seq_counts[!grepl("->", names(seq_counts))])
  ))

  print("Price constancy across an order's lifecycle:")
  price_nunique <- tapply(real$price, real$order_id, function(x) length(unique(x)))
  print(paste("Order_ids with >1 distinct price across their lifecycle:", sum(price_nunique > 1)))

  print("Size conservation check (NEW size vs. total size removed by CANCEL/DELETE/EXECUTE_VISIBLE):")
  new_size <- tapply(real$size[real$event_type == "NEW"], real$order_id[real$event_type == "NEW"], sum)
  reduce_size <- tapply(real$size[real$event_type != "NEW"], real$order_id[real$event_type != "NEW"], sum)
  common_ids <- intersect(names(new_size), names(reduce_size))
  balance <- new_size[common_ids] - reduce_size[common_ids]
  print(summary(balance))
  print(paste("Orders exactly balanced (fully removed/filled, balance == 0):", sum(abs(balance) < 1e-9)))
  print(paste("Orders where removed size exceeds NEW size (balance < 0, would indicate a modeling error):", sum(balance < -1e-9)))
  print(paste("Orders with leftover size (balance > 0, still partially resting at end of sample):", sum(balance > 1e-9)))

  cancel_count_per_order <- tapply(real$event_type, real$order_id, function(x) sum(x == "CANCEL"))
  print("Distribution of CANCEL count per order_id (0 = never canceled, does CANCEL repeat before DELETE?):")
  print(table(cancel_count_per_order))

  exec_count_per_order <- tapply(real$event_type, real$order_id, function(x) sum(x == "EXECUTE_VISIBLE"))
  print("Distribution of EXECUTE_VISIBLE count per order_id (do orders get partially filled multiple times?):")
  print(table(exec_count_per_order))

  invisible(real)
}

if (file.exists("data/processed/lobster_normalized.csv")) {
  invisible(load_and_profile_lobster("data/processed/lobster_normalized.csv"))
} else {
  print("No processed LOBSTER dataset found at data/processed/lobster_normalized.csv")
}

if (file.exists("data/processed/lobster_with_book.csv")) {
  invisible(load_and_profile_lobster_book("data/processed/lobster_with_book.csv"))
} else {
  print("No processed LOBSTER+book dataset found at data/processed/lobster_with_book.csv")
}

if (file.exists("data/processed/lobster_normalized.csv")) {
  invisible(analyze_order_lifecycles("data/processed/lobster_normalized.csv"))
} else {
  print("No processed LOBSTER dataset found at data/processed/lobster_normalized.csv")
}
