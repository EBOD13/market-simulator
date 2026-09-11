# R/types.R
#
# Canonical interchange schema shared between this repo and Leka: one row
# per order-book event, fully self-contained (side/type/price/quantity are
# always present, not just the fields that changed), so a consumer can
# process each row independently without replaying prior state.
#
# Leka's event model: NEW / CANCEL / REDUCE. There is no MODIFY. CANCEL
# removes a resting order entirely; REDUCE (Leka's ReduceOrder{orderId,
# newQuantity}) shrinks a resting order in place and preserves queue
# priority; a reprice or size increase is CANCEL followed by NEW.

library(bit64)

order_events <- factor(
  c("NEW", "CANCEL", "REDUCE"),
  levels = c("NEW", "CANCEL", "REDUCE")
)

order_sides <- factor(
  c("BUY", "SELL"),
  levels = c("BUY", "SELL")
)

order_types <- factor(
  c("LIMIT", "MARKET"),
  levels = c("LIMIT", "MARKET")
)

# Column order for the interchange CSV. price is an integer in 1/10000
# units (matches LOBSTER's own fixed-point encoding); ts_ns is nanoseconds
# as bit64::integer64, since a nanosecond epoch timestamp overflows the
# ~2^53 exact-integer range of a double.
interchange_columns <- c(
  "seq", "ts_ns", "event", "order_id", "side", "type", "price", "quantity"
)

# TRUE where x is a non-missing whole number (within floating-point tolerance).
is_whole_number <- function(x, tol = 1e-8) {
  x_num <- suppressWarnings(as.numeric(x))
  !is.na(x_num) & abs(x_num - round(x_num)) < tol
}

# Validate a data frame against the interchange schema. Returns TRUE
# (invisibly) on success; stops with a description of every violation found
# on failure, so a caller sees the full list of problems in one pass.
validate_interchange <- function(df) {
  if (!is.data.frame(df)) {
    stop("validate_interchange() requires a data.frame.")
  }

  missing_cols <- setdiff(interchange_columns, names(df))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Interchange schema violation: missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  n <- nrow(df)
  errors <- character(0)

  # seq: non-missing whole numbers, strictly increasing.
  if (any(is.na(df$seq)) || !all(is_whole_number(df$seq))) {
    errors <- c(errors, "seq must contain non-missing whole numbers.")
  } else if (n > 1 && any(diff(as.numeric(df$seq)) <= 0)) {
    errors <- c(errors, "seq must be strictly increasing.")
  }

  # ts_ns: bit64 integer64 (a double cannot hold a nanosecond epoch
  # timestamp exactly), non-missing, non-decreasing. Comparisons use
  # bit64's own operators to avoid the precision loss of as.numeric().
  if (!inherits(df$ts_ns, "integer64")) {
    errors <- c(errors, "ts_ns must be a bit64::integer64 vector.")
  } else if (any(is.na(df$ts_ns))) {
    errors <- c(errors, "ts_ns must not contain missing values.")
  } else if (n > 1 && any(df$ts_ns[-1] < df$ts_ns[-n])) {
    errors <- c(errors, "ts_ns must be non-decreasing (events in time order).")
  }

  # event
  if (any(is.na(df$event)) || !all(as.character(df$event) %in% levels(order_events))) {
    errors <- c(errors, paste0(
      "event must be one of: ", paste(levels(order_events), collapse = ", ")
    ))
  }

  # order_id: non-missing, non-negative whole numbers.
  if (any(is.na(df$order_id)) || !all(is_whole_number(df$order_id)) ||
      any(as.numeric(df$order_id) < 0)) {
    errors <- c(errors, "order_id must contain non-missing, non-negative whole numbers.")
  }

  # side
  if (any(is.na(df$side)) || !all(as.character(df$side) %in% levels(order_sides))) {
    errors <- c(errors, paste0(
      "side must be one of: ", paste(levels(order_sides), collapse = ", ")
    ))
  }

  # type
  if (any(is.na(df$type)) || !all(as.character(df$type) %in% levels(order_types))) {
    errors <- c(errors, paste0(
      "type must be one of: ", paste(levels(order_types), collapse = ", ")
    ))
  }

  # price: non-negative integer (1/10000 units) for LIMIT orders. MARKET
  # orders carry no limit price, so NA is allowed there; a non-NA price is
  # still validated if present.
  price_num <- suppressWarnings(as.numeric(df$price))
  type_chr <- as.character(df$type)
  price_present_and_invalid <- !is.na(price_num) & !is_whole_number(price_num)
  price_missing_for_limit <- type_chr == "LIMIT" & is.na(price_num)
  price_negative <- !is.na(price_num) & price_num < 0
  if (any(price_present_and_invalid | price_missing_for_limit | price_negative)) {
    errors <- c(errors, paste0(
      "price must be a non-negative integer (1/10000 units) for LIMIT orders; ",
      "MARKET orders may omit it (NA)."
    ))
  }

  # quantity: non-missing, non-negative whole numbers.
  if (any(is.na(df$quantity)) || !all(is_whole_number(df$quantity)) ||
      any(as.numeric(df$quantity) < 0)) {
    errors <- c(errors, "quantity must contain non-missing, non-negative whole numbers.")
  }

  if (length(errors) > 0) {
    stop(paste0(
      "Interchange schema violation(s):\n  - ",
      paste(errors, collapse = "\n  - ")
    ))
  }

  invisible(TRUE)
}

# Validate and write a data frame as the interchange CSV. Every column is
# formatted explicitly (rather than relying on write.csv's defaults) so
# large integers and ts_ns never round-trip through scientific notation.
write_interchange_csv <- function(df, file_path) {
  validate_interchange(df)

  out <- data.frame(
    seq = format(as.numeric(df$seq), scientific = FALSE, trim = TRUE),
    ts_ns = trimws(format(df$ts_ns, scientific = FALSE)),
    event = as.character(df$event),
    order_id = format(as.numeric(df$order_id), scientific = FALSE, trim = TRUE),
    side = as.character(df$side),
    type = as.character(df$type),
    price = ifelse(
      is.na(df$price), "",
      format(as.numeric(df$price), scientific = FALSE, trim = TRUE)
    ),
    quantity = format(as.numeric(df$quantity), scientific = FALSE, trim = TRUE),
    stringsAsFactors = FALSE
  )
  names(out) <- interchange_columns

  write.csv(out, file_path, row.names = FALSE, quote = FALSE)
  invisible(file_path)
}
