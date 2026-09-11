# tests/test_types.R
# Tests for the R/types.R interchange schema (validator + writer).
# Run from the repository root, e.g. `Rscript tests/test_types.R`, matching
# the other scripts in this repo (they assume cwd == repo root).

source("R/types.R", encoding = "UTF-8")

library(bit64)

failures <- character(0)

expect_error <- function(label, expr) {
  ok <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  if (!ok) {
    message(paste0("FAIL (expected an error but got none): ", label))
    failures <<- c(failures, label)
  }
}

expect_ok <- function(label, expr) {
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) {
    message(paste0("FAIL (unexpected error): ", label, " -> ", conditionMessage(e)))
    FALSE
  })
  if (!ok) failures <<- c(failures, label)
}

expect_equal <- function(label, actual, expected) {
  if (!identical(actual, expected)) {
    message(paste0(
      "FAIL (not equal): ", label,
      " -> got ", paste(as.character(actual), collapse = ","),
      ", expected ", paste(as.character(expected), collapse = ",")
    ))
    failures <<- c(failures, label)
  }
}

# A large nanosecond epoch timestamp deliberately exceeds a double's exact
# integer range (~2^53), so any silent fallback to numeric would corrupt it.
big_ts <- bit64::as.integer64(c(
  "1700000000000000001", "1700000000000000101",
  "1700000000000000101", "1700000000000000900"
))

make_valid_df <- function() {
  data.frame(
    seq = 1:4,
    ts_ns = big_ts,
    event = c("NEW", "REDUCE", "CANCEL", "NEW"),
    order_id = c(101, 101, 101, 102),
    side = c("BUY", "BUY", "BUY", "SELL"),
    type = c("LIMIT", "LIMIT", "LIMIT", "MARKET"),
    price = c(1000000, 1000000, 1000000, NA),
    quantity = c(100, 40, 0, 50),
    stringsAsFactors = FALSE
  )
}

# --- validate_interchange: happy path ---------------------------------

expect_ok("valid data frame passes validation", validate_interchange(make_valid_df()))

# --- validate_interchange: structural violations -----------------------

expect_error("missing column is rejected", {
  df <- make_valid_df()
  df$seq <- NULL
  validate_interchange(df)
})

expect_error("non-data-frame input is rejected", validate_interchange(list(a = 1)))

# --- validate_interchange: categorical columns --------------------------

expect_error("MODIFY is no longer a valid event", {
  df <- make_valid_df()
  df$event[1] <- "MODIFY"
  validate_interchange(df)
})

expect_error("unknown side is rejected", {
  df <- make_valid_df()
  df$side[1] <- "B"
  validate_interchange(df)
})

expect_error("unknown type is rejected", {
  df <- make_valid_df()
  df$type[1] <- "STOP"
  validate_interchange(df)
})

expect_error("NA event is rejected", {
  df <- make_valid_df()
  df$event[1] <- NA
  validate_interchange(df)
})

# --- validate_interchange: seq ------------------------------------------

expect_error("non-increasing seq is rejected", {
  df <- make_valid_df()
  df$seq <- c(1, 1, 2, 3)
  validate_interchange(df)
})

expect_error("non-whole seq is rejected", {
  df <- make_valid_df()
  df$seq <- c(1, 2.5, 3, 4)
  validate_interchange(df)
})

# --- validate_interchange: ts_ns -----------------------------------------

expect_error("plain numeric ts_ns (not integer64) is rejected", {
  df <- make_valid_df()
  df$ts_ns <- suppressWarnings(as.numeric(df$ts_ns))
  validate_interchange(df)
})

expect_error("decreasing ts_ns is rejected", {
  df <- make_valid_df()
  df$ts_ns <- bit64::as.integer64(c(
    "1700000000000000900", "1700000000000000101",
    "1700000000000000101", "1700000000000000001"
  ))
  validate_interchange(df)
})

expect_ok("equal (tied) ts_ns values are allowed", {
  df <- make_valid_df()
  df$ts_ns <- bit64::as.integer64(rep("1700000000000000001", 4))
  validate_interchange(df)
})

# --- validate_interchange: price -----------------------------------------

expect_error("NA price on a LIMIT order is rejected", {
  df <- make_valid_df()
  df$price[1] <- NA
  validate_interchange(df)
})

expect_error("fractional price is rejected", {
  df <- make_valid_df()
  df$price[1] <- 1000000.5
  validate_interchange(df)
})

expect_error("negative price is rejected", {
  df <- make_valid_df()
  df$price[1] <- -1
  validate_interchange(df)
})

expect_ok("NA price on a MARKET order is allowed", {
  df <- make_valid_df()
  validate_interchange(df)
})

# --- validate_interchange: quantity, order_id -----------------------------

expect_error("negative quantity is rejected", {
  df <- make_valid_df()
  df$quantity[1] <- -5
  validate_interchange(df)
})

expect_error("NA order_id is rejected", {
  df <- make_valid_df()
  df$order_id[1] <- NA
  validate_interchange(df)
})

# --- write_interchange_csv: validates before writing ----------------------

expect_error("writer refuses an invalid data frame", {
  df <- make_valid_df()
  df$event[1] <- "MODIFY"
  write_interchange_csv(df, tempfile(fileext = ".csv"))
})

# --- write_interchange_csv: round-trip -------------------------------------

tmp_csv <- tempfile(fileext = ".csv")
df_out <- make_valid_df()
write_interchange_csv(df_out, tmp_csv)

round_tripped <- read.csv(tmp_csv, stringsAsFactors = FALSE, colClasses = "character")

expect_equal("round-trip: column order", names(round_tripped), interchange_columns)
expect_equal("round-trip: event values", round_tripped$event, as.character(df_out$event))
expect_equal("round-trip: side values", round_tripped$side, as.character(df_out$side))
expect_equal(
  "round-trip: ts_ns preserved at full 64-bit precision (no scientific notation)",
  bit64::as.integer64(round_tripped$ts_ns),
  df_out$ts_ns
)
expect_equal(
  "round-trip: price preserved, NA written as empty string for MARKET row",
  round_tripped$price,
  c("1000000", "1000000", "1000000", "")
)
expect_equal(
  "round-trip: quantity preserved as whole numbers",
  round_tripped$quantity,
  c("100", "40", "0", "50")
)

unlink(tmp_csv)

# --- report ------------------------------------------------------------

if (length(failures) > 0) {
  stop(paste0(length(failures), " test(s) failed:\n  - ", paste(failures, collapse = "\n  - ")))
}

print(paste("All", "tests passed."))
