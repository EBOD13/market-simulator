# R/types.R

# Define the types we are working with for our synthetic data simulation. These types are used to generate synthetic data for testing and validation purposes.

order_events <- factor(
  c("NEW", "CANCEL", "MODIFY"),
  levels = c("NEW", "CANCEL", "MODIFY")
)

order_sides <- factor(
  c("BUY", "SELL"),
  levels = c("BUY", "SELL")
)

order_types <- factor(
  c("LIMIT", "MARKET"),
  levels = c("LIMIT", "MARKET")
)