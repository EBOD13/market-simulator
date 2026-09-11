# R/data_loader.R
# Load data to be parsed and processed for analysis. This function reads in the raw data files, performs necessary cleaning and transformations, and returns a structured data frame ready for further analysis.

load_market_data <- function(file_path) {
    read.csv(file_path)
}