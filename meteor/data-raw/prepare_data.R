# Script to prepare the human AMR data for inclusion in the meteor package

# Load required libraries
library(dplyr)
library(readr)

# Source data processing functions
source("R/data/data_processing.R")

# Create data-raw directory if it doesn't exist
if (!dir.exists("data-raw")) {
  dir.create("data-raw")
}

# Copy human_data_raw.csv to data-raw if it exists in the parent directory
if (file.exists("../human_data_raw.csv") && !file.exists("data-raw/human_data_raw.csv")) {
  file.copy("../human_data_raw.csv", "data-raw/human_data_raw.csv")
}

# Process the human data
human_amr <- process_human_data("data-raw/human_data_raw.csv")

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save the processed data as RDS
saveRDS(human_amr, "data/human_amr.rds")

# Create documentation
create_human_data_docs("data/human_amr.rds", "R/data/human_amr.R")

# Print summary
cat("Processed", nrow(human_amr), "records from human AMR data.\n")
cat("Saved to data/human_amr.rds\n")
cat("Documentation created at R/data/human_amr.R\n") 