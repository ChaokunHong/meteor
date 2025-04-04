# Script to prepare human AMR data for inclusion in the meteor package

# Load necessary libraries
library(readr)
library(dplyr)

# Read the raw data file
human_data_raw <- read_csv("../human_data_raw.csv")

# Basic cleaning
human_data <- human_data_raw %>%
  # Convert column names to lowercase for consistency
  rename_with(tolower) %>%
  # Filter out rows with missing critical data
  filter(!is.na(`study id`), !is.na(firstauthor_last), !is.na(year_published), !is.na(location))

# Create metadata
attr(human_data, "domain") <- "human"
attr(human_data, "description") <- "Human antimicrobial resistance data collected from published studies"
attr(human_data, "source") <- "Published literature"
attr(human_data, "date_processed") <- Sys.Date()

# Save the data to be included in the package
usethis::use_data(human_data, overwrite = TRUE)

# Create a standardized version for easier use
# First validate the data
validated_human_data <- meteor::validate_data(human_data, domain = "human", strict = FALSE)

# Then standardize it
standardized_human_data <- meteor::standardize_amr_data(validated_human_data, domain = "human")

# Save the standardized data
usethis::use_data(standardized_human_data, overwrite = TRUE)

# Print summary of processed data
cat("Human data processed and saved:\n")
cat("Raw data:", nrow(human_data), "rows,", ncol(human_data), "columns\n")
cat("Standardized data:", nrow(standardized_human_data), "rows,", ncol(standardized_human_data), "columns\n") 