# Global settings and functions for the METEOR Shiny application

# Load required packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Global options
options(stringsAsFactors = FALSE)

# Define any helper functions for the app
format_percentage <- function(x, digits = 1) {
  paste0(format(round(x * 100, digits), nsmall = digits), "%")
}

# Load example data if available
example_data <- NULL
try({
  data_path <- system.file("extdata", "example_amr_data.csv", package = "meteor")
  if (file.exists(data_path)) {
    example_data <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE)
  }
}, silent = TRUE) 