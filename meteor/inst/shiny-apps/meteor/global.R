# Global script for the meteor Shiny application

# Load required packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(meteor)
library(scales)

# Load functions (will be available in the final package)
if (!exists("meteor_version")) {
  # Define package functions if running standalone
  meteor_version <- function() {
    return("0.1.0 (Development)")
  }
  
  # If human_amr dataset doesn't exist, create sample data
  if (!exists("human_amr")) {
    set.seed(123)
    
    # Create sample data
    sample_data <- expand.grid(
      pathogen = c("Ecoil", "KP", "SA", "SP"),
      antibiotic = c("CIP", "TET", "AMP", "GEN", "SXT"),
      location = c("India", "Bangladesh", "Pakistan", "Nepal", "Sri Lanka")
    )
    
    # Add metadata
    sample_data$study_id <- paste0("S", sample(1:20, nrow(sample_data), replace = TRUE))
    sample_data$first_author <- sample(c("Smith", "Jones", "Kumar", "Patel", "Wang"), nrow(sample_data), replace = TRUE)
    sample_data$year_published <- sample(2010:2022, nrow(sample_data), replace = TRUE)
    sample_data$study_type <- sample(c("Cross-sectional", "Case-control", "Cohort Study"), nrow(sample_data), replace = TRUE)
    sample_data$environment <- sample(c("Hospital", "Urban Area", "Rural Area"), nrow(sample_data), replace = TRUE)
    sample_data$population <- sample(c("Adults", "Children", "Whole age"), nrow(sample_data), replace = TRUE)
    
    # Add resistance data
    sample_data$resistance_rate <- runif(nrow(sample_data), 0, 1)
    sample_data$sample_size_ab <- sample(30:200, nrow(sample_data), replace = TRUE)
    sample_data$resistant_count <- round(sample_data$resistance_rate * sample_data$sample_size_ab)
    
    # Add record ID
    sample_data$record_id <- paste0(sample_data$study_id, "_", sample_data$antibiotic, "_", sample_data$pathogen)
    
    # Set as human_amr dataset
    human_amr <- sample_data
  }
} 