# Global settings and functions for the METEOR Shiny application

# Load required packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(meteor)

# Check for required packages
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "ggplot2", "dplyr", "tidyr", "meta")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("The following required packages are missing: ", paste(missing_packages, collapse = ", "), 
       "\nPlease install them with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
}

# Function to check if a dataset is valid
check_dataset <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

# Function to generate meta-analysis summary
generate_meta_summary <- function(meta_result) {
  if (is.null(meta_result)) {
    return("No meta-analysis results available.")
  }
  
  # Extract information from meta object
  if (inherits(meta_result, "meta")) {
    meta_obj <- meta_result
  } else if (inherits(meta_result, "list") && !is.null(meta_result$overall) && 
             inherits(meta_result$overall, "meta")) {
    meta_obj <- meta_result$overall
  } else {
    return("Invalid meta-analysis result format.")
  }
  
  # Generate summary text
  summary_text <- paste0(
    "Meta-Analysis Summary\n",
    "--------------------\n",
    "Number of studies: ", meta_obj$k, "\n",
    "Total sample size: ", sum(meta_obj$n), "\n",
    "\n",
    "Random effects model:\n",
    "Pooled estimate: ", round(meta_obj$TE.random * 100, 2), "% (", 
    round(meta_obj$lower.random * 100, 2), "% - ", 
    round(meta_obj$upper.random * 100, 2), "%)\n",
    "\n",
    "Heterogeneity:\n",
    "I² = ", round(meta_obj$I2, 1), "%, ",
    "tau² = ", round(meta_obj$tau2, 4), ", ",
    "p ", ifelse(meta_obj$pval.Q < 0.001, "< 0.001", paste("=", round(meta_obj$pval.Q, 3))),
    "\n"
  )
  
  return(summary_text)
}

# Define color palettes for visualizations
color_palettes <- list(
  viridis = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
  reds = c("#FEE5D9", "#FCBBA1", "#FC9272", "#FB6A4A", "#DE2D26", "#A50F15"),
  blues = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C"),
  greens = c("#EDF8E9", "#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#006D2C")
)

# Set default options
options(
  meteor.default_meta_method = "REML",
  meteor.confidence_level = 0.95,
  meteor.color_palette = "viridis",
  meteor.na_handling = "omit",
  meteor.significant_digits = 3,
  meteor.show_warnings = TRUE
) 