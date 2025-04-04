#' Process Human AMR Data
#'
#' Process the raw human antimicrobial resistance data and convert it to a structured format.
#'
#' @param file_path Path to the raw human AMR data CSV file
#' @return A data frame with processed human AMR data
#' @export
#' @examples
#' \dontrun{
#' human_data <- process_human_data("data-raw/human_data_raw.csv")
#' }
process_human_data <- function(file_path) {
  # Read the raw data
  raw_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Create an empty list to store processed data
  processed_data <- list()
  
  # Process the data for each study
  for (i in 1:nrow(raw_data)) {
    study_row <- raw_data[i, ]
    
    # Extract study metadata
    study_id <- study_row$study.ID
    first_author <- study_row$firstauthor_last
    year_published <- study_row$year_published
    location <- study_row$location
    study_type <- study_row$study_type
    environment <- study_row$Environment
    population <- study_row$population
    sample_size <- study_row$n_pop
    age_range <- study_row$age_range
    
    # Get all antibiotics and pathogen combinations
    # Extract columns that have 'r_', 'n_', and 'd_' prefixes
    r_cols <- grep("^r_", colnames(study_row), value = TRUE)
    n_cols <- grep("^n_", colnames(study_row), value = TRUE)
    d_cols <- grep("^d_", colnames(study_row), value = TRUE)
    
    # Process each antibiotic-pathogen pair
    for (r_col in r_cols) {
      # Extract antibiotic and pathogen
      parts <- strsplit(gsub("^r_", "", r_col), "_")[[1]]
      if (length(parts) != 2) next
      
      antibiotic <- parts[1]
      pathogen <- parts[2]
      
      # Corresponding n_ and d_ columns
      n_col <- paste0("n_", antibiotic, "_", pathogen)
      d_col <- paste0("d_", antibiotic, "_", pathogen)
      
      # Skip if any values are NA
      if (is.na(study_row[[r_col]]) || is.na(study_row[[n_col]]) || is.na(study_row[[d_col]])) {
        next
      }
      
      # Extract resistance rate, sample size, and resistant count
      resistance_rate <- as.numeric(study_row[[r_col]])
      sample_size_ab <- as.numeric(study_row[[n_col]])
      resistant_count <- as.numeric(study_row[[d_col]])
      
      # Skip if any values are NA after conversion
      if (is.na(resistance_rate) || is.na(sample_size_ab) || is.na(resistant_count)) {
        next
      }
      
      # Create a record
      record <- data.frame(
        record_id = paste0(study_id, "_", antibiotic, "_", pathogen),
        study_id = study_id,
        first_author = first_author,
        year_published = year_published,
        location = location,
        study_type = study_type,
        environment = environment,
        population = population,
        sample_size = sample_size,
        age_range = age_range,
        pathogen = pathogen,
        antibiotic = antibiotic,
        resistance_rate = resistance_rate,
        sample_size_ab = sample_size_ab,
        resistant_count = resistant_count,
        stringsAsFactors = FALSE
      )
      
      # Add record to processed data
      processed_data[[length(processed_data) + 1]] <- record
    }
  }
  
  # Combine all records
  if (length(processed_data) > 0) {
    result <- do.call(rbind, processed_data)
    rownames(result) <- NULL
    return(result)
  } else {
    return(data.frame())
  }
}

#' Prepare Human AMR Data for Package
#'
#' Prepare the human AMR data for inclusion in the package
#'
#' @param input_file Path to the raw human AMR data CSV file
#' @param output_file Path to save the processed RDS file
#' @return A data frame with processed human AMR data
#' @export
#' @examples
#' \dontrun{
#' prepare_human_data("data-raw/human_data_raw.csv", "data/human_amr.rds")
#' }
prepare_human_data <- function(input_file, output_file) {
  # Process the data
  processed_data <- process_human_data(input_file)
  
  # Save as RDS
  saveRDS(processed_data, output_file)
  
  return(processed_data)
}

#' Create Human AMR Data Documentation
#'
#' Generate roxygen2 documentation for human AMR data
#'
#' @param data_path Path to the human AMR data RDS file
#' @param output_file Path to save the documentation R file
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' create_human_data_docs("data/human_amr.rds", "R/data/human_amr.R")
#' }
create_human_data_docs <- function(data_path, output_file) {
  # Load the data
  data <- readRDS(data_path)
  
  # Create documentation
  doc <- c(
    "#' Human Antimicrobial Resistance Data",
    "#'",
    "#' A dataset containing antimicrobial resistance rates from human studies",
    "#'",
    "#' @format A data frame with variables:",
    "#' \\describe{",
    "#'   \\item{record_id}{Unique identifier for each record}",
    "#'   \\item{study_id}{Study identifier}",
    "#'   \\item{first_author}{Last name of the first author}",
    "#'   \\item{year_published}{Year of publication}",
    "#'   \\item{location}{Country or region of the study}",
    "#'   \\item{study_type}{Type of study (e.g., Cross-sectional, Cohort Study)}",
    "#'   \\item{environment}{Study environment (e.g., Rural Area, Urban Area, Hospital)}",
    "#'   \\item{population}{Population studied (e.g., Adults, Children)}",
    "#'   \\item{sample_size}{Total sample size of the study}",
    "#'   \\item{age_range}{Age range of participants}",
    "#'   \\item{pathogen}{Pathogen studied (e.g., Ecoil, KP, SA, SP)}",
    "#'   \\item{antibiotic}{Antibiotic tested (e.g., AMP, CIP, TET)}",
    "#'   \\item{resistance_rate}{Proportion of resistant isolates}",
    "#'   \\item{sample_size_ab}{Sample size for the specific antibiotic-pathogen pair}",
    "#'   \\item{resistant_count}{Number of resistant isolates}",
    "#' }",
    "#' @source Various published studies on antimicrobial resistance",
    "\"human_amr\"",
    ""
  )
  
  # Write to file
  writeLines(doc, output_file)
} 