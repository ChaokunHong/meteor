#' Import AMR data
#'
#' Import antimicrobial resistance data from various file formats
#'
#' @param file_path Path to the data file
#' @param format Format of the data file. If NULL, will be guessed from file extension
#' @param ... Additional arguments passed to the respective read function
#' @return A data frame containing the imported data
#' @export
#' @examples
#' \dontrun{
#' data <- import_amr_data("path/to/amr_data.csv")
#' }
import_amr_data <- function(file_path, format = NULL, ...) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  # Determine format from file extension if not provided
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file_path))
    format <- switch(ext,
                   csv = "csv",
                   tsv = "tsv",
                   txt = "txt",
                   xlsx = "excel",
                   xls = "excel",
                   rds = "rds",
                   rdata = "rdata",
                   stop("Unknown file extension: ", ext, ". Please specify format explicitly."))
  }
  
  # Import based on format
  data <- switch(tolower(format),
                csv = readr::read_csv(file_path, ...),
                tsv = readr::read_tsv(file_path, ...),
                txt = readr::read_delim(file_path, ...),
                excel = readxl::read_excel(file_path, ...),
                rds = readRDS(file_path),
                rdata = {
                  tmp_env <- new.env()
                  load(file_path, envir = tmp_env)
                  if (length(tmp_env) == 1) {
                    tmp_env[[ls(tmp_env)[1]]]
                  } else {
                    warning("Multiple objects in RData file. Returning list of all objects.")
                    as.list(tmp_env)
                  }
                },
                stop("Unsupported format: ", format))
  
  return(data)
}

#' Import local researcher data
#'
#' Import and standardize a researcher's local antimicrobial resistance data
#'
#' @param file_path Path to the data file
#' @param format Format of the data file
#' @param mapping A named list mapping the dataset's column names to standard meteor column names
#' @param ... Additional arguments passed to import_amr_data
#' @return A standardized data frame
#' @export
#' @examples
#' \dontrun{
#' mapping <- list(
#'   site = "location", 
#'   organism = "pathogen", 
#'   drug = "antibiotic",
#'   percentage = "resistance_rate"
#' )
#' local_data <- import_local_data("local_data.csv", mapping = mapping)
#' }
import_local_data <- function(file_path, format = NULL, mapping = NULL, ...) {
  # Import the data
  data <- import_amr_data(file_path, format, ...)
  
  # Apply column mapping if provided
  if (!is.null(mapping)) {
    # Check if all mapping keys exist in the dataset
    missing_cols <- setdiff(names(mapping), colnames(data))
    if (length(missing_cols) > 0) {
      stop("The following columns in the mapping are not present in the data: ", 
           paste(missing_cols, collapse = ", "))
    }
    
    # Rename columns based on mapping
    for (old_name in names(mapping)) {
      new_name <- mapping[[old_name]]
      colnames(data)[colnames(data) == old_name] <- new_name
    }
  }
  
  # Return the standardized data
  return(data)
}

#' Validate AMR data
#'
#' Check the integrity and completeness of AMR data
#'
#' @param data A data frame containing AMR data
#' @param required_columns Vector of column names that must be present
#' @param validate_values Whether to check for valid values in key columns
#' @return A list with validation results and the data (potentially with warnings/flags added)
#' @export
#' @examples
#' \dontrun{
#' data <- import_amr_data("amr_data.csv")
#' validation <- validate_data(data)
#' if (validation$valid) {
#'   # Proceed with analysis
#' } else {
#'   # Handle validation issues
#'   print(validation$issues)
#' }
#' }
validate_data <- function(data, 
                         required_columns = c("pathogen", "antibiotic", "resistance_rate"),
                         validate_values = TRUE) {
  # Initialize results
  validation <- list(
    valid = TRUE,
    issues = list(),
    data = data
  )
  
  # Check data is a data frame
  if (!is.data.frame(data)) {
    validation$valid <- FALSE
    validation$issues$wrong_type <- "Input is not a data frame"
    return(validation)
  }
  
  # Check required columns
  missing_cols <- setdiff(required_columns, colnames(data))
  if (length(missing_cols) > 0) {
    validation$valid <- FALSE
    validation$issues$missing_columns <- missing_cols
  }
  
  # If missing required columns, return early
  if (!validation$valid) {
    return(validation)
  }
  
  # Check for NAs in key columns
  for (col in required_columns) {
    na_count <- sum(is.na(data[[col]]))
    if (na_count > 0) {
      validation$issues$na_values <- c(validation$issues$na_values, 
                                      setNames(na_count, col))
      validation$valid <- FALSE
    }
  }
  
  # Validate resistance rate values if present
  if ("resistance_rate" %in% colnames(data) && validate_values) {
    # Check for values outside the valid range [0, 1]
    invalid_rates <- data$resistance_rate < 0 | data$resistance_rate > 1
    if (any(invalid_rates, na.rm = TRUE)) {
      validation$issues$invalid_rates <- which(invalid_rates)
      validation$valid <- FALSE
    }
  }
  
  # Add warning flags to the data
  if (!validation$valid && nrow(data) > 0) {
    data$validation_warning <- FALSE
    
    # Flag rows with invalid resistance rates if present
    if (!is.null(validation$issues$invalid_rates)) {
      data$validation_warning[validation$issues$invalid_rates] <- TRUE
    }
    
    validation$data <- data
  }
  
  return(validation)
}

#' Standardize AMR data
#'
#' Standardize column names, values, and structure of AMR data
#'
#' @param data A data frame containing AMR data
#' @param pathogen_map A named list mapping pathogen values to standard names
#' @param antibiotic_map A named list mapping antibiotic values to standard names
#' @return A standardized data frame
#' @export
#' @examples
#' \dontrun{
#' data <- import_amr_data("amr_data.csv")
#' 
#' # Define mapping for non-standard pathogen names
#' pathogen_map <- list(
#'   "E. coli" = "Ecoil",
#'   "Escherichia coli" = "Ecoil",
#'   "K. pneumoniae" = "KP",
#'   "Klebsiella pneumoniae" = "KP"
#' )
#' 
#' standardized <- standardize_amr_data(data, pathogen_map = pathogen_map)
#' }
standardize_amr_data <- function(data, pathogen_map = NULL, antibiotic_map = NULL) {
  # Make a copy of the data
  std_data <- data
  
  # Standardize pathogen names if map is provided
  if (!is.null(pathogen_map) && "pathogen" %in% colnames(std_data)) {
    for (original in names(pathogen_map)) {
      std_data$pathogen[std_data$pathogen == original] <- pathogen_map[[original]]
    }
  }
  
  # Standardize antibiotic names if map is provided
  if (!is.null(antibiotic_map) && "antibiotic" %in% colnames(std_data)) {
    for (original in names(antibiotic_map)) {
      std_data$antibiotic[std_data$antibiotic == original] <- antibiotic_map[[original]]
    }
  }
  
  # Ensure resistance_rate is numeric between 0 and 1
  if ("resistance_rate" %in% colnames(std_data)) {
    std_data$resistance_rate <- as.numeric(std_data$resistance_rate)
    
    # Clip values to [0, 1] range
    std_data$resistance_rate[std_data$resistance_rate < 0] <- 0
    std_data$resistance_rate[std_data$resistance_rate > 1] <- 1
  }
  
  # Ensure year is numeric
  if ("year_published" %in% colnames(std_data)) {
    std_data$year_published <- as.numeric(std_data$year_published)
  }
  
  return(std_data)
}

#' Filter AMR data
#'
#' Filter AMR data based on various criteria
#'
#' @param data A data frame containing AMR data
#' @param pathogen Vector of pathogen values to include
#' @param antibiotic Vector of antibiotic values to include
#' @param location Vector of location values to include
#' @param year_range Vector of two years defining the range to include
#' @param population Vector of population values to include
#' @param ... Additional filter criteria specified as named arguments
#' @return A filtered data frame
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' 
#' # Filter to specific pathogens and antibiotics in a date range
#' filtered <- filter_amr_data(
#'   human_amr,
#'   pathogen = c("Ecoil", "KP"),
#'   antibiotic = c("CIP", "TET"),
#'   year_range = c(2010, 2020)
#' )
#' }
filter_amr_data <- function(data, 
                          pathogen = NULL, 
                          antibiotic = NULL,
                          location = NULL,
                          year_range = NULL,
                          population = NULL,
                          ...) {
  # Start with all data
  result <- data
  
  # Apply pathogen filter
  if (!is.null(pathogen) && "pathogen" %in% colnames(result)) {
    result <- result[result$pathogen %in% pathogen, ]
  }
  
  # Apply antibiotic filter
  if (!is.null(antibiotic) && "antibiotic" %in% colnames(result)) {
    result <- result[result$antibiotic %in% antibiotic, ]
  }
  
  # Apply location filter
  if (!is.null(location) && "location" %in% colnames(result)) {
    result <- result[result$location %in% location, ]
  }
  
  # Apply year range filter
  if (!is.null(year_range) && length(year_range) == 2 && "year_published" %in% colnames(result)) {
    result <- result[result$year_published >= year_range[1] & 
                    result$year_published <= year_range[2], ]
  }
  
  # Apply population filter
  if (!is.null(population) && "population" %in% colnames(result)) {
    result <- result[result$population %in% population, ]
  }
  
  # Apply additional filters passed as named arguments
  extra_args <- list(...)
  for (arg_name in names(extra_args)) {
    if (arg_name %in% colnames(result)) {
      filter_val <- extra_args[[arg_name]]
      if (is.atomic(filter_val)) {
        result <- result[result[[arg_name]] %in% filter_val, ]
      }
    }
  }
  
  return(result)
}

#' Summarize AMR dataset
#'
#' Create a summary of an AMR dataset
#'
#' @param data A data frame containing AMR data
#' @param by Variables to group by for summary statistics
#' @return A list containing various summary statistics
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' summary <- summarize_dataset(human_amr, by = c("pathogen", "antibiotic"))
#' }
summarize_dataset <- function(data, by = NULL) {
  # Initialize summary list
  summary <- list()
  
  # Overall dataset properties
  summary$n_records <- nrow(data)
  
  if (nrow(data) == 0) {
    warning("Empty dataset")
    return(summary)
  }
  
  # Time span
  if ("year_published" %in% colnames(data)) {
    summary$year_range <- range(data$year_published, na.rm = TRUE)
    summary$year_counts <- table(data$year_published)
  }
  
  # Geographic distribution
  if ("location" %in% colnames(data)) {
    summary$locations <- sort(table(data$location), decreasing = TRUE)
  }
  
  # Pathogen distribution
  if ("pathogen" %in% colnames(data)) {
    summary$pathogens <- sort(table(data$pathogen), decreasing = TRUE)
  }
  
  # Antibiotic distribution
  if ("antibiotic" %in% colnames(data)) {
    summary$antibiotics <- sort(table(data$antibiotic), decreasing = TRUE)
  }
  
  # Study types
  if ("study_type" %in% colnames(data)) {
    summary$study_types <- sort(table(data$study_type), decreasing = TRUE)
  }
  
  # Population types
  if ("population" %in% colnames(data)) {
    summary$populations <- sort(table(data$population), decreasing = TRUE)
  }
  
  # Resistance rate summary
  if ("resistance_rate" %in% colnames(data)) {
    summary$resistance_rate <- list(
      mean = mean(data$resistance_rate, na.rm = TRUE),
      median = median(data$resistance_rate, na.rm = TRUE),
      range = range(data$resistance_rate, na.rm = TRUE),
      quantiles = quantile(data$resistance_rate, 
                         probs = c(0, 0.25, 0.5, 0.75, 1), 
                         na.rm = TRUE)
    )
  }
  
  # Group-specific summaries if requested
  if (!is.null(by) && all(by %in% colnames(data))) {
    # Create combinations of grouping variables
    grp_formula <- as.formula(paste("~", paste(by, collapse = "+")))
    groups <- aggregate(data$resistance_rate, grp_formula, FUN = mean, data = data)
    colnames(groups)[ncol(groups)] <- "mean_resistance_rate"
    
    # Add sample sizes
    counts <- aggregate(data$resistance_rate, grp_formula, FUN = length, data = data)
    colnames(counts)[ncol(counts)] <- "sample_size"
    
    # Merge results
    summary$by_group <- merge(groups, counts)
    
    # Sort by mean resistance rate (descending)
    summary$by_group <- summary$by_group[order(summary$by_group$mean_resistance_rate, 
                                            decreasing = TRUE), ]
  }
  
  return(summary)
}

#' Merge domains
#'
#' Merge data from different domains (human, animal, environment)
#'
#' @param ... Data frames from different domains, either as named arguments or a list
#' @param add_domain_col Whether to add a 'domain' column to identify the source
#' @return A merged data frame
#' @export
#' @examples
#' \dontrun{
#' # Merge with individual data frames
#' merged <- merge_domains(human = human_data, animal = animal_data)
#' 
#' # Or with a list
#' domains <- list(human = human_data, animal = animal_data)
#' merged <- merge_domains(domains)
#' }
merge_domains <- function(..., add_domain_col = TRUE) {
  # Get the arguments
  args <- list(...)
  
  # If the first argument is a list and it's the only argument, use that list
  if (length(args) == 1 && is.list(args[[1]]) && !is.data.frame(args[[1]])) {
    args <- args[[1]]
  }
  
  # Check if there are any data frames to merge
  if (length(args) == 0) {
    stop("No data frames provided to merge")
  }
  
  # Initialize an empty result list
  merged_list <- list()
  
  # Process each data frame
  for (domain_name in names(args)) {
    domain_data <- args[[domain_name]]
    
    # Ensure it's a data frame
    if (!is.data.frame(domain_data)) {
      warning("Skipping non-data-frame for domain: ", domain_name)
      next
    }
    
    # Make a copy of the data
    domain_copy <- domain_data
    
    # Add domain column if requested
    if (add_domain_col) {
      domain_copy$domain <- domain_name
    }
    
    # Add to the result list
    merged_list[[length(merged_list) + 1]] <- domain_copy
  }
  
  # Combine all data frames
  if (length(merged_list) == 0) {
    stop("No valid data frames to merge")
  } else if (length(merged_list) == 1) {
    result <- merged_list[[1]]
  } else {
    # Identify common columns for binding
    common_cols <- Reduce(intersect, lapply(merged_list, colnames))
    
    if (length(common_cols) == 0) {
      stop("No common columns found among the domains")
    }
    
    # Subset to common columns and bind rows
    merged_list <- lapply(merged_list, function(df) df[, common_cols, drop = FALSE])
    result <- do.call(rbind, merged_list)
  }
  
  return(result)
} 