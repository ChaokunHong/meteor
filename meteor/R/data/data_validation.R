#' Validate AMR data
#'
#' Check if the imported data has the required structure and contains valid values
#'
#' @param data Data frame containing AMR data
#' @param domain Character string specifying the domain ("human", "animal", or "environment")
#' @param strict Logical; if TRUE, validation errors will stop execution; if FALSE, warnings will be issued
#'
#' @return The validated data frame (possibly with additional attributes)
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate imported data
#' validated_data <- validate_data(amr_data, domain = "human")
#' }
validate_data <- function(data, domain = c("human", "animal", "environment"), strict = FALSE) {
  domain <- match.arg(domain)
  
  if (!is.data.frame(data)) {
    if (strict) stop("Input must be a data frame") else warning("Input is not a data frame, converting")
    data <- as.data.frame(data)
  }
  
  # Check for empty data
  if (nrow(data) == 0) {
    if (strict) stop("Data contains 0 rows") else warning("Data contains 0 rows")
    return(data)
  }
  
  # Define required columns based on domain
  common_required <- c("study ID", "firstauthor_last", "year_published", "location")
  
  domain_required <- switch(domain,
                          human = c("population", "Environment", "n_pop"),
                          animal = c("animal_type", "sample_type"),
                          environment = c("environment_category", "sample_type"))
  
  required_cols <- c(common_required, domain_required)
  
  # Check if required columns exist
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    if (strict) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    } else {
      warning("Missing required columns: ", paste(missing_cols, collapse = ", "), 
              ". This may cause issues with analysis.")
    }
  }
  
  # Check for resistance rate data
  has_resistance_data <- FALSE
  resistance_pattern <- "^r_.*"
  if (any(grepl(resistance_pattern, names(data)))) {
    has_resistance_data <- TRUE
  }
  
  if (!has_resistance_data) {
    if (strict) {
      stop("No resistance data columns found (should match pattern 'r_*')")
    } else {
      warning("No resistance data columns found (should match pattern 'r_*')")
    }
  }
  
  # Check data types
  if ("year_published" %in% names(data)) {
    if (!is.numeric(data$year_published)) {
      if (strict) stop("year_published must be numeric") else warning("year_published is not numeric")
    } else {
      # Check year range
      invalid_years <- data$year_published < 1900 | data$year_published > as.integer(format(Sys.Date(), "%Y"))
      if (any(invalid_years, na.rm = TRUE)) {
        if (strict) {
          stop("Invalid years detected in year_published")
        } else {
          warning("Some year_published values are outside the valid range (1900-", 
                  format(Sys.Date(), "%Y"), ")")
        }
      }
    }
  }
  
  # Check for missing values in critical columns
  if (all(required_cols %in% names(data))) {
    na_counts <- sapply(data[required_cols], function(x) sum(is.na(x)))
    cols_with_na <- names(na_counts[na_counts > 0])
    
    if (length(cols_with_na) > 0) {
      msg <- paste("Missing values detected in critical columns:", 
                   paste(paste0(cols_with_na, " (", na_counts[cols_with_na], ")"), collapse = ", "))
      if (strict) stop(msg) else warning(msg)
    }
  }
  
  # Check resistance rate values (if present)
  resistance_cols <- grep(resistance_pattern, names(data), value = TRUE)
  for (col in resistance_cols) {
    if (is.numeric(data[[col]])) {
      invalid_rates <- data[[col]] < 0 | data[[col]] > 1
      if (any(invalid_rates, na.rm = TRUE)) {
        msg <- paste("Invalid resistance rates detected in", col, 
                     "(should be between 0 and 1)")
        if (strict) stop(msg) else warning(msg)
      }
    }
  }
  
  # Add validation information as attribute
  attr(data, "validated") <- TRUE
  attr(data, "validation_time") <- Sys.time()
  attr(data, "domain") <- domain
  
  return(data)
}

#' Check data quality
#'
#' Performs comprehensive quality checks on AMR data
#'
#' @param data Data frame containing AMR data
#' @param domain Character string specifying the domain
#' @param return_score Logical; if TRUE, returns a quality score
#'
#' @return If return_score is TRUE, returns a numeric quality score; otherwise, returns the data with quality attributes
#' @export
#'
#' @examples
#' \dontrun{
#' # Check data quality and get detailed report
#' quality_check <- check_data_quality(amr_data)
#' 
#' # Get only the quality score
#' quality_score <- check_data_quality(amr_data, return_score = TRUE)
#' }
check_data_quality <- function(data, domain = c("human", "animal", "environment"), return_score = FALSE) {
  domain <- match.arg(domain)
  
  # Initialize quality metrics
  quality <- list(
    completeness = NA,
    consistency = NA,
    accuracy = NA,
    overall_score = NA,
    issues = character()
  )
  
  # Check completeness
  na_ratio <- sum(is.na(data)) / (nrow(data) * ncol(data))
  quality$completeness <- 1 - na_ratio
  
  # Check for duplicate rows
  dupes <- duplicated(data) | duplicated(data, fromLast = TRUE)
  quality$consistency <- 1 - sum(dupes) / nrow(data)
  
  # Check accuracy of resistance data
  resistance_pattern <- "^r_.*"
  resistance_cols <- grep(resistance_pattern, names(data), value = TRUE)
  
  if (length(resistance_cols) > 0) {
    invalid_values <- 0
    for (col in resistance_cols) {
      if (is.numeric(data[[col]])) {
        # Check for values outside 0-1 range
        invalid <- data[[col]] < 0 | data[[col]] > 1
        invalid_values <- invalid_values + sum(invalid, na.rm = TRUE)
        
        # Check if corresponding numerator/denominator match the rate
        rate_name <- col
        pathogen_antibiotic <- sub("^r_", "", rate_name)
        n_name <- paste0("n_", pathogen_antibiotic)
        d_name <- paste0("d_", pathogen_antibiotic)
        
        if (n_name %in% names(data) && d_name %in% names(data)) {
          # Calculate expected rates
          expected_rates <- data[[n_name]] / data[[d_name]]
          # Check if reported rates match calculated rates (with some tolerance for rounding)
          rate_diffs <- abs(data[[rate_name]] - expected_rates)
          mismatches <- rate_diffs > 0.0001 & !is.na(rate_diffs)
          
          if (any(mismatches)) {
            quality$issues <- c(quality$issues, 
                              paste0("Reported rates don't match calculated rates for ", 
                                    sum(mismatches), " entries in ", rate_name))
          }
        }
      }
    }
    
    # Calculate accuracy based on invalid values
    total_resistance_values <- sum(!is.na(unlist(data[resistance_cols])))
    quality$accuracy <- 1 - (invalid_values / max(1, total_resistance_values))
  } else {
    quality$accuracy <- NA
    quality$issues <- c(quality$issues, "No resistance data columns found")
  }
  
  # Check for unusual patterns
  # 1. All resistance rates are exactly the same within a study
  study_groups <- split(data, data[["study ID"]])
  for (study_id in names(study_groups)) {
    study_data <- study_groups[[study_id]]
    for (col in resistance_cols) {
      if (is.numeric(study_data[[col]]) && length(unique(na.omit(study_data[[col]]))) == 1 && 
          length(na.omit(study_data[[col]])) > 1) {
        quality$issues <- c(quality$issues, 
                           paste0("All values in ", col, " are identical (", 
                                 unique(na.omit(study_data[[col]])), ") for study ", study_id))
      }
    }
  }
  
  # Overall quality score (weighted average of available metrics)
  available_metrics <- !is.na(c(quality$completeness, quality$consistency, quality$accuracy))
  if (any(available_metrics)) {
    metrics <- c(quality$completeness, quality$consistency, quality$accuracy)[available_metrics]
    quality$overall_score <- mean(metrics)
  } else {
    quality$overall_score <- NA
  }
  
  # Additional domain-specific checks
  if (domain == "human") {
    # Check population sizes
    if ("n_pop" %in% names(data)) {
      if (any(data$n_pop < 10, na.rm = TRUE)) {
        quality$issues <- c(quality$issues, "Some studies have very small population sizes (< 10)")
      }
    }
  } else if (domain == "animal") {
    # Add animal-specific checks here
  } else if (domain == "environment") {
    # Add environment-specific checks here
  }
  
  if (return_score) {
    return(quality$overall_score)
  } else {
    # Add quality information as attributes
    attr(data, "quality") <- quality
    attr(data, "quality_check_time") <- Sys.time()
    
    return(data)
  }
}

#' Standardize AMR data
#'
#' Converts AMR data to a standardized format for analysis
#'
#' @param data Data frame containing AMR data
#' @param domain Character string specifying the domain
#'
#' @return A standardized data frame in METEOR format
#' @export
#'
#' @examples
#' \dontrun{
#' # Standardize data after validation
#' standardized_data <- standardize_amr_data(validated_data)
#' }
standardize_amr_data <- function(data, domain = c("human", "animal", "environment")) {
  domain <- match.arg(domain)
  
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    warning("Input is not a data frame, converting")
    data <- as.data.frame(data)
  }
  
  # Create a new standardized data frame
  std_data <- data.frame(
    study_id = character(),
    firstauthor = character(),
    year = integer(),
    country = character(),
    region = character(),
    pathogen = character(),
    antibiotic = character(),
    resistance_rate = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    n_resistant = integer(),
    n_tested = integer(),
    domain = character(),
    stringsAsFactors = FALSE
  )
  
  # Extract resistance data
  resistance_pattern <- "^r_([A-Z]+)_([A-Za-z]+)$"  # Format: r_ANTIBIOTIC_PATHOGEN
  resistance_cols <- grep(resistance_pattern, names(data), value = TRUE)
  
  if (length(resistance_cols) == 0) {
    warning("No resistance data columns found matching the pattern r_ANTIBIOTIC_PATHOGEN")
    return(std_data)
  }
  
  # Process each resistance column
  for (col in resistance_cols) {
    # Extract antibiotic and pathogen from column name
    matches <- regmatches(col, regexec(resistance_pattern, col))[[1]]
    if (length(matches) < 3) {
      warning("Column name format not recognized: ", col)
      next
    }
    
    antibiotic <- matches[2]
    pathogen <- matches[3]
    
    # Look for corresponding numerator and denominator columns
    n_col <- paste0("n_", antibiotic, "_", pathogen)
    d_col <- paste0("d_", antibiotic, "_", pathogen)
    
    # Check if columns exist
    if (!n_col %in% names(data) || !d_col %in% names(data)) {
      warning("Missing numerator or denominator columns for ", col)
      next
    }
    
    # For each study, create a record
    for (i in 1:nrow(data)) {
      # Skip if missing resistance data
      if (is.na(data[i, col])) {
        next
      }
      
      # Extract study information
      study_id <- as.character(data[i, "study ID"])
      first_author <- as.character(data[i, "firstauthor_last"])
      year <- as.integer(data[i, "year_published"])
      location <- as.character(data[i, "location"])
      
      # Extract resistance information
      resistance_rate <- as.numeric(data[i, col])
      n_resistant <- as.integer(data[i, n_col])
      n_tested <- as.integer(data[i, d_col])
      
      # Calculate confidence interval (Wilson method)
      if (!is.na(n_resistant) && !is.na(n_tested) && n_tested > 0) {
        ci <- prop.test(n_resistant, n_tested, conf.level = 0.95)$conf.int
        ci_lower <- ci[1]
        ci_upper <- ci[2]
      } else {
        ci_lower <- NA
        ci_upper <- NA
      }
      
      # Determine region (simplified)
      region <- "Unknown"
      if (!is.na(location)) {
        asia_countries <- c("India", "Pakistan", "Bangladesh", "Nepal", "Sri Lanka", "China", "Japan")
        if (location %in% asia_countries) {
          region <- "Asia"
        } else if (location %in% c("USA", "Canada")) {
          region <- "North America"
        } else if (location %in% c("UK", "France", "Germany", "Italy", "Spain")) {
          region <- "Europe"
        }
      }
      
      # Map pathogen codes to full names if needed
      pathogen_full <- switch(pathogen,
                            Ecoil = "Escherichia coli",
                            KP = "Klebsiella pneumoniae",
                            SA = "Staphylococcus aureus",
                            SP = "Streptococcus pneumoniae",
                            pathogen)
      
      # Map antibiotic codes to full names
      antibiotic_full <- switch(antibiotic,
                              AML = "Amoxicillin",
                              AMP = "Ampicillin",
                              AMK = "Amikacin",
                              AZM = "Azithromycin",
                              ATM = "Aztreonam",
                              CFZ = "Cefazolin",
                              FEP = "Cefepime",
                              CFM = "Cefixime",
                              CTX = "Cefotaxime",
                              FOX = "Cefoxitin",
                              CAZ = "Ceftazidime",
                              CRO = "Ceftriaxone",
                              LEX = "Cephalexin",
                              CHL = "Chloramphenicol",
                              CIP = "Ciprofloxacin",
                              CLI = "Clindamycin",
                              CST = "Colistin",
                              SXT = "Co-trimoxazole",
                              DOX = "Doxycycline",
                              ERY = "Erythromycin",
                              GEN = "Gentamicin",
                              IPM = "Imipenem",
                              KAN = "Kanamycin",
                              LVX = "Levofloxacin",
                              MEM = "Meropenem",
                              NAL = "Nalidixic acid",
                              NIT = "Nitrofurantoin",
                              OFX = "Ofloxacin",
                              OXA = "Oxacillin",
                              PEN = "Penicillin",
                              STR = "Streptomycin",
                              SOX = "Sulfoxazole",
                              TET = "Tetracycline",
                              TGC = "Tigecycline",
                              VAN = "Vancomycin",
                              multi = "Multiple antibiotics",  # For multi-drug resistance
                              antibiotic)
      
      # Create a new row
      new_row <- data.frame(
        study_id = study_id,
        firstauthor = first_author,
        year = year,
        country = location,
        region = region,
        pathogen = pathogen_full,
        antibiotic = antibiotic_full,
        resistance_rate = resistance_rate,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        n_resistant = n_resistant,
        n_tested = n_tested,
        domain = domain,
        stringsAsFactors = FALSE
      )
      
      # Add additional domain-specific columns
      if (domain == "human") {
        if ("population" %in% names(data)) {
          new_row$population_type <- as.character(data[i, "population"])
        } else {
          new_row$population_type <- NA
        }
        
        if ("Environment" %in% names(data)) {
          new_row$setting <- as.character(data[i, "Environment"])
        } else {
          new_row$setting <- NA
        }
      } else if (domain == "animal") {
        if ("animal_type" %in% names(data)) {
          new_row$animal_type <- as.character(data[i, "animal_type"])
        } else {
          new_row$animal_type <- NA
        }
      } else if (domain == "environment") {
        if ("environment_category" %in% names(data)) {
          new_row$environment_category <- as.character(data[i, "environment_category"])
        } else {
          new_row$environment_category <- NA
        }
      }
      
      # Append to the standardized data frame
      std_data <- rbind(std_data, new_row)
    }
  }
  
  # Add metadata
  attr(std_data, "standardized") <- TRUE
  attr(std_data, "standardization_time") <- Sys.time()
  attr(std_data, "source_data") <- deparse(substitute(data))
  attr(std_data, "domain") <- domain
  
  # Warn if no data was extracted
  if (nrow(std_data) == 0) {
    warning("No data could be extracted and standardized")
  }
  
  return(std_data)
}

#' Handle missing data
#'
#' Imputes or handles missing values in AMR data
#'
#' @param data Data frame containing AMR data
#' @param method Character string specifying the imputation method
#' @param cols Character vector of column names to impute
#'
#' @return Data frame with imputed missing values
#' @export
#'
#' @examples
#' \dontrun{
#' # Impute missing resistance rates with mean values
#' imputed_data <- impute_missing_data(standardized_data, method = "mean", 
#'                                    cols = "resistance_rate")
#' }
impute_missing_data <- function(data, method = c("remove", "mean", "median", "mode"), 
                              cols = NULL) {
  method <- match.arg(method)
  
  # If no columns specified, detect numeric columns with missing values
  if (is.null(cols)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    cols <- numeric_cols[sapply(data[numeric_cols], function(x) any(is.na(x)))]
    
    if (length(cols) == 0) {
      message("No missing values detected in numeric columns")
      return(data)
    }
  }
  
  # Validate columns exist in data
  invalid_cols <- cols[!cols %in% names(data)]
  if (length(invalid_cols) > 0) {
    warning("The following columns do not exist in data: ", 
            paste(invalid_cols, collapse = ", "))
    cols <- cols[cols %in% names(data)]
  }
  
  if (length(cols) == 0) {
    warning("No valid columns to impute")
    return(data)
  }
  
  # Handle missing data based on method
  result <- data
  
  if (method == "remove") {
    # Remove rows with missing values in specified columns
    complete_rows <- complete.cases(data[, cols, drop = FALSE])
    if (sum(!complete_rows) > 0) {
      message("Removed ", sum(!complete_rows), " rows with missing values")
      result <- data[complete_rows, , drop = FALSE]
    }
  } else {
    # Impute values
    for (col in cols) {
      if (!is.numeric(data[[col]])) {
        warning("Column '", col, "' is not numeric. Skipping imputation.")
        next
      }
      
      na_indices <- is.na(data[[col]])
      
      if (sum(na_indices) == 0) {
        message("No missing values in column '", col, "'")
        next
      }
      
      # Calculate imputation value
      if (method == "mean") {
        impute_value <- mean(data[[col]], na.rm = TRUE)
      } else if (method == "median") {
        impute_value <- stats::median(data[[col]], na.rm = TRUE)
      } else if (method == "mode") {
        # Simple mode calculation (most common value)
        tab <- table(data[[col]])
        impute_value <- as.numeric(names(tab)[which.max(tab)])
      }
      
      # Impute missing values
      result[[col]][na_indices] <- impute_value
      
      message("Imputed ", sum(na_indices), " missing values in column '", col, 
              "' with ", method, " (", round(impute_value, 4), ")")
    }
  }
  
  # Add imputation information as attributes
  attr(result, "imputed") <- TRUE
  attr(result, "imputation_method") <- method
  attr(result, "imputed_columns") <- cols
  attr(result, "imputation_time") <- Sys.time()
  
  return(result)
} 