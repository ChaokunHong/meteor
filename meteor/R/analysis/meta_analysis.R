#' Calculate pooled resistance rate
#'
#' Performs meta-analysis to calculate pooled antimicrobial resistance rates
#'
#' @param data Data frame containing standardized AMR data
#' @param by Character vector of variables to group by
#' @param method Meta-analysis method (e.g., "fixed" or "random")
#' @param measure Effect measure (default: "PR" for proportion)
#' @param ... Additional arguments passed to meta::metaprop
#'
#' @return A list containing meta-analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate pooled resistance rates by pathogen and antibiotic
#' meta_results <- calculate_pooled_rate(
#'   standardized_data,
#'   by = c("pathogen", "antibiotic"),
#'   method = "random"
#' )
#' }
calculate_pooled_rate <- function(data, by = NULL, 
                                method = c("random", "fixed"), 
                                measure = "PR", ...) {
  method <- match.arg(method)
  
  # Check for meta package
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("Package 'meta' is required for this function. Please install it.")
  }
  
  # Check if data is standardized
  if (is.null(attr(data, "standardized"))) {
    warning("Input data does not appear to be standardized. Results may be unreliable.")
  }
  
  # Check required columns
  required_cols <- c("n_resistant", "n_tested")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # If no grouping variables, perform overall meta-analysis
  if (is.null(by)) {
    # Perform meta-analysis on all data
    meta_result <- try(
      meta::metaprop(
        event = data$n_resistant,
        n = data$n_tested,
        studlab = data$study_id,
        method = method,
        sm = measure,
        ...
      ),
      silent = TRUE
    )
    
    if (inherits(meta_result, "try-error")) {
      warning("Meta-analysis failed: ", as.character(meta_result))
      return(NULL)
    }
    
    # Return result
    return(list(
      overall = meta_result,
      by = NULL,
      data = data
    ))
  } else {
    # Check if grouping variables exist
    missing_by <- by[!by %in% names(data)]
    if (length(missing_by) > 0) {
      stop("Grouping variables not found in data: ", paste(missing_by, collapse = ", "))
    }
    
    # Create grouping factor
    if (length(by) == 1) {
      groups <- data[[by]]
    } else {
      # Combine multiple grouping variables
      groups <- do.call(paste, c(data[by], sep = " | "))
    }
    
    # Split data by groups
    grouped_data <- split(data, groups)
    
    # Perform meta-analysis for each group
    meta_results <- list()
    
    for (group_name in names(grouped_data)) {
      group_data <- grouped_data[[group_name]]
      
      # Skip groups with fewer than 2 studies
      if (nrow(group_data) < 2) {
        warning("Group '", group_name, "' has fewer than 2 studies, skipping meta-analysis")
        meta_results[[group_name]] <- NULL
        next
      }
      
      # Perform meta-analysis
      meta_result <- try(
        meta::metaprop(
          event = group_data$n_resistant,
          n = group_data$n_tested,
          studlab = group_data$study_id,
          method = method,
          sm = measure,
          ...
        ),
        silent = TRUE
      )
      
      if (inherits(meta_result, "try-error")) {
        warning("Meta-analysis failed for group '", group_name, "': ", as.character(meta_result))
        meta_results[[group_name]] <- NULL
      } else {
        meta_results[[group_name]] <- meta_result
      }
    }
    
    # Calculate overall meta-analysis
    overall_result <- try(
      meta::metaprop(
        event = data$n_resistant,
        n = data$n_tested,
        studlab = data$study_id,
        method = method,
        sm = measure,
        ...
      ),
      silent = TRUE
    )
    
    if (inherits(overall_result, "try-error")) {
      warning("Overall meta-analysis failed: ", as.character(overall_result))
      overall_result <- NULL
    }
    
    # Return results
    return(list(
      overall = overall_result,
      by = meta_results,
      grouping = by,
      data = data
    ))
  }
}

#' Analyze heterogeneity
#'
#' Analyzes heterogeneity in meta-analysis results
#'
#' @param meta_result Meta-analysis result from calculate_pooled_rate
#' @param detailed Logical; if TRUE, returns detailed heterogeneity statistics
#'
#' @return A data frame with heterogeneity statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze heterogeneity in meta-analysis results
#' heterogeneity <- analyze_heterogeneity(meta_results)
#' }
analyze_heterogeneity <- function(meta_result, detailed = FALSE) {
  # Check input
  if (is.null(meta_result)) {
    stop("Meta-analysis result is NULL")
  }
  
  # Initialize results data frame
  result <- data.frame(
    group = character(),
    k = integer(),            # Number of studies
    Q = numeric(),            # Cochran's Q statistic
    df = integer(),           # Degrees of freedom
    p_value = numeric(),      # P-value for Q test
    I2 = numeric(),           # I² statistic (%)
    tau2 = numeric(),         # Tau² (between-study variance)
    H = numeric(),            # H statistic
    stringsAsFactors = FALSE
  )
  
  # Function to extract heterogeneity statistics from a meta object
  extract_stats <- function(meta_obj, group_name) {
    if (is.null(meta_obj) || !inherits(meta_obj, "meta")) {
      return(NULL)
    }
    
    data.frame(
      group = group_name,
      k = meta_obj$k,
      Q = meta_obj$Q,
      df = meta_obj$df.Q,
      p_value = meta_obj$pval.Q,
      I2 = meta_obj$I2,
      tau2 = meta_obj$tau2,
      H = meta_obj$H,
      stringsAsFactors = FALSE
    )
  }
  
  # Extract overall heterogeneity stats
  if (!is.null(meta_result$overall)) {
    overall_stats <- extract_stats(meta_result$overall, "Overall")
    result <- rbind(result, overall_stats)
  }
  
  # Extract heterogeneity stats for each group
  if (!is.null(meta_result$by)) {
    for (group_name in names(meta_result$by)) {
      group_stats <- extract_stats(meta_result$by[[group_name]], group_name)
      if (!is.null(group_stats)) {
        result <- rbind(result, group_stats)
      }
    }
  }
  
  # For detailed output, add interpretation and additional diagnostics
  if (detailed && nrow(result) > 0) {
    # Add I² interpretation
    result$I2_interpretation <- cut(
      result$I2,
      breaks = c(-Inf, 25, 50, 75, Inf),
      labels = c("Low", "Moderate", "Substantial", "Considerable"),
      right = FALSE
    )
    
    # Add heterogeneity significance
    result$heterogeneity_significant <- result$p_value < 0.05
    
    # Add prediction intervals for random-effects models
    if (!is.null(meta_result$overall) && meta_result$overall$method == "Inverse") {
      result$PI_lower <- rep(NA, nrow(result))
      result$PI_upper <- rep(NA, nrow(result))
      
      if (!is.null(meta_result$overall)) {
        pi_overall <- meta_result$overall$lower.predict
        result$PI_lower[result$group == "Overall"] <- meta_result$overall$lower.predict
        result$PI_upper[result$group == "Overall"] <- meta_result$overall$upper.predict
      }
      
      if (!is.null(meta_result$by)) {
        for (i in 1:nrow(result)) {
          group_name <- result$group[i]
          if (group_name != "Overall" && !is.null(meta_result$by[[group_name]])) {
            result$PI_lower[i] <- meta_result$by[[group_name]]$lower.predict
            result$PI_upper[i] <- meta_result$by[[group_name]]$upper.predict
          }
        }
      }
    }
  }
  
  return(result)
}

#' Perform subgroup analysis
#'
#' Performs meta-analysis by subgroups
#'
#' @param data Data frame containing standardized AMR data
#' @param by Character vector of subgrouping variables
#' @param meta_var Character vector of variables to include in meta-analysis
#' @param method Meta-analysis method
#' @param ... Additional parameters passed to calculate_pooled_rate
#'
#' @return A list containing subgroup meta-analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Perform subgroup analysis by region and year
#' subgroup_results <- perform_subgroup_analysis(
#'   standardized_data,
#'   by = c("region", "year"),
#'   meta_var = c("pathogen", "antibiotic")
#' )
#' }
perform_subgroup_analysis <- function(data, by, meta_var = NULL, 
                                   method = c("random", "fixed"), ...) {
  method <- match.arg(method)
  
  # Check if subgrouping variables exist
  missing_by <- by[!by %in% names(data)]
  if (length(missing_by) > 0) {
    stop("Subgrouping variables not found in data: ", paste(missing_by, collapse = ", "))
  }
  
  # If meta_var is NULL, just do simple subgroup analysis
  if (is.null(meta_var)) {
    return(calculate_pooled_rate(data, by = by, method = method, ...))
  }
  
  # Check if meta variables exist
  missing_meta <- meta_var[!meta_var %in% names(data)]
  if (length(missing_meta) > 0) {
    stop("Meta-analysis variables not found in data: ", paste(missing_meta, collapse = ", "))
  }
  
  # Create subgroups
  subgroups <- list()
  
  # Split by the 'by' variables first
  if (length(by) == 1) {
    subgroup_split <- split(data, data[[by]])
  } else {
    # Combine multiple grouping variables
    subgroup_factor <- do.call(paste, c(data[by], sep = " | "))
    subgroup_split <- split(data, subgroup_factor)
  }
  
  # For each subgroup, perform meta-analysis by meta_var
  for (subgroup_name in names(subgroup_split)) {
    subgroup_data <- subgroup_split[[subgroup_name]]
    
    # Skip subgroups with insufficient data
    if (nrow(subgroup_data) < 2) {
      warning("Subgroup '", subgroup_name, "' has fewer than 2 studies, skipping")
      next
    }
    
    # Perform meta-analysis within this subgroup
    subgroup_result <- calculate_pooled_rate(subgroup_data, by = meta_var, method = method, ...)
    
    # Save result
    subgroups[[subgroup_name]] <- subgroup_result
  }
  
  # Also calculate an overall result
  overall_result <- calculate_pooled_rate(data, by = meta_var, method = method, ...)
  
  # Return results
  return(list(
    overall = overall_result,
    subgroups = subgroups,
    by_vars = by,
    meta_vars = meta_var,
    data = data
  ))
}

#' Perform sensitivity analysis
#'
#' Analyzes the influence of individual studies on meta-analysis results
#'
#' @param data Data frame containing standardized AMR data
#' @param by Character vector of variables to group by
#' @param method Meta-analysis method
#' @param ... Additional parameters passed to calculate_pooled_rate
#'
#' @return A list containing sensitivity analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Perform sensitivity analysis by removing each study
#' sensitivity_results <- perform_sensitivity_analysis(standardized_data)
#' }
perform_sensitivity_analysis <- function(data, by = NULL, 
                                      method = c("random", "fixed"), ...) {
  method <- match.arg(method)
  
  # Calculate overall meta-analysis
  overall_result <- calculate_pooled_rate(data, by = by, method = method, ...)
  
  # Extract all unique study IDs
  study_ids <- unique(data$study_id)
  
  # Initialize results list
  sensitivity <- list(
    overall = overall_result,
    leave_one_out = list(),
    influence = data.frame(
      study_id = character(),
      k_included = integer(),       # Number of studies included
      original_estimate = numeric(), # Original pooled estimate
      estimate_without = numeric(),  # Estimate without this study
      absolute_change = numeric(),   # Absolute change in estimate
      relative_change = numeric(),   # Relative change in estimate (%)
      influence_score = numeric(),   # Influence score
      stringsAsFactors = FALSE
    )
  )
  
  # Perform leave-one-out analysis
  for (study_id in study_ids) {
    # Remove this study
    data_without <- subset(data, study_id != study_id)
    
    # Skip if insufficient data
    if (nrow(data_without) < 2) {
      warning("Removing study '", study_id, "' leaves fewer than 2 studies, skipping")
      next
    }
    
    # Perform meta-analysis without this study
    result_without <- calculate_pooled_rate(data_without, by = by, method = method, ...)
    
    # Save result
    sensitivity$leave_one_out[[study_id]] <- result_without
    
    # Calculate influence metrics
    if (!is.null(overall_result$overall) && !is.null(result_without$overall)) {
      original_est <- overall_result$overall$TE.random
      if (method == "fixed") {
        original_est <- overall_result$overall$TE.fixed
      }
      
      without_est <- result_without$overall$TE.random
      if (method == "fixed") {
        without_est <- result_without$overall$TE.fixed
      }
      
      abs_change <- without_est - original_est
      rel_change <- (abs_change / original_est) * 100
      
      # Simple influence score (higher = more influence)
      influence_score <- abs(rel_change)
      
      # Add to influence table
      new_row <- data.frame(
        study_id = study_id,
        k_included = nrow(data_without),
        original_estimate = original_est,
        estimate_without = without_est,
        absolute_change = abs_change,
        relative_change = rel_change,
        influence_score = influence_score,
        stringsAsFactors = FALSE
      )
      
      sensitivity$influence <- rbind(sensitivity$influence, new_row)
    }
  }
  
  # Sort influence by score
  if (nrow(sensitivity$influence) > 0) {
    sensitivity$influence <- sensitivity$influence[
      order(sensitivity$influence$influence_score, decreasing = TRUE), 
    ]
  }
  
  # Calculate some summary stats for the influence analysis
  if (nrow(sensitivity$influence) > 0) {
    sensitivity$summary <- list(
      max_influence = max(sensitivity$influence$influence_score),
      mean_influence = mean(sensitivity$influence$influence_score),
      max_study = sensitivity$influence$study_id[which.max(sensitivity$influence$influence_score)],
      direction_changes = sum(sign(sensitivity$influence$original_estimate) != 
                             sign(sensitivity$influence$estimate_without))
    )
  }
  
  return(sensitivity)
}

#' Assess publication bias
#'
#' Evaluates potential publication bias in meta-analysis data
#'
#' @param data Data frame containing standardized AMR data
#' @param by Character vector of variables to group by
#' @param methods Character vector of methods to use for publication bias assessment
#'
#' @return A list containing publication bias assessment results
#' @export
#'
#' @examples
#' \dontrun{
#' # Assess publication bias using multiple methods
#' bias_results <- assess_publication_bias(
#'   standardized_data,
#'   methods = c("funnel", "egger", "trim_fill")
#' )
#' }
assess_publication_bias <- function(data, by = NULL, 
                                  methods = c("funnel", "egger", "trim_fill")) {
  # Check for meta and metafor packages
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("Package 'meta' is required for this function. Please install it.")
  }
  
  if ("trim_fill" %in% methods && !requireNamespace("metafor", quietly = TRUE)) {
    warning("Package 'metafor' is required for trim-and-fill method. This method will be skipped.")
    methods <- methods[methods != "trim_fill"]
  }
  
  # Validate methods
  valid_methods <- c("funnel", "egger", "trim_fill")
  invalid_methods <- methods[!methods %in% valid_methods]
  if (length(invalid_methods) > 0) {
    warning("Invalid methods specified: ", paste(invalid_methods, collapse = ", "), 
            ". Will use only valid methods.")
    methods <- methods[methods %in% valid_methods]
  }
  
  if (length(methods) == 0) {
    stop("No valid methods specified for publication bias assessment.")
  }
  
  # Prepare data for analysis
  if (is.null(by)) {
    # Single meta-analysis for all data
    meta_result <- try(
      meta::metaprop(
        event = data$n_resistant,
        n = data$n_tested,
        studlab = data$study_id,
        method = "Inverse",
        sm = "PR"
      ),
      silent = TRUE
    )
    
    if (inherits(meta_result, "try-error")) {
      warning("Meta-analysis failed: ", as.character(meta_result))
      return(NULL)
    }
    
    # Initialize results list
    results <- list(
      by = NULL,
      funnel = NULL,
      egger = NULL,
      trim_fill = NULL
    )
    
    # Perform publication bias assessments
    if ("funnel" %in% methods) {
      # Funnel plot asymmetry
      results$funnel <- meta_result
    }
    
    if ("egger" %in% methods) {
      # Egger's test
      if (meta_result$k >= 10) {
        egger_test <- meta::metabias(meta_result, method = "linreg")
        results$egger <- egger_test
      } else {
        warning("At least 10 studies are required for Egger's test. Skipping.")
      }
    }
    
    if ("trim_fill" %in% methods) {
      # Trim and fill analysis using metafor
      if (meta_result$k >= 3) {
        # Convert meta object to rma object for metafor
        es <- meta_result$TE
        se <- meta_result$seTE
        
        trim_fill <- try(
          metafor::trimfill(metafor::rma(yi = es, sei = se)),
          silent = TRUE
        )
        
        if (!inherits(trim_fill, "try-error")) {
          results$trim_fill <- trim_fill
        } else {
          warning("Trim and fill analysis failed: ", as.character(trim_fill))
        }
      } else {
        warning("At least 3 studies are required for trim and fill analysis. Skipping.")
      }
    }
    
    return(results)
  } else {
    # Grouped meta-analysis
    if (length(by) == 1) {
      groups <- data[[by]]
    } else {
      # Combine multiple grouping variables
      groups <- do.call(paste, c(data[by], sep = " | "))
    }
    
    # Split data by groups
    grouped_data <- split(data, groups)
    
    # Perform publication bias assessment for each group
    group_results <- list()
    
    for (group_name in names(grouped_data)) {
      group_data <- grouped_data[[group_name]]
      
      # Skip groups with insufficient studies
      if (nrow(group_data) < 3) {
        warning("Group '", group_name, "' has fewer than 3 studies, skipping publication bias assessment")
        next
      }
      
      # Perform meta-analysis for this group
      meta_result <- try(
        meta::metaprop(
          event = group_data$n_resistant,
          n = group_data$n_tested,
          studlab = group_data$study_id,
          method = "Inverse",
          sm = "PR"
        ),
        silent = TRUE
      )
      
      if (inherits(meta_result, "try-error")) {
        warning("Meta-analysis failed for group '", group_name, "': ", as.character(meta_result))
        next
      }
      
      # Initialize results for this group
      group_result <- list(
        funnel = NULL,
        egger = NULL,
        trim_fill = NULL
      )
      
      # Perform publication bias assessments
      if ("funnel" %in% methods) {
        # Funnel plot data
        group_result$funnel <- meta_result
      }
      
      if ("egger" %in% methods) {
        # Egger's test
        if (meta_result$k >= 10) {
          egger_test <- meta::metabias(meta_result, method = "linreg")
          group_result$egger <- egger_test
        } else {
          message("Group '", group_name, "' has fewer than 10 studies, skipping Egger's test")
        }
      }
      
      if ("trim_fill" %in% methods) {
        # Trim and fill analysis using metafor
        if (meta_result$k >= 3) {
          # Convert meta object to rma object for metafor
          es <- meta_result$TE
          se <- meta_result$seTE
          
          trim_fill <- try(
            metafor::trimfill(metafor::rma(yi = es, sei = se)),
            silent = TRUE
          )
          
          if (!inherits(trim_fill, "try-error")) {
            group_result$trim_fill <- trim_fill
          } else {
            warning("Trim and fill analysis failed for group '", group_name, "': ", as.character(trim_fill))
          }
        } else {
          message("Group '", group_name, "' has fewer than 3 studies, skipping trim and fill analysis")
        }
      }
      
      # Save results for this group
      group_results[[group_name]] <- group_result
    }
    
    # Return results
    return(list(
      by = by,
      groups = group_results
    ))
  }
} 