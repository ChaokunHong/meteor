#' Calculate pooled resistance rate
#'
#' Perform meta-analysis to calculate pooled antimicrobial resistance rates
#'
#' @param data A data frame containing AMR data
#' @param by Variables to group by for separate meta-analyses
#' @param method Meta-analysis method: "fixed" or "random"
#' @param measure Effect measure to use (default: "PR" for proportion)
#' @param weight_by Variable to use for weighting (default: "sample_size_ab")
#' @return A list containing meta-analysis results
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' results <- calculate_pooled_rate(human_amr, by = c("pathogen", "antibiotic"))
#' }
calculate_pooled_rate <- function(data, 
                                by = NULL,
                                method = "random",
                                measure = "PR",
                                weight_by = "sample_size_ab") {
  # Check dependencies
  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("Package 'metafor' is required for this function")
  }
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Check required columns
  required_cols <- c("resistance_rate", weight_by)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check if resistance_rate is numeric and between 0 and 1
  if (!is.numeric(data$resistance_rate) || 
      any(data$resistance_rate < 0 | data$resistance_rate > 1, na.rm = TRUE)) {
    warning("resistance_rate should be numeric between 0 and 1; correcting...")
    data$resistance_rate <- pmin(pmax(as.numeric(data$resistance_rate), 0), 1)
  }
  
  # Initialize results list
  results <- list(
    overall = NULL,
    by_group = list(),
    data = data,
    method = method,
    measure = measure
  )
  
  # If no grouping variables, perform overall meta-analysis
  if (is.null(by) || length(by) == 0) {
    # Transform proportions using Freeman-Tukey double arcsine transformation
    data$prop_transformed <- metafor::escalc(
      measure = "PFT",
      xi = round(data$resistance_rate * data[[weight_by]]),
      ni = data[[weight_by]]
    )$yi
    
    # Compute standard errors
    data$se <- 1 / sqrt(data[[weight_by]])
    
    # Perform meta-analysis
    meta_result <- metafor::rma(
      yi = prop_transformed,
      sei = se,
      method = method,
      data = data
    )
    
    # Back-transform results to proportions
    results$overall <- list(
      model = meta_result,
      pooled_rate = metafor::transf.ipft(meta_result$b),
      ci_lower = metafor::transf.ipft(meta_result$ci.lb),
      ci_upper = metafor::transf.ipft(meta_result$ci.ub),
      heterogeneity = c(I2 = meta_result$I2, H2 = meta_result$H2, tau2 = meta_result$tau2),
      k = meta_result$k
    )
    
    return(results)
  }
  
  # If grouping variables, perform meta-analysis for each group
  if (!all(by %in% colnames(data))) {
    stop("Not all grouping variables are in the data: ", 
         paste(setdiff(by, colnames(data)), collapse = ", "))
  }
  
  # Split data by groups
  groups <- unique(data[, by, drop = FALSE])
  
  # For each group, perform meta-analysis
  for (i in 1:nrow(groups)) {
    # Extract group values
    group_vals <- as.list(groups[i, ])
    
    # Create group identifier
    group_id <- paste(
      sapply(1:length(by), function(j) paste0(by[j], "=", group_vals[[by[j]]])),
      collapse = ", "
    )
    
    # Filter data for this group
    filter_expr <- paste(
      sapply(1:length(by), function(j) paste0("data$", by[j], " == '", group_vals[[by[j]]], "'")),
      collapse = " & "
    )
    group_data <- subset(data, eval(parse(text = filter_expr)))
    
    # Skip if not enough data
    if (nrow(group_data) < 2) {
      results$by_group[[group_id]] <- list(
        values = group_vals,
        pooled_rate = NA,
        ci_lower = NA,
        ci_upper = NA,
        heterogeneity = NA,
        k = nrow(group_data)
      )
      next
    }
    
    # Transform proportions
    group_data$prop_transformed <- metafor::escalc(
      measure = "PFT",
      xi = round(group_data$resistance_rate * group_data[[weight_by]]),
      ni = group_data[[weight_by]]
    )$yi
    
    # Compute standard errors
    group_data$se <- 1 / sqrt(group_data[[weight_by]])
    
    # Perform meta-analysis
    tryCatch({
      meta_result <- metafor::rma(
        yi = prop_transformed,
        sei = se,
        method = method,
        data = group_data
      )
      
      # Store results
      results$by_group[[group_id]] <- list(
        values = group_vals,
        model = meta_result,
        pooled_rate = metafor::transf.ipft(meta_result$b),
        ci_lower = metafor::transf.ipft(meta_result$ci.lb),
        ci_upper = metafor::transf.ipft(meta_result$ci.ub),
        heterogeneity = c(I2 = meta_result$I2, H2 = meta_result$H2, tau2 = meta_result$tau2),
        k = meta_result$k,
        data = group_data
      )
    }, error = function(e) {
      warning("Error in meta-analysis for group: ", group_id, " - ", e$message)
      results$by_group[[group_id]] <- list(
        values = group_vals,
        pooled_rate = NA,
        ci_lower = NA,
        ci_upper = NA,
        heterogeneity = NA,
        k = nrow(group_data),
        error = e$message
      )
    })
  }
  
  # Create a summary data frame of results
  summary_data <- data.frame(
    group = names(results$by_group),
    k = sapply(results$by_group, function(x) x$k),
    pooled_rate = sapply(results$by_group, function(x) x$pooled_rate),
    ci_lower = sapply(results$by_group, function(x) x$ci_lower),
    ci_upper = sapply(results$by_group, function(x) x$ci_upper),
    I2 = sapply(results$by_group, function(x) {
      if (is.list(x$heterogeneity)) x$heterogeneity["I2"] else NA
    }),
    stringsAsFactors = FALSE
  )
  
  # Add group variables as separate columns
  for (var in by) {
    summary_data[[var]] <- sapply(results$by_group, function(x) x$values[[var]])
  }
  
  # Sort by pooled rate
  summary_data <- summary_data[order(summary_data$pooled_rate, decreasing = TRUE), ]
  
  # Add to results
  results$summary <- summary_data
  
  return(results)
}

#' Analyze heterogeneity
#'
#' Analyze heterogeneity in antimicrobial resistance data
#'
#' @param data A data frame containing AMR data
#' @param by Variables to group by for separate heterogeneity analyses
#' @param method Meta-analysis method: "fixed" or "random"
#' @return A list containing heterogeneity analysis results
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' heterogeneity <- analyze_heterogeneity(human_amr, by = c("pathogen", "antibiotic"))
#' }
analyze_heterogeneity <- function(data, by = NULL, method = "random") {
  # Check dependencies
  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("Package 'metafor' is required for this function")
  }
  
  # Run meta-analysis first to get heterogeneity statistics
  meta_results <- calculate_pooled_rate(data, by = by, method = method)
  
  # Extract heterogeneity statistics
  if (is.null(by) || length(by) == 0) {
    # Overall heterogeneity only
    results <- list(
      overall = meta_results$overall$heterogeneity,
      method = method
    )
  } else {
    # Heterogeneity by group
    results <- list(
      by_group = lapply(meta_results$by_group, function(x) x$heterogeneity),
      summary = meta_results$summary[, c("group", by, "k", "I2")],
      method = method
    )
    
    # Add overall heterogeneity test if possible
    if (all(c("pathogen", "antibiotic") %in% by)) {
      # Try to perform a meta-regression with moderators
      tryCatch({
        # Prepare data for meta-regression
        data$prop_transformed <- metafor::escalc(
          measure = "PFT",
          xi = round(data$resistance_rate * data$sample_size_ab),
          ni = data$sample_size_ab
        )$yi
        
        data$se <- 1 / sqrt(data$sample_size_ab)
        
        # Perform meta-regression with moderators
        metareg <- metafor::rma(
          yi = prop_transformed,
          sei = se,
          mods = ~ as.factor(pathogen) * as.factor(antibiotic),
          method = method,
          data = data
        )
        
        # Add to results
        results$meta_regression <- list(
          model = metareg,
          QE = metareg$QE,
          QEp = metareg$QEp,
          QM = metareg$QM,
          QMp = metareg$QMp
        )
      }, error = function(e) {
        warning("Could not perform meta-regression: ", e$message)
      })
    }
  }
  
  return(results)
}

#' Perform subgroup analysis
#'
#' Perform subgroup analysis of antimicrobial resistance rates
#'
#' @param data A data frame containing AMR data
#' @param by Variables to group by for subgroup analyses
#' @param method Meta-analysis method: "fixed" or "random"
#' @return A list containing subgroup analysis results
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' subgroups <- perform_subgroup_analysis(human_amr, by = c("population", "location"))
#' }
perform_subgroup_analysis <- function(data, by, method = "random") {
  # Check dependencies
  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("Package 'metafor' is required for this function")
  }
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!all(by %in% colnames(data))) {
    stop("Not all grouping variables are in the data: ", 
         paste(setdiff(by, colnames(data)), collapse = ", "))
  }
  
  # Initialize results list
  results <- list(
    subgroups = list(),
    method = method
  )
  
  # Analyze each subgroup variable separately
  for (var in by) {
    # Get unique values for this variable
    values <- unique(data[[var]])
    
    # Skip if NA or too few values
    if (length(values) < 2 || all(is.na(values))) {
      next
    }
    
    # Store results for this variable
    var_results <- list()
    
    # For each value, perform meta-analysis
    for (val in values) {
      if (is.na(val)) next
      
      # Subset data
      subset_data <- data[data[[var]] == val, ]
      
      # Skip if not enough data
      if (nrow(subset_data) < 2) {
        var_results[[as.character(val)]] <- list(
          value = val,
          pooled_rate = NA,
          ci_lower = NA,
          ci_upper = NA,
          heterogeneity = NA,
          k = nrow(subset_data)
        )
        next
      }
      
      # Perform meta-analysis
      tryCatch({
        # Transform proportions
        subset_data$prop_transformed <- metafor::escalc(
          measure = "PFT",
          xi = round(subset_data$resistance_rate * subset_data$sample_size_ab),
          ni = subset_data$sample_size_ab
        )$yi
        
        subset_data$se <- 1 / sqrt(subset_data$sample_size_ab)
        
        # Run meta-analysis
        meta_result <- metafor::rma(
          yi = prop_transformed,
          sei = se,
          method = method,
          data = subset_data
        )
        
        # Store results
        var_results[[as.character(val)]] <- list(
          value = val,
          model = meta_result,
          pooled_rate = metafor::transf.ipft(meta_result$b),
          ci_lower = metafor::transf.ipft(meta_result$ci.lb),
          ci_upper = metafor::transf.ipft(meta_result$ci.ub),
          heterogeneity = c(I2 = meta_result$I2, H2 = meta_result$H2, tau2 = meta_result$tau2),
          k = meta_result$k
        )
      }, error = function(e) {
        warning("Error in meta-analysis for ", var, "=", val, " - ", e$message)
        var_results[[as.character(val)]] <- list(
          value = val,
          pooled_rate = NA,
          ci_lower = NA,
          ci_upper = NA,
          heterogeneity = NA,
          k = nrow(subset_data),
          error = e$message
        )
      })
    }
    
    # Test for subgroup differences if possible
    subgroup_test <- NULL
    tryCatch({
      # Prepare data for meta-regression
      data$prop_transformed <- metafor::escalc(
        measure = "PFT",
        xi = round(data$resistance_rate * data$sample_size_ab),
        ni = data$sample_size_ab
      )$yi
      
      data$se <- 1 / sqrt(data$sample_size_ab)
      
      # Perform meta-regression with subgroup as moderator
      metareg <- metafor::rma(
        yi = prop_transformed,
        sei = se,
        mods = ~ as.factor(get(var)),
        method = method,
        data = data
      )
      
      # Test for subgroup differences
      subgroup_test <- list(
        QM = metareg$QM,
        QMp = metareg$QMp,
        R2 = metareg$R2,
        model = metareg
      )
    }, error = function(e) {
      warning("Could not test for subgroup differences in ", var, ": ", e$message)
    })
    
    # Create summary data frame
    summary_data <- data.frame(
      variable = var,
      value = sapply(var_results, function(x) x$value),
      k = sapply(var_results, function(x) x$k),
      pooled_rate = sapply(var_results, function(x) x$pooled_rate),
      ci_lower = sapply(var_results, function(x) x$ci_lower),
      ci_upper = sapply(var_results, function(x) x$ci_upper),
      I2 = sapply(var_results, function(x) {
        if (is.list(x$heterogeneity)) x$heterogeneity["I2"] else NA
      }),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    
    # Sort by pooled rate
    summary_data <- summary_data[order(summary_data$pooled_rate, decreasing = TRUE), ]
    
    # Add to results
    results$subgroups[[var]] <- list(
      values = var_results,
      subgroup_test = subgroup_test,
      summary = summary_data
    )
  }
  
  # Create overall summary
  all_summaries <- do.call(rbind, lapply(results$subgroups, function(x) x$summary))
  if (nrow(all_summaries) > 0) {
    results$summary <- all_summaries
  }
  
  return(results)
}

#' Perform sensitivity analysis
#'
#' Perform sensitivity analysis by sequentially omitting studies
#'
#' @param data A data frame containing AMR data
#' @param by Variables to group by for separate sensitivity analyses
#' @param method Meta-analysis method: "fixed" or "random"
#' @return A list containing sensitivity analysis results
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' sensitivity <- perform_sensitivity_analysis(
#'   human_amr,
#'   by = c("pathogen", "antibiotic")
#' )
#' }
perform_sensitivity_analysis <- function(data, by = NULL, method = "random") {
  # Check dependencies
  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("Package 'metafor' is required for this function")
  }
  
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Required columns
  required_cols <- c("resistance_rate", "sample_size_ab")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Initialize results list
  results <- list(
    overall = NULL,
    by_group = list(),
    method = method
  )
  
  # Define sensitivity analysis function
  run_sensitivity <- function(subset_data) {
    # Skip if not enough data
    if (nrow(subset_data) < 3) {
      return(list(
        leave_one_out = NULL,
        influence = NULL,
        warning = "Not enough data for sensitivity analysis (need at least 3 studies)"
      ))
    }
    
    # Transform proportions
    subset_data$prop_transformed <- metafor::escalc(
      measure = "PFT",
      xi = round(subset_data$resistance_rate * subset_data$sample_size_ab),
      ni = subset_data$sample_size_ab
    )$yi
    
    subset_data$se <- 1 / sqrt(subset_data$sample_size_ab)
    
    # Run full meta-analysis
    meta_result <- tryCatch({
      metafor::rma(
        yi = prop_transformed,
        sei = se,
        method = method,
        data = subset_data
      )
    }, error = function(e) {
      warning("Error in meta-analysis: ", e$message)
      return(NULL)
    })
    
    if (is.null(meta_result)) {
      return(list(
        leave_one_out = NULL,
        influence = NULL,
        warning = "Error in meta-analysis"
      ))
    }
    
    # Leave-one-out analysis
    loo_results <- tryCatch({
      metafor::leave1out(meta_result)
    }, error = function(e) {
      warning("Error in leave-one-out analysis: ", e$message)
      return(NULL)
    })
    
    # Influence diagnostics
    influence_results <- tryCatch({
      metafor::influence(meta_result)
    }, error = function(e) {
      warning("Error in influence analysis: ", e$message)
      return(NULL)
    })
    
    # Return results
    return(list(
      full_model = meta_result,
      leave_one_out = loo_results,
      influence = influence_results
    ))
  }
  
  # If no grouping variables, perform overall sensitivity analysis
  if (is.null(by) || length(by) == 0) {
    results$overall <- run_sensitivity(data)
    return(results)
  }
  
  # If grouping variables, perform sensitivity analysis for each group
  if (!all(by %in% colnames(data))) {
    stop("Not all grouping variables are in the data: ", 
         paste(setdiff(by, colnames(data)), collapse = ", "))
  }
  
  # Split data by groups
  groups <- unique(data[, by, drop = FALSE])
  
  # For each group, perform sensitivity analysis
  for (i in 1:nrow(groups)) {
    # Extract group values
    group_vals <- as.list(groups[i, ])
    
    # Create group identifier
    group_id <- paste(
      sapply(1:length(by), function(j) paste0(by[j], "=", group_vals[[by[j]]])),
      collapse = ", "
    )
    
    # Filter data for this group
    filter_expr <- paste(
      sapply(1:length(by), function(j) paste0("data$", by[j], " == '", group_vals[[by[j]]], "'")),
      collapse = " & "
    )
    group_data <- subset(data, eval(parse(text = filter_expr)))
    
    # Run sensitivity analysis for this group
    results$by_group[[group_id]] <- list(
      values = group_vals,
      sensitivity = run_sensitivity(group_data)
    )
  }
  
  return(results)
}

#' Compare with meta-analysis
#'
#' Compare local data with existing meta-analysis results
#'
#' @param local_data A data frame containing local AMR data
#' @param meta_results Meta-analysis results from calculate_pooled_rate
#' @param by Variables to group by for comparison
#' @return A list containing comparison results
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' 
#' # Generate meta-analysis results
#' meta_results <- calculate_pooled_rate(human_amr, by = c("pathogen", "antibiotic"))
#' 
#' # Compare local data with meta-analysis
#' local_data <- data.frame(
#'   pathogen = c("Ecoil", "KP"),
#'   antibiotic = c("CIP", "TET"),
#'   resistance_rate = c(0.65, 0.45),
#'   sample_size_ab = c(100, 80)
#' )
#' 
#' comparison <- compare_with_meta(local_data, meta_results, by = c("pathogen", "antibiotic"))
#' }
compare_with_meta <- function(local_data, meta_results, by) {
  # Validate inputs
  if (!is.data.frame(local_data)) {
    stop("local_data must be a data frame")
  }
  
  if (!is.list(meta_results) || is.null(meta_results$by_group)) {
    stop("meta_results must be the output from calculate_pooled_rate()")
  }
  
  if (!all(by %in% colnames(local_data))) {
    stop("Not all grouping variables are in local_data: ", 
         paste(setdiff(by, colnames(local_data)), collapse = ", "))
  }
  
  required_cols <- c("resistance_rate")
  missing_cols <- setdiff(required_cols, colnames(local_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in local_data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Initialize results
  results <- list(
    comparisons = list(),
    summary = NULL
  )
  
  # Prepare data for comparison
  comparison_data <- list()
  
  # For each row in local data, find matching meta-analysis group
  for (i in 1:nrow(local_data)) {
    local_row <- local_data[i, ]
    
    # Create group identifier
    group_vals <- local_row[, by, drop = FALSE]
    group_id <- paste(
      sapply(1:length(by), function(j) paste0(by[j], "=", group_vals[[by[j]]])),
      collapse = ", "
    )
    
    # Find matching meta-analysis group
    meta_group <- NULL
    for (group_name in names(meta_results$by_group)) {
      # Extract group values from meta-analysis
      meta_vals <- meta_results$by_group[[group_name]]$values
      
      # Check if all grouping variables match
      match <- TRUE
      for (var in by) {
        if (meta_vals[[var]] != local_row[[var]]) {
          match <- FALSE
          break
        }
      }
      
      if (match) {
        meta_group <- meta_results$by_group[[group_name]]
        break
      }
    }
    
    # If no matching group found, skip
    if (is.null(meta_group)) {
      warning("No matching meta-analysis group found for local data row ", i)
      next
    }
    
    # Calculate z-score and p-value for difference
    local_rate <- local_row$resistance_rate
    meta_rate <- meta_group$pooled_rate
    meta_ci_lower <- meta_group$ci_lower
    meta_ci_upper <- meta_group$ci_upper
    
    # Estimate standard error from confidence interval
    meta_se <- (meta_ci_upper - meta_ci_lower) / (2 * 1.96)
    
    # Standard error for local data (using Wilson score interval for proportions)
    n <- local_row$sample_size_ab
    if (is.null(n) || is.na(n)) {
      n <- 30  # Default sample size if not provided
      warning("sample_size_ab not provided for local data row ", i, ". Using default n=30.")
    }
    local_se <- sqrt((local_rate * (1 - local_rate)) / n)
    
    # Calculate z-score and p-value
    z_score <- (local_rate - meta_rate) / sqrt(meta_se^2 + local_se^2)
    p_value <- 2 * pnorm(-abs(z_score))
    
    # Determine if local rate is significantly different
    significant <- p_value < 0.05
    direction <- if (local_rate > meta_rate) "higher" else if (local_rate < meta_rate) "lower" else "same"
    
    # Store comparison
    comparison <- list(
      local = list(
        rate = local_rate,
        se = local_se,
        sample_size = n
      ),
      meta = list(
        rate = meta_rate,
        ci_lower = meta_ci_lower,
        ci_upper = meta_ci_upper,
        se = meta_se,
        k = meta_group$k
      ),
      difference = list(
        absolute = local_rate - meta_rate,
        percentage = (local_rate - meta_rate) / meta_rate * 100,
        z_score = z_score,
        p_value = p_value,
        significant = significant,
        direction = direction
      ),
      group_values = as.list(group_vals)
    )
    
    # Add to comparison data
    comparison_data[[length(comparison_data) + 1]] <- comparison
  }
  
  # Store all comparisons
  results$comparisons <- comparison_data
  
  # Create summary data frame
  if (length(comparison_data) > 0) {
    summary_df <- data.frame(
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    
    # Add group variables
    for (var in by) {
      summary_df[[var]] <- sapply(comparison_data, function(x) x$group_values[[var]])
    }
    
    # Add comparison metrics
    summary_df$local_rate <- sapply(comparison_data, function(x) x$local$rate)
    summary_df$meta_rate <- sapply(comparison_data, function(x) x$meta$rate)
    summary_df$absolute_diff <- sapply(comparison_data, function(x) x$difference$absolute)
    summary_df$percentage_diff <- sapply(comparison_data, function(x) x$difference$percentage)
    summary_df$z_score <- sapply(comparison_data, function(x) x$difference$z_score)
    summary_df$p_value <- sapply(comparison_data, function(x) x$difference$p_value)
    summary_df$significant <- sapply(comparison_data, function(x) x$difference$significant)
    summary_df$direction <- sapply(comparison_data, function(x) x$difference$direction)
    
    # Sort by absolute difference
    summary_df <- summary_df[order(abs(summary_df$absolute_diff), decreasing = TRUE), ]
    
    results$summary <- summary_df
  }
  
  return(results)
} 