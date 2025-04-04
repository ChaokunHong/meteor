#' Create forest plot
#'
#' Creates a forest plot from meta-analysis results
#'
#' @param meta_result Meta-analysis result from calculate_pooled_rate or similar
#' @param title Plot title
#' @param xlab X-axis label
#' @param show_study_labels Logical; whether to show study labels
#' @param text_size Base text size for the plot
#' @param ... Additional arguments passed to meta::forest
#'
#' @return A meta forest plot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create forest plot from meta-analysis results
#' forest_plot <- create_forest_plot(meta_results)
#' }
create_forest_plot <- function(meta_result, title = NULL, xlab = "Resistance Rate", 
                             show_study_labels = TRUE, text_size = 12, ...) {
  # Check for meta package
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("Package 'meta' is required for this function. Please install it.")
  }
  
  # Validate input
  if (is.null(meta_result)) {
    stop("Meta-analysis result is NULL")
  }
  
  # Get meta object
  if (inherits(meta_result, "meta")) {
    meta_obj <- meta_result
  } else if (inherits(meta_result, "list") && !is.null(meta_result$overall) && 
             inherits(meta_result$overall, "meta")) {
    meta_obj <- meta_result$overall
  } else {
    stop("Input is not a valid meta-analysis result")
  }
  
  # Set default title if not provided
  if (is.null(title)) {
    title <- "Forest Plot of Antimicrobial Resistance Rates"
  }
  
  # Create forest plot
  forest_plot <- meta::forest(
    meta_obj,
    leftcols = if (show_study_labels) c("studlab", "event", "n", "effect", "ci") else c("event", "n", "effect", "ci"),
    rightcols = c("w.random"),
    fontsize = text_size,
    xlab = xlab,
    title = title,
    prediction = TRUE,
    ...
  )
  
  return(invisible(forest_plot))
}

#' Create interactive forest plot
#'
#' Creates an interactive forest plot using plotly
#'
#' @param meta_result Meta-analysis result from calculate_pooled_rate or similar
#' @param title Plot title
#' @param xlab X-axis label
#' @param color_scheme Color scheme for the plot
#' @param sort_by How to sort the studies (none, year, effect, precision)
#' @param width Plot width in pixels
#' @param height Plot height in pixels
#'
#' @return A plotly interactive plot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create interactive forest plot
#' interactive_forest <- create_interactive_forest(meta_results)
#' }
create_interactive_forest <- function(meta_result, title = "Forest Plot of Antimicrobial Resistance Rates", 
                                   xlab = "Resistance Rate", color_scheme = "viridis",
                                   sort_by = c("none", "year", "effect", "precision"),
                                   width = 800, height = 500) {
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function. Please install it.")
  }
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for this function. Please install it.")
  }
  
  sort_by <- match.arg(sort_by)
  
  # Validate input
  if (is.null(meta_result)) {
    stop("Meta-analysis result is NULL")
  }
  
  # Get meta object
  if (inherits(meta_result, "meta")) {
    meta_obj <- meta_result
  } else if (inherits(meta_result, "list") && !is.null(meta_result$overall) && 
             inherits(meta_result$overall, "meta")) {
    meta_obj <- meta_result$overall
  } else {
    stop("Input is not a valid meta-analysis result")
  }
  
  # Extract data for plotting
  data <- data.frame(
    study = meta_obj$studlab,
    year = if (!is.null(meta_obj$data$year)) meta_obj$data$year else NA,
    effect = meta_obj$TE,
    lower = meta_obj$lower,
    upper = meta_obj$upper,
    weight = meta_obj$w.random,
    n_events = meta_obj$event,
    n_total = meta_obj$n,
    stringsAsFactors = FALSE
  )
  
  # Sort data if requested
  if (sort_by == "year" && !all(is.na(data$year))) {
    data <- data[order(data$year), ]
  } else if (sort_by == "effect") {
    data <- data[order(data$effect), ]
  } else if (sort_by == "precision") {
    data <- data[order(data$upper - data$lower), ]
  }
  
  # Add overall/pooled effect
  data_with_overall <- rbind(
    data,
    data.frame(
      study = "Overall (Random)",
      year = NA,
      effect = meta_obj$TE.random,
      lower = meta_obj$lower.random,
      upper = meta_obj$upper.random,
      weight = 100,
      n_events = sum(meta_obj$event),
      n_total = sum(meta_obj$n),
      stringsAsFactors = FALSE
    )
  )
  
  # Add prediction interval if available
  if (!is.null(meta_obj$lower.predict) && !is.null(meta_obj$upper.predict)) {
    data_with_overall <- rbind(
      data_with_overall,
      data.frame(
        study = "Prediction Interval",
        year = NA,
        effect = meta_obj$TE.random,
        lower = meta_obj$lower.predict,
        upper = meta_obj$upper.predict,
        weight = NA,
        n_events = NA,
        n_total = NA,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Reverse order for plotting (to have studies at the top)
  data_with_overall$study <- factor(data_with_overall$study, 
                                   levels = rev(data_with_overall$study))
  
  # Create formatted text for hover
  data_with_overall$hover_text <- paste0(
    "Study: ", data_with_overall$study, "<br>",
    "Effect: ", round(data_with_overall$effect * 100, 1), "% (", 
    round(data_with_overall$lower * 100, 1), "% - ", 
    round(data_with_overall$upper * 100, 1), "%)<br>",
    "Events: ", data_with_overall$n_events, "/", data_with_overall$n_total, "<br>",
    "Weight: ", round(data_with_overall$weight, 1), "%"
  )
  
  # Identify the overall and prediction interval rows
  is_overall <- data_with_overall$study %in% c("Overall (Random)", "Prediction Interval")
  
  # Create base plot
  p <- ggplot2::ggplot(data_with_overall, ggplot2::aes(x = effect, y = study, xmin = lower, xmax = upper)) +
    # Add study effect sizes and CIs
    ggplot2::geom_errorbarh(ggplot2::aes(height = 0.2, text = hover_text)) +
    ggplot2::geom_point(ggplot2::aes(size = weight, text = hover_text), shape = 15, 
                       color = ifelse(is_overall, "red", "blue")) +
    # Add reference line at 0
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
    # Customize appearance
    ggplot2::labs(title = title, x = xlab, y = "") +
    ggplot2::scale_x_continuous(labels = scales::percent_format(), 
                              limits = c(0, 1)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(hjust = 0),
      legend.position = "none"
    )
  
  # Convert to interactive plotly plot
  interactive_plot <- plotly::ggplotly(p, tooltip = "text", width = width, height = height)
  
  return(interactive_plot)
}

#' Create subgroup forest plot
#'
#' Creates a forest plot showing subgroup analysis results
#'
#' @param subgroup_results Subgroup meta-analysis result from perform_subgroup_analysis
#' @param title Plot title
#' @param xlab X-axis label
#' @param ... Additional arguments passed to meta::forest
#'
#' @return A meta forest plot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create forest plot showing subgroup results
#' subgroup_forest <- create_subgroup_forest(subgroup_results)
#' }
create_subgroup_forest <- function(subgroup_results, title = "Subgroup Analysis", 
                                xlab = "Resistance Rate", ...) {
  # Check for meta package
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("Package 'meta' is required for this function. Please install it.")
  }
  
  # Validate input
  if (!is.list(subgroup_results) || is.null(subgroup_results$subgroups)) {
    stop("Input is not a valid subgroup analysis result")
  }
  
  # Extract subgroup meta-analysis objects
  subgroup_meta_list <- list()
  subgroup_names <- names(subgroup_results$subgroups)
  
  for (name in subgroup_names) {
    if (!is.null(subgroup_results$subgroups[[name]]$overall)) {
      subgroup_meta_list[[name]] <- subgroup_results$subgroups[[name]]$overall
    }
  }
  
  if (length(subgroup_meta_list) == 0) {
    stop("No valid subgroup results found")
  }
  
  # Create a grouped forest plot
  forest_plot <- meta::forest(
    subgroup_meta_list,
    leftcols = c("studlab", "effect", "ci"),
    rightcols = c("w.random"),
    text.random = "Subgroup Summary",
    overall = TRUE,
    overall.hetstat = TRUE,
    test.subgroup.random = TRUE,
    title = title,
    xlab = xlab,
    ...
  )
  
  return(invisible(forest_plot))
}

#' Create cumulative forest plot
#'
#' Creates a cumulative forest plot to show the evolution of the effect size as studies are added
#'
#' @param meta_result Meta-analysis result from calculate_pooled_rate or similar
#' @param order_by Variable to order studies by (typically "year")
#' @param title Plot title
#' @param xlab X-axis label
#' @param ... Additional arguments passed to meta::forest
#'
#' @return A meta forest plot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Create cumulative forest plot ordered by year
#' cumulative_forest <- create_cumulative_forest(meta_results, order_by = "year")
#' }
create_cumulative_forest <- function(meta_result, order_by = "year", 
                                  title = "Cumulative Meta-Analysis", 
                                  xlab = "Resistance Rate", ...) {
  # Check for meta package
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("Package 'meta' is required for this function. Please install it.")
  }
  
  # Validate input
  if (is.null(meta_result)) {
    stop("Meta-analysis result is NULL")
  }
  
  # Get meta object
  if (inherits(meta_result, "meta")) {
    meta_obj <- meta_result
  } else if (inherits(meta_result, "list") && !is.null(meta_result$overall) && 
             inherits(meta_result$overall, "meta")) {
    meta_obj <- meta_result$overall
  } else {
    stop("Input is not a valid meta-analysis result")
  }
  
  # Check for order_by variable in data
  if (!order_by %in% names(meta_obj$data) && order_by != "none") {
    warning("Order variable '", order_by, "' not found in data. Using default order.")
    order_by <- "none"
  }
  
  # Create cumulative meta-analysis
  if (order_by == "none") {
    # Use default order
    cumulative_result <- meta::metacum(meta_obj)
  } else {
    # Sort by the specified variable
    sorted_indices <- order(meta_obj$data[[order_by]])
    cumulative_result <- meta::metacum(meta_obj, sortvar = meta_obj$data[[order_by]])
  }
  
  # Create forest plot of cumulative meta-analysis
  forest_plot <- meta::forest(
    cumulative_result,
    leftcols = c("studlab", "effect", "ci"),
    rightcols = c("I2"),
    fontsize = 10,
    title = title,
    xlab = xlab,
    ...
  )
  
  return(invisible(forest_plot))
} 