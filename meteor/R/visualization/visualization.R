#' Create a forest plot
#'
#' Create a forest plot for meta-analysis results
#'
#' @param meta_results Results from calculate_pooled_rate function
#' @param groups Vector of group names to include (NULL for all)
#' @param sort_by Variable to sort by: "effect" (default), "name", or "precision"
#' @param text_size Base text size for the plot
#' @param interactive Whether to create an interactive plot (requires plotly)
#' @return A ggplot2 object or plotly object if interactive=TRUE
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' results <- calculate_pooled_rate(human_amr, by = c("pathogen", "antibiotic"))
#' forest_plot <- create_forest_plot(results)
#' }
create_forest_plot <- function(meta_results, 
                             groups = NULL,
                             sort_by = "effect",
                             text_size = 12,
                             interactive = FALSE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package 'plotly' is required for interactive plots. Falling back to static plot.")
    interactive <- FALSE
  }
  
  # Check if meta_results is valid
  if (!is.list(meta_results) || (!is.null(meta_results$overall) && is.null(meta_results$by_group))) {
    # Create forest plot using metafor's forest function if it's a single meta-analysis
    if (!is.null(meta_results$overall) && !is.null(meta_results$overall$model)) {
      if (!requireNamespace("metafor", quietly = TRUE)) {
        stop("Package 'metafor' is required for this function")
      }
      
      # Use metafor's forest function and capture the plot
      forest_plot <- metafor::forest(
        meta_results$overall$model,
        transf = metafor::transf.ipft,
        refline = meta_results$overall$pooled_rate,
        xlab = "Proportion",
        header = "Study"
      )
      
      # Return the plot
      return(forest_plot)
    } else {
      stop("Invalid meta_results object. Must be output from calculate_pooled_rate function.")
    }
  }
  
  # Extract summary data from meta_results
  if (is.null(meta_results$summary)) {
    stop("No summary data found in meta_results")
  }
  
  # Filter groups if specified
  plot_data <- meta_results$summary
  if (!is.null(groups)) {
    plot_data <- plot_data[plot_data$group %in% groups, ]
    if (nrow(plot_data) == 0) {
      stop("No data left after filtering groups")
    }
  }
  
  # Sort data
  if (sort_by == "effect") {
    plot_data <- plot_data[order(plot_data$pooled_rate, decreasing = TRUE), ]
  } else if (sort_by == "name") {
    plot_data <- plot_data[order(plot_data$group), ]
  } else if (sort_by == "precision") {
    # Calculate precision (inverse of CI width)
    plot_data$precision <- 1 / (plot_data$ci_upper - plot_data$ci_lower)
    plot_data <- plot_data[order(plot_data$precision, decreasing = TRUE), ]
  }
  
  # Create forest plot with ggplot2
  library(ggplot2)
  
  # Add nice labels with study count
  plot_data$label <- paste0(plot_data$group, " (k=", plot_data$k, ")")
  
  # Ensure proper factor order for plotting
  plot_data$label <- factor(plot_data$label, levels = rev(plot_data$label))
  
  # Create the plot
  p <- ggplot(plot_data, aes(y = label, x = pooled_rate)) +
    geom_point(aes(size = k), shape = 15) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
    geom_vline(xintercept = mean(plot_data$pooled_rate, na.rm = TRUE), 
              linetype = "dashed", color = "gray50") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      title = "Forest Plot of Antimicrobial Resistance Rates",
      subtitle = paste("Based on", sum(plot_data$k, na.rm = TRUE), "studies"),
      x = "Resistance Rate",
      y = NULL,
      size = "Studies"
    ) +
    theme_minimal(base_size = text_size) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom"
    )
  
  # Make interactive if requested
  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = c("y", "x", "xmin", "xmax", "size"))
  }
  
  return(p)
}

#' Create an interactive forest plot
#'
#' Create an interactive forest plot for meta-analysis results
#'
#' @param meta_results Results from calculate_pooled_rate function
#' @param groups Vector of group names to include (NULL for all)
#' @param sort_by Variable to sort by: "effect" (default), "name", or "precision"
#' @param width Plot width in pixels
#' @param height Plot height in pixels
#' @return A plotly object
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' results <- calculate_pooled_rate(human_amr, by = c("pathogen", "antibiotic"))
#' interactive_forest <- create_interactive_forest(results)
#' }
create_interactive_forest <- function(meta_results, 
                                    groups = NULL,
                                    sort_by = "effect",
                                    width = 800,
                                    height = 600) {
  # Create interactive forest plot
  p <- create_forest_plot(meta_results, groups, sort_by, interactive = TRUE)
  
  # Set dimensions
  p <- plotly::layout(p, 
                     width = width, 
                     height = height,
                     showlegend = TRUE)
  
  return(p)
}

#' Create a geographic map
#'
#' Create a geographic map of antimicrobial resistance rates
#'
#' @param data A data frame containing AMR data
#' @param by Variables to group by for calculating rates by location
#' @param location_var Name of the location variable
#' @param color_palette Color palette to use
#' @param interactive Whether to create an interactive map (requires leaflet)
#' @return A ggplot2 object or leaflet object if interactive=TRUE
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' geo_map <- create_geo_map(human_amr, by = c("pathogen", "antibiotic"))
#' }
create_geo_map <- function(data, 
                         by = NULL,
                         location_var = "location",
                         color_palette = "viridis",
                         interactive = FALSE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  if (interactive && !requireNamespace("leaflet", quietly = TRUE)) {
    warning("Package 'leaflet' is required for interactive maps. Falling back to static map.")
    interactive <- FALSE
  }
  
  # Ensure location variable exists
  if (!(location_var %in% colnames(data))) {
    stop("Location variable '", location_var, "' not found in data")
  }
  
  # Ensure resistance_rate exists
  if (!("resistance_rate" %in% colnames(data))) {
    stop("resistance_rate variable not found in data")
  }
  
  # Add grouping variables if specified
  group_vars <- location_var
  if (!is.null(by)) {
    if (!all(by %in% colnames(data))) {
      stop("Not all grouping variables are in the data: ", 
           paste(setdiff(by, colnames(data)), collapse = ", "))
    }
    group_vars <- c(group_vars, by)
  }
  
  # Aggregate data by location and grouping variables
  # Create formula for aggregation
  grp_formula <- as.formula(paste("~", paste(group_vars, collapse = "+")))
  
  # Calculate mean resistance rate
  rates <- aggregate(data$resistance_rate, grp_formula, FUN = mean, data = data, na.rm = TRUE)
  colnames(rates)[ncol(rates)] <- "mean_resistance_rate"
  
  # Calculate sample size
  counts <- aggregate(data$resistance_rate, grp_formula, FUN = length, data = data)
  colnames(counts)[ncol(counts)] <- "sample_size"
  
  # Merge results
  map_data <- merge(rates, counts)
  
  # Sort by mean resistance rate
  map_data <- map_data[order(map_data$mean_resistance_rate, decreasing = TRUE), ]
  
  # Create static map using ggplot2
  if (!interactive) {
    # Note: For a full implementation, you'd need a country/location mapping
    # For this example, we'll create a simple bar chart by location
    library(ggplot2)
    
    # Ensure location is a factor with levels ordered by resistance rate
    map_data[[location_var]] <- factor(
      map_data[[location_var]],
      levels = unique(map_data[[location_var]][order(map_data$mean_resistance_rate)])
    )
    
    # Create the plot
    p <- ggplot(map_data, aes_string(x = location_var, y = "mean_resistance_rate", 
                                   fill = "mean_resistance_rate")) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c(name = "Resistance Rate", option = color_palette) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(
        title = "Geographic Distribution of Antimicrobial Resistance",
        x = "Location",
        y = "Mean Resistance Rate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
    
    return(p)
  } else {
    # Interactive map with leaflet
    # Note: For a full implementation, you would need geocoding to convert country names to coordinates
    # This is a placeholder implementation
    library(leaflet)
    
    # Create a simple leaflet map
    # In a real implementation, you would join with a geospatial dataset
    m <- leaflet() %>%
      addTiles() %>%
      addLegend(
        position = "bottomright",
        pal = leaflet::colorNumeric(palette = color_palette, domain = c(0, 1)),
        values = map_data$mean_resistance_rate,
        title = "Resistance Rate"
      )
    
    return(m)
  }
}

#' Create a resistance heatmap
#'
#' Create a heatmap of antimicrobial resistance rates
#'
#' @param data A data frame containing AMR data or meta-analysis results
#' @param x_var Variable for x-axis (default: "pathogen")
#' @param y_var Variable for y-axis (default: "antibiotic")
#' @param fill_var Variable for cell fill (default: "resistance_rate")
#' @param color_palette Color palette to use
#' @param interactive Whether to create an interactive heatmap (requires plotly)
#' @return A ggplot2 object or plotly object if interactive=TRUE
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' heatmap <- create_resistance_heatmap(human_amr)
#' }
create_resistance_heatmap <- function(data, 
                                    x_var = "pathogen",
                                    y_var = "antibiotic",
                                    fill_var = "resistance_rate",
                                    color_palette = "viridis",
                                    interactive = FALSE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package 'plotly' is required for interactive heatmaps. Falling back to static heatmap.")
    interactive <- FALSE
  }
  
  # Check if input is meta-analysis results or data frame
  if (is.list(data) && !is.data.frame(data) && !is.null(data$summary)) {
    # Extract summary data from meta-analysis results
    plot_data <- data$summary
    
    # Check if necessary variables exist
    if (!(x_var %in% colnames(plot_data))) {
      stop("Variable '", x_var, "' not found in meta-analysis results")
    }
    if (!(y_var %in% colnames(plot_data))) {
      stop("Variable '", y_var, "' not found in meta-analysis results")
    }
    
    # Update fill_var if it's not resistance_rate
    if (!(fill_var %in% colnames(plot_data))) {
      fill_var <- "pooled_rate"
      if (!(fill_var %in% colnames(plot_data))) {
        stop("Neither '", fill_var, "' nor 'pooled_rate' found in meta-analysis results")
      }
    }
  } else if (is.data.frame(data)) {
    # Use data frame directly
    plot_data <- data
    
    # Check if necessary variables exist
    if (!(x_var %in% colnames(plot_data))) {
      stop("Variable '", x_var, "' not found in data")
    }
    if (!(y_var %in% colnames(plot_data))) {
      stop("Variable '", y_var, "' not found in data")
    }
    if (!(fill_var %in% colnames(plot_data))) {
      stop("Variable '", fill_var, "' not found in data")
    }
    
    # Aggregate data if there are duplicate combinations
    if (length(unique(paste(plot_data[[x_var]], plot_data[[y_var]]))) < nrow(plot_data)) {
      grp_formula <- as.formula(paste("~", paste(c(x_var, y_var), collapse = "+")))
      agg_data <- aggregate(plot_data[[fill_var]], grp_formula, FUN = mean, data = plot_data, na.rm = TRUE)
      colnames(agg_data)[ncol(agg_data)] <- fill_var
      plot_data <- agg_data
    }
  } else {
    stop("Input must be a data frame or meta-analysis results from calculate_pooled_rate")
  }
  
  # Create heatmap with ggplot2
  library(ggplot2)
  
  # Create the plot
  p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(
      name = "Resistance Rate",
      option = color_palette,
      limits = c(0, 1),
      labels = scales::percent
    ) +
    labs(
      title = "Antimicrobial Resistance Heatmap",
      x = x_var,
      y = y_var
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  # Add text labels with resistance rates
  p <- p + geom_text(
    aes_string(label = paste0("scales::percent(", fill_var, ", accuracy = 1)")),
    color = "white",
    size = 3
  )
  
  # Make interactive if requested
  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = c(x_var, y_var, fill_var))
  }
  
  return(p)
}

#' Create a trend plot
#'
#' Create a plot showing antimicrobial resistance trends over time
#'
#' @param data A data frame containing AMR data
#' @param by Variables to group by for separate trend lines
#' @param time_var Name of the time variable (default: "year_published")
#' @param color_var Variable to use for line colors (optional)
#' @param interactive Whether to create an interactive plot (requires plotly)
#' @return A ggplot2 object or plotly object if interactive=TRUE
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' trend_plot <- create_trend_plot(human_amr, by = "pathogen")
#' }
create_trend_plot <- function(data, 
                            by = NULL,
                            time_var = "year_published",
                            color_var = NULL,
                            interactive = FALSE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package 'plotly' is required for interactive plots. Falling back to static plot.")
    interactive <- FALSE
  }
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!(time_var %in% colnames(data))) {
    stop("Time variable '", time_var, "' not found in data")
  }
  
  if (!("resistance_rate" %in% colnames(data))) {
    stop("resistance_rate variable not found in data")
  }
  
  # Process grouping variables
  group_vars <- time_var
  if (!is.null(by)) {
    if (!all(by %in% colnames(data))) {
      stop("Not all grouping variables are in the data: ", 
           paste(setdiff(by, colnames(data)), collapse = ", "))
    }
    group_vars <- c(group_vars, by)
  }
  
  # Set color variable if specified
  if (is.null(color_var) && !is.null(by) && length(by) > 0) {
    color_var <- by[1]
  }
  
  # Aggregate data by time and grouping variables
  grp_formula <- as.formula(paste("~", paste(group_vars, collapse = "+")))
  rates <- aggregate(data$resistance_rate, grp_formula, FUN = mean, data = data, na.rm = TRUE)
  colnames(rates)[ncol(rates)] <- "mean_resistance_rate"
  
  # Calculate confidence intervals (if enough data points)
  ci_data <- NULL
  tryCatch({
    if (length(unique(data[[time_var]])) >= 3) {
      # Calculate standard error
      se_data <- aggregate(data$resistance_rate, grp_formula, 
                          FUN = function(x) sd(x, na.rm = TRUE) / sqrt(length(x)))
      colnames(se_data)[ncol(se_data)] <- "se"
      
      # Merge with rates
      ci_data <- merge(rates, se_data)
      
      # Calculate 95% CI
      ci_data$ci_lower <- ci_data$mean_resistance_rate - 1.96 * ci_data$se
      ci_data$ci_upper <- ci_data$mean_resistance_rate + 1.96 * ci_data$se
      
      # Clip CI to valid range
      ci_data$ci_lower <- pmax(ci_data$ci_lower, 0)
      ci_data$ci_upper <- pmin(ci_data$ci_upper, 1)
    }
  }, error = function(e) {
    warning("Could not calculate confidence intervals: ", e$message)
  })
  
  # Create trend plot with ggplot2
  library(ggplot2)
  
  # Base plot
  p <- ggplot(rates, aes_string(x = time_var, y = "mean_resistance_rate"))
  
  # Add color if specified
  if (!is.null(color_var)) {
    p <- p + aes_string(color = color_var, group = color_var)
  }
  
  # Add confidence intervals if available
  if (!is.null(ci_data)) {
    if (!is.null(color_var)) {
      p <- p + geom_ribbon(
        data = ci_data,
        aes_string(ymin = "ci_lower", ymax = "ci_upper", fill = color_var, color = NULL),
        alpha = 0.2
      )
    } else {
      p <- p + geom_ribbon(
        data = ci_data,
        aes(ymin = ci_lower, ymax = ci_upper),
        alpha = 0.2,
        fill = "gray70"
      )
    }
  }
  
  # Add lines and points
  p <- p +
    geom_line() +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      title = "Antimicrobial Resistance Trends Over Time",
      x = "Year",
      y = "Mean Resistance Rate",
      color = if (!is.null(color_var)) color_var else NULL,
      fill = if (!is.null(color_var)) color_var else NULL
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Make interactive if requested
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
}

#' Create a comparison plot
#'
#' Create a plot comparing antimicrobial resistance rates between groups
#'
#' @param data A data frame containing AMR data or comparison results
#' @param compare_var Variable to use for comparison (e.g., "domain", "location")
#' @param group_var Variable to use for grouping (e.g., "pathogen", "antibiotic")
#' @param fill_var Variable to use for fill color (default: "resistance_rate")
#' @param interactive Whether to create an interactive plot (requires plotly)
#' @return A ggplot2 object or plotly object if interactive=TRUE
#' @export
#' @examples
#' \dontrun{
#' data(human_amr)
#' comparison_plot <- create_comparison_plot(human_amr, 
#'                                         compare_var = "location", 
#'                                         group_var = "pathogen")
#' }
create_comparison_plot <- function(data, 
                                 compare_var,
                                 group_var,
                                 fill_var = "resistance_rate",
                                 interactive = FALSE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  if (interactive && !requireNamespace("plotly", quietly = TRUE)) {
    warning("Package 'plotly' is required for interactive plots. Falling back to static plot.")
    interactive <- FALSE
  }
  
  # Check if input is comparison results or data frame
  if (is.list(data) && !is.data.frame(data) && !is.null(data$summary)) {
    # Extract summary data from comparison results
    plot_data <- data$summary
    
    # Check if necessary variables exist
    if (!(compare_var %in% colnames(plot_data))) {
      stop("Variable '", compare_var, "' not found in comparison results")
    }
    if (!(group_var %in% colnames(plot_data))) {
      stop("Variable '", group_var, "' not found in comparison results")
    }
    
    # Update fill_var for comparison results
    if (fill_var == "resistance_rate" && !("resistance_rate" %in% colnames(plot_data))) {
      # Try to find appropriate column
      if ("local_rate" %in% colnames(plot_data) && "meta_rate" %in% colnames(plot_data)) {
        # Create a long-format data frame for comparison
        library(tidyr)
        plot_data <- tidyr::pivot_longer(
          plot_data,
          cols = c("local_rate", "meta_rate"),
          names_to = compare_var,
          values_to = "resistance_rate"
        )
        fill_var <- "resistance_rate"
      } else {
        stop("Could not find appropriate resistance rate variables in comparison results")
      }
    }
  } else if (is.data.frame(data)) {
    # Use data frame directly
    plot_data <- data
    
    # Check if necessary variables exist
    if (!(compare_var %in% colnames(plot_data))) {
      stop("Variable '", compare_var, "' not found in data")
    }
    if (!(group_var %in% colnames(plot_data))) {
      stop("Variable '", group_var, "' not found in data")
    }
    if (!(fill_var %in% colnames(plot_data))) {
      stop("Variable '", fill_var, "' not found in data")
    }
    
    # Aggregate data
    grp_formula <- as.formula(paste("~", paste(c(compare_var, group_var), collapse = "+")))
    agg_data <- aggregate(plot_data[[fill_var]], grp_formula, FUN = mean, data = plot_data, na.rm = TRUE)
    colnames(agg_data)[ncol(agg_data)] <- fill_var
    plot_data <- agg_data
  } else {
    stop("Input must be a data frame or comparison results")
  }
  
  # Create comparison plot with ggplot2
  library(ggplot2)
  
  # Determine plot type based on number of unique values
  n_compare <- length(unique(plot_data[[compare_var]]))
  n_group <- length(unique(plot_data[[group_var]]))
  
  if (n_compare <= 5 && n_group <= 10) {
    # Use grouped bar chart
    p <- ggplot(plot_data, aes_string(x = group_var, y = fill_var, fill = compare_var)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_fill_brewer(palette = "Set1") +
      labs(
        title = "Comparison of Antimicrobial Resistance Rates",
        x = group_var,
        y = "Resistance Rate",
        fill = compare_var
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  } else if (n_compare <= 10 && n_group > 10) {
    # Use faceted approach for many groups
    p <- ggplot(plot_data, aes_string(x = group_var, y = fill_var, fill = compare_var)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_fill_brewer(palette = "Set1") +
      facet_wrap(~ get(compare_var), scales = "free_x") +
      labs(
        title = "Comparison of Antimicrobial Resistance Rates",
        x = group_var,
        y = "Resistance Rate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(face = "bold")
      )
  } else {
    # Use heatmap for many values
    p <- ggplot(plot_data, aes_string(x = group_var, y = compare_var, fill = fill_var)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(
        name = "Resistance Rate",
        limits = c(0, 1),
        labels = scales::percent
      ) +
      labs(
        title = "Comparison of Antimicrobial Resistance Rates",
        x = group_var,
        y = compare_var
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        panel.grid = element_blank()
      )
  }
  
  # Make interactive if requested
  if (interactive) {
    p <- plotly::ggplotly(p)
  }
  
  return(p)
} 