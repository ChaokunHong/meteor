#' Get the meteor package version
#'
#' @return A character string indicating the current version of the meteor package
#' @export
#' @examples
#' meteor_version()
meteor_version <- function() {
  pkg_version <- utils::packageVersion("meteor")
  return(as.character(pkg_version))
}

#' Initialize a meteor project
#'
#' Creates a directory structure for a new meteor project
#'
#' @param path Path where the project should be created
#' @param project_name Name of the project
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @return Invisibly returns the path to the created project
#' @export
#' @examples
#' \dontrun{
#' initialize_meteor_project("path/to/projects", "my_amr_analysis")
#' }
initialize_meteor_project <- function(path, project_name, overwrite = FALSE) {
  # Create full project path
  project_path <- file.path(path, project_name)
  
  # Check if directory exists
  if (dir.exists(project_path) && !overwrite) {
    stop("Project directory already exists. Set overwrite = TRUE to overwrite.")
  }
  
  # Create main directory
  dir.create(project_path, recursive = TRUE, showWarnings = FALSE)
  
  # Create subdirectories
  dirs <- c(
    "data/raw",
    "data/processed",
    "analysis",
    "reports",
    "figures"
  )
  
  for (d in dirs) {
    dir.create(file.path(project_path, d), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create README file
  readme_content <- paste0(
    "# ", project_name, "\n\n",
    "Project created with meteor version ", meteor_version(), "\n\n",
    "## Structure\n\n",
    "- data/raw: Place raw data files here\n",
    "- data/processed: Processed data will be stored here\n",
    "- analysis: R scripts for analysis\n",
    "- reports: Generated reports\n",
    "- figures: Saved visualizations\n\n",
    "## Usage\n\n",
    "```r\n",
    "library(meteor)\n",
    "# Import data\n",
    "data <- import_amr_data('data/raw/your_data.csv')\n",
    "# More analysis steps...\n",
    "```\n"
  )
  
  writeLines(readme_content, file.path(project_path, "README.md"))
  
  # Create example analysis script
  example_script <- c(
    "# Example AMR analysis with meteor",
    "",
    "library(meteor)",
    "",
    "# Import data",
    "# data <- import_amr_data('data/raw/your_data.csv')",
    "",
    "# Use built-in data",
    "data(human_amr)",
    "",
    "# Validate data",
    "validated_data <- validate_data(human_amr)",
    "",
    "# Run meta-analysis",
    "meta_results <- calculate_pooled_rate(",
    "  data = validated_data,",
    "  by = c('pathogen', 'antibiotic'),",
    "  method = 'random'",
    ")",
    "",
    "# Create visualization",
    "forest_plot <- create_forest_plot(meta_results)",
    "",
    "# Save visualization",
    "# save_plot(forest_plot, 'figures/forest_plot.png')",
    "",
    "# Generate report",
    "# generate_summary_report(",
    "#   data = validated_data,",
    "#   results = meta_results,",
    "#   plots = list(forest_plot),",
    "#   output_file = 'reports/summary_report.html'",
    "# )"
  )
  
  writeLines(example_script, file.path(project_path, "analysis", "example_analysis.R"))
  
  message("Project created at: ", project_path)
  return(invisible(project_path))
}

#' Set meteor options
#'
#' Set global options for the meteor package
#'
#' @param ... Named options to set
#' @return Invisibly returns the updated options
#' @export
#' @examples
#' set_meteor_options(forest_plot_height = 800, map_color_scheme = "viridis")
set_meteor_options <- function(...) {
  # Define the option name prefix
  prefix <- "meteor."
  
  # Get the options passed to the function
  opts <- list(...)
  
  # Check if options are named
  if (length(opts) > 0 && is.null(names(opts))) {
    stop("All options must be named.")
  }
  
  # Set each option
  for (i in seq_along(opts)) {
    opt_name <- names(opts)[i]
    opt_value <- opts[[i]]
    full_name <- paste0(prefix, opt_name)
    options(setNames(list(opt_value), full_name))
  }
  
  # Return all meteor options
  return(invisible(get_meteor_options()))
}

#' Get meteor options
#'
#' Get current global options for the meteor package
#'
#' @param option_name Optional name of a specific option to retrieve
#' @return A list of all meteor options or a single option value
#' @export
#' @examples
#' get_meteor_options()
#' get_meteor_options("forest_plot_height")
get_meteor_options <- function(option_name = NULL) {
  # Define the option name prefix
  prefix <- "meteor."
  
  # Get all options
  all_options <- options()
  
  # Filter for meteor options
  meteor_options <- all_options[grep(paste0("^", prefix), names(all_options))]
  
  # Remove the prefix from option names
  names(meteor_options) <- gsub(prefix, "", names(meteor_options))
  
  # Return specific option if requested
  if (!is.null(option_name)) {
    full_name <- paste0(prefix, option_name)
    return(getOption(full_name))
  }
  
  return(meteor_options)
}

#' Check package dependencies
#'
#' Check if all required packages for a specific functionality are installed
#'
#' @param feature Feature to check dependencies for (e.g., "visualization", "analysis")
#' @param quietly If TRUE, suppresses messages about installed packages
#' @return Logical indicating whether all dependencies are installed
#' @export
#' @examples
#' check_dependencies("visualization")
check_dependencies <- function(feature = NULL, quietly = FALSE) {
  # Define dependencies for each feature
  deps <- list(
    core = c("dplyr", "stringr", "tidyr", "readr"),
    visualization = c("ggplot2", "plotly", "leaflet"),
    analysis = c("metafor", "meta"),
    shiny = c("shiny", "DT", "htmltools", "shinydashboard"),
    reporting = c("rmarkdown", "knitr")
  )
  
  # If no feature specified, check all dependencies
  if (is.null(feature)) {
    all_deps <- unique(unlist(deps))
    feature_deps <- all_deps
  } else if (feature %in% names(deps)) {
    feature_deps <- deps[[feature]]
  } else {
    stop("Unknown feature: ", feature, ". Available features: ", 
         paste(names(deps), collapse = ", "))
  }
  
  # Check if packages are installed
  installed <- vapply(feature_deps, function(pkg) {
    is_installed <- requireNamespace(pkg, quietly = TRUE)
    if (!quietly && is_installed) {
      message("Package '", pkg, "' is installed.")
    } else if (!quietly && !is_installed) {
      message("Package '", pkg, "' is NOT installed.")
    }
    return(is_installed)
  }, logical(1))
  
  all_installed <- all(installed)
  
  if (!all_installed && !quietly) {
    missing_pkgs <- feature_deps[!installed]
    message("Missing packages: ", paste(missing_pkgs, collapse = ", "), 
            ". Use install.packages() to install them.")
  }
  
  return(all_installed)
}

#' Display help information for meteor package
#'
#' @param topic Specific topic to get help on (NULL for general help)
#' @return Invisibly returns NULL
#' @export
#' @examples
#' meteor_help()
#' meteor_help("visualization")
meteor_help <- function(topic = NULL) {
  # Define help topics
  help_topics <- list(
    general = c(
      "meteor: Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms",
      "",
      "Main functions:",
      "  - initialize_meteor_project(): Create a new project structure",
      "  - import_amr_data(): Import AMR data from various sources",
      "  - launch_meteor(): Launch the Shiny application",
      "",
      "For more information on specific topics, try:",
      "  meteor_help(\"data\")",
      "  meteor_help(\"analysis\")",
      "  meteor_help(\"visualization\")",
      "  meteor_help(\"shiny\")"
    ),
    
    data = c(
      "Data Management Functions:",
      "",
      "  - import_amr_data(): Import AMR data from CSV, Excel, or RData files",
      "  - import_local_data(): Import researcher's local data",
      "  - validate_data(): Check data integrity and structure",
      "  - standardize_amr_data(): Standardize data fields",
      "  - filter_amr_data(): Filter data by various criteria",
      "",
      "Built-in datasets:",
      "  - human_amr: Human AMR data from published studies"
    ),
    
    analysis = c(
      "Analysis Functions:",
      "",
      "  - calculate_pooled_rate(): Calculate pooled resistance rates",
      "  - analyze_heterogeneity(): Analyze between-study heterogeneity",
      "  - perform_subgroup_analysis(): Perform analysis by subgroups",
      "  - perform_sensitivity_analysis(): Assess sensitivity of results",
      "  - perform_meta_regression(): Conduct meta-regression analysis",
      "  - compare_with_meta(): Compare local data with meta-analysis results"
    ),
    
    visualization = c(
      "Visualization Functions:",
      "",
      "  - create_forest_plot(): Create forest plots for meta-analysis results",
      "  - create_geo_map(): Create geographic maps of resistance rates",
      "  - create_resistance_heatmap(): Create heatmaps of resistance patterns",
      "  - create_trend_plot(): Visualize resistance trends over time",
      "  - create_comparison_plot(): Compare resistance across domains or regions"
    ),
    
    shiny = c(
      "Shiny Application Functions:",
      "",
      "  - launch_meteor(): Launch the main Shiny application",
      "  - run_module(): Run a specific Shiny module",
      "",
      "Available modules:",
      "  - forest_plot_module: Interactive forest plots",
      "  - map_module: Geographic visualization",
      "  - trend_module: Resistance trend analysis",
      "  - comparison_module: Compare resistance across domains"
    )
  )
  
  # Display appropriate help
  if (is.null(topic)) {
    cat(paste(help_topics$general, collapse = "\n"))
  } else if (topic %in% names(help_topics)) {
    cat(paste(help_topics[[topic]], collapse = "\n"))
  } else {
    stop("Unknown help topic: ", topic, ". Available topics: general, ", 
         paste(setdiff(names(help_topics), "general"), collapse = ", "))
  }
  
  invisible(NULL)
}

#' Launch the meteor Shiny application
#'
#' @param data Optional data to pre-load into the application
#' @param ... Additional parameters passed to shiny::runApp
#' @return Invisibly returns the return value from shiny::runApp
#' @export
#' @examples
#' \dontrun{
#' launch_meteor()
#' }
launch_meteor <- function(data = NULL, ...) {
  # Check if shiny is installed
  if (!check_dependencies("shiny", quietly = TRUE)) {
    stop("The shiny package is required to launch the application.")
  }
  
  # Set data in options if provided
  if (!is.null(data)) {
    set_meteor_options(shiny_data = data)
  }
  
  # Get the path to the app
  app_path <- system.file("shiny-apps/meteor", package = "meteor")
  
  if (app_path == "") {
    stop("Could not find the Shiny application. Try reinstalling the package.")
  }
  
  # Launch the app
  return(invisible(shiny::runApp(app_path, ...)))
} 