#' METEOR: Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms
#'
#' @description
#' METEOR is a comprehensive R package for analyzing antimicrobial resistance (AMR) data.
#' It provides tools for meta-analysis, visualization, and comparison of AMR data across
#' human, animal, and environmental domains.
#'
#' @details
#' The package includes functions for:
#' \itemize{
#'   \item Data Import: Import AMR data from various file formats
#'   \item Data Validation: Validate and standardize AMR data
#'   \item Meta-Analysis: Perform meta-analyses with various methods
#'   \item Visualization: Create forests plots, maps, and heatmaps
#'   \item Reporting: Generate comprehensive reports
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{import_amr_data}}}{Import AMR data from files}
#'   \item{\code{\link{validate_data}}}{Validate AMR data structure}
#'   \item{\code{\link{standardize_amr_data}}}{Standardize AMR data format}
#'   \item{\code{\link{calculate_pooled_rate}}}{Perform meta-analysis}
#'   \item{\code{\link{create_forest_plot}}}{Create forest plots}
#'   \item{\code{\link{launch_meteor}}}{Launch Shiny application}
#' }
#'
#' @section Package Options:
#' Package options can be set with \code{\link{set_meteor_options}}:
#' \describe{
#'   \item{\code{default_meta_method}}{Default meta-analysis method ("REML")}
#'   \item{\code{confidence_level}}{Confidence level for intervals (0.95)}
#'   \item{\code{color_palette}}{Default color palette for plots ("viridis")}
#'   \item{\code{na_handling}}{How to handle NA values ("omit")}
#'   \item{\code{significant_digits}}{Number of significant digits (3)}
#'   \item{\code{show_warnings}}{Whether to show warnings (TRUE)}
#' }
#'
#' @section Data:
#' \describe{
#'   \item{\code{human_data}}{Human AMR data}
#'   \item{\code{standardized_human_data}}{Standardized human AMR data}
#' }
#'
#' @docType package
#' @name meteor-package
NULL

#' Human antimicrobial resistance data
#'
#' A dataset containing antimicrobial resistance data from human studies.
#'
#' @format A data frame with multiple columns:
#' \describe{
#'   \item{study ID}{Unique study identifier}
#'   \item{firstauthor_last}{Last name of the first author}
#'   \item{year_published}{Publication year}
#'   \item{location}{Study location/country}
#'   \item{population}{Study population (e.g., Adults, Children)}
#'   \item{Environment}{Study setting (e.g., Hospital, Community)}
#'   \item{n_pop}{Population size}
#'   \item{r_*_*}{Resistance rates for specific antibiotic-pathogen combinations}
#'   \item{n_*_*}{Number of resistant isolates}
#'   \item{d_*_*}{Number of tested isolates}
#' }
#'
#' @source Published literature
"human_data"

#' Standardized human antimicrobial resistance data
#'
#' A standardized version of the human AMR data, ready for meta-analysis.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{study_id}{Unique study identifier}
#'   \item{firstauthor}{First author's last name}
#'   \item{year}{Publication year}
#'   \item{country}{Study country}
#'   \item{region}{Geographic region}
#'   \item{pathogen}{Pathogen name}
#'   \item{antibiotic}{Antibiotic name}
#'   \item{resistance_rate}{Antimicrobial resistance rate}
#'   \item{ci_lower}{Lower bound of confidence interval}
#'   \item{ci_upper}{Upper bound of confidence interval}
#'   \item{n_resistant}{Number of resistant isolates}
#'   \item{n_tested}{Number of tested isolates}
#'   \item{domain}{Data domain (human)}
#'   \item{population_type}{Type of population studied}
#'   \item{setting}{Study setting}
#' }
#'
#' @source Processed from human_data
"standardized_human_data" 