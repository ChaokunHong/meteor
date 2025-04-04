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
#' @author Chaokun Hong \email{chaokun.hong@gmail.com}
#' @keywords meta-analysis antimicrobial resistance
#' @seealso \url{https://github.com/ChaokunHong/meteor}
#'
#' @docType package
#' @name meteor-package
NULL 