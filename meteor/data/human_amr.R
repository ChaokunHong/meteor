#' Human Antimicrobial Resistance Data
#'
#' A dataset containing antimicrobial resistance rates from human studies
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{record_id}{Unique identifier for each record}
#'   \item{study_id}{Study identifier}
#'   \item{first_author}{Last name of the first author}
#'   \item{year_published}{Year of publication}
#'   \item{location}{Country or region of the study}
#'   \item{study_type}{Type of study (e.g., Cross-sectional, Cohort Study)}
#'   \item{environment}{Study environment (e.g., Rural Area, Urban Area, Hospital)}
#'   \item{population}{Population studied (e.g., Adults, Children)}
#'   \item{sample_size}{Total sample size of the study}
#'   \item{age_range}{Age range of participants}
#'   \item{pathogen}{Pathogen studied (e.g., Ecoil, KP, SA, SP)}
#'   \item{antibiotic}{Antibiotic tested (e.g., AMP, CIP, TET)}
#'   \item{resistance_rate}{Proportion of resistant isolates}
#'   \item{sample_size_ab}{Sample size for the specific antibiotic-pathogen pair}
#'   \item{resistant_count}{Number of resistant isolates}
#' }
#' @source Various published studies on antimicrobial resistance
"human_amr" 