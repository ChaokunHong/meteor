#' Launch the METEOR Shiny Application
#'
#' This function launches the METEOR Shiny application for interactive exploration
#' of antimicrobial resistance data.
#'
#' @param ... Additional arguments passed to shiny::runApp
#'
#' @return The Shiny application object
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the application
#' launch_meteor()
#' 
#' # Launch with specific options
#' launch_meteor(port = 4000, host = "0.0.0.0")
#' }
launch_meteor <- function(...) {
  app_dir <- system.file("shiny-apps", "meteor-app", package = "meteor")
  
  if (app_dir == "") {
    stop("Could not find the Shiny application directory. Try reinstalling the 'meteor' package.")
  }
  
  shiny::runApp(app_dir, ...)
} 