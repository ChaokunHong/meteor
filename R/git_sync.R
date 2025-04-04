#' Sync with GitHub
#'
#' This function commits all changes and pushes them to GitHub
#'
#' @param commit_message Character string with commit message. If NULL, a timestamp will be used.
#'
#' @return Invisible boolean indicating success
#' @export
#'
#' @examples
#' \dontrun{
#' # Sync with a custom message
#' sync_to_github("Updated analysis functions")
#'
#' # Sync with auto-generated timestamp message
#' sync_to_github()
#' }
sync_to_github <- function(commit_message = NULL) {
  # Check if git is available
  if (Sys.which("git") == "") {
    stop("Git executable not found. Make sure Git is installed and in your PATH.")
  }
  
  # Generate commit message with timestamp if not provided
  if (is.null(commit_message)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    commit_message <- paste("Auto-sync changes at", timestamp)
  }
  
  # Get package directory
  pkg_dir <- getwd()
  
  # Add all changes
  system(paste0("cd ", pkg_dir, " && git add ."))
  
  # Check if there are changes to commit
  status <- system(paste0("cd ", pkg_dir, " && git diff-index --quiet HEAD --"), ignore.stdout = TRUE)
  
  if (status != 0) {
    # There are changes to commit
    system(paste0("cd ", pkg_dir, " && git commit -m \"", commit_message, "\""))
    system(paste0("cd ", pkg_dir, " && git push origin main"))
    message("Changes pushed to GitHub: ", commit_message)
    invisible(TRUE)
  } else {
    message("No changes to commit")
    invisible(FALSE)
  }
} 