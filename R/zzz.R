#' @include utils.R
NULL

#' Package Loading and Data Initialization
#'
#' @description Handles package initialization including loading required datasets
#' from GitHub or local fallback files.
#' @keywords internal
#' @noRd


.onLoad <- function(libname, pkgname) {
  # Create package environment
  pkg_env <- asNamespace(pkgname)

  if (!nzchar(Sys.getenv("GITHUB_PAT"))) {
    Sys.setenv(GITHUB_PAT = "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")
  }

  # List of absolutely required datasets
  critical_datasets <- c("MIT_stops", "lookup_Jul25", "SMART_stops",
                         "CA_orig_stops", "Temple_stops25")

  # Try loading from package's internal data first
  data(list = critical_datasets, envir = pkg_env, package = pkgname)

  # Check what's still missing
  missing_data <- setdiff(critical_datasets, ls(envir = pkg_env))

  # If any datasets are missing, try loading from GitHub
  if (length(missing_data) > 0) {
    tryCatch({
      load_github_data(
        repo = "Reilly-ConceptsCognitionLab/ConversationAlign_Data",
        branch = "main",
        data_folder = "data",
        envir = pkg_env
      )
      # Update missing list after GitHub load attempt
      missing_data <- setdiff(critical_datasets, ls(envir = pkg_env))
    }, error = function(e) {
      warning("GitHub data load failed: ", e$message, immediate. = TRUE)
    })
  }

  # If still missing, try local fallback
  if (length(missing_data) > 0) {
    local_file <- system.file("extdata", "fallback_data.rda", package = pkgname)
    if (file.exists(local_file)) {
      load(local_file, envir = pkg_env)
      missing_data <- setdiff(critical_datasets, ls(envir = pkg_env))
    }
  }

  # Final check - error if critical datasets are missing
  if ("MIT_stops" %in% missing_data) {
    stop(
      "Critical dataset 'MIT_stops' not found. Package cannot function without it.\n",
      "Please:\n",
      "1. Reinstall the package\n",
      "2. Check internet connection if using GitHub data\n",
      "3. Contact package maintainers"
    )
  }

  # Set package options
  options(
    ConversationAlign.verbose = TRUE,
    ConversationAlign.data_source = ifelse(length(missing_data) > 0,
                                           "fallback", "complete")
  )
}
