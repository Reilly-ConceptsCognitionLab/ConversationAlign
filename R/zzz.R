#' @include utils.R
NULL

#' Package Loading and Data Initialization
#'
#' @description Handles package initialization including loading required datasets
#' from GitHub or local fallback files.
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  pkg_env <- asNamespace(pkgname)

  # Define required datasets - update this list as needed
  required_datasets <- c(
    "lookup_Jul25",
    "MIT_stops",
    "SMART_stops",
    "CA_orig_stops",
    "Temple_stops25"
  )

  # Try loading from GitHub first
  tryCatch({
    load_github_data(
      repo = "Reilly-ConceptsCognitionLab/ConversationAlign_Data",
      branch = "main",
      data_folder = "data",
      envir = pkg_env
    )
    packageStartupMessage("Successfully loaded datasets from GitHub")
  }, error = function(e) {
    warning("GitHub data load failed: ", e$message, immediate. = TRUE)

    # Try loading from local fallback
    local_data <- system.file("extdata", "fallback_data.rda", package = pkgname)
    if (file.exists(local_data)) {
      load(local_data, envir = pkg_env)
      packageStartupMessage("Loaded datasets from local fallback")
    }
  })

  # Verify all required datasets are present
  missing_data <- setdiff(required_datasets, ls(envir = pkg_env))
  if (length(missing_data) > 0) {
    warning(
      "Missing critical datasets: ", paste(missing_data, collapse = ", "), "\n",
      "Functionality may be impaired. Please:\n",
      "1. Check your internet connection\n",
      "2. Update the package\n",
      "3. Contact package maintainers if problem persists",
      immediate. = TRUE
    )
  }

  # Initialize package options if needed
  options(
    ConversationAlign.verbose = TRUE,
    ConversationAlign.cache = TRUE
  )
}

#' Package Attach Message
#'
#' @description Displays startup messages when package is attached
#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  pkg_env <- asNamespace(pkgname)

  if (all(c("lookup_Jul25", "MIT_stops") %in% ls(envir = pkg_env))) {
    packageStartupMessage(
      cli::col_green("ConversationAlign loaded successfully with all datasets")
    )
  } else {
    packageStartupMessage(
      cli::col_yellow("ConversationAlign loaded with some datasets missing"),
      "\nSome functionality may be limited"
    )
  }

  # Display version information
  version <- utils::packageVersion(pkgname)
  packageStartupMessage(
    "Version: ", version,
    "\nFor help: ?ConversationAlign or visit GitHub repo"
  )
}

#' Package Unload Cleanup
#'
#' @description Handles cleanup when package is unloaded
#' @keywords internal
#' @noRd
.onUnload <- function(libpath) {
  options(
    ConversationAlign.verbose = NULL,
    ConversationAlign.cache = NULL
  )
}

# Register S3 methods if needed
.onLoad <- function(libname, pkgname) {
  # Add your S3 method registrations here if any
  # Example:
  # if (getRversion() >= "2.15.1") {
  #   utils::globalVariables(c(".", "group", "value"))
  # }
  invisible()
}
