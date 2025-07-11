#' @include utils.R
NULL

#' Package Loading and Data Initialization
#'
#' @description Handles package initialization including loading required datasets
#' from GitHub or local fallback files.
#' @keywords internal
#' @importFrom utils download.file
#' @noRd


.onLoad <- function(libname, pkgname) {
  # Create package environment
  pkg_env <- asNamespace(pkgname)

  # Critical datasets (must match the .rda filenames exactly)
  critical_datasets <- c("MIT_stops", "lookup_Jul25", "SMART_stops",
                         "CA_orig_stops", "Temple_stops25")

  # 1. Try loading from GitHub repo (primary source)
  tryCatch({
    # Use raw GitHub content URL
    repo_url <- "https://raw.githubusercontent.com/Reilly-ConceptsCognitionLab/ConversationAlign_Data/main/data/"

    # Temporary directory for downloads
    temp_dir <- tempdir()

    # Download and load each dataset
    for(ds in critical_datasets) {
      temp_file <- file.path(temp_dir, paste0(ds, ".rda"))
      utils::download.file(
        url = paste0(repo_url, ds, ".rda"),
        destfile = temp_file,
        mode = "wb",
        quiet = TRUE
      )
      load(temp_file, envir = pkg_env)
      unlink(temp_file) # Clean up
    }

    # Verify all datasets loaded
    missing_data <- setdiff(critical_datasets, ls(envir = pkg_env))

    if(length(missing_data) > 0) {
      warning("These datasets failed to load: ", paste(missing_data, collapse = ", "),
              immediate. = TRUE)
    }

  }, error = function(e) {
    warning("Primary data load failed: ", e$message, immediate. = TRUE)

    # 2. Fallback to cached version if available
    cache_dir <- tools::R_user_dir(pkgname, which = "cache")
    cached_files <- file.path(cache_dir, paste0(critical_datasets, ".rda"))

    if(any(file.exists(cached_files))) {
      for(cf in cached_files[file.exists(cached_files)]) {
        load(cf, envir = pkg_env)
      }
      packageStartupMessage("Loaded cached version of datasets")

      # Check if any datasets are still missing
      still_missing <- setdiff(critical_datasets, ls(envir = pkg_env))
      if(length(still_missing) > 0) {
        packageStartupMessage(
          "Warning: Critical datasets missing (", paste(still_missing, collapse = ", "), ").\n",
          "Basic functionality will be impaired.\n",
          "Please check your internet connection and try:\n",
          "1. library(", pkgname, ")\n",
          "2. Contact maintainers if problem persists"
        )
      }
    } else {
      packageStartupMessage(
        "Warning: No data available - critical datasets missing.\n",
        "Basic functionality will be impaired.\n",
        "Please check your internet connection and try:\n",
        "1. library(", pkgname, ")\n",
        "2. Contact maintainers if problem persists"
      )
    }
  })

  # Set package options
  options(
    ConversationAlign.data_source = ifelse(
      all(critical_datasets %in% ls(envir = pkg_env)),
      "github",
      ifelse(any(critical_datasets %in% ls(envir = pkg_env)),
             "partial_cache",
             "none")
    )
  )
}
