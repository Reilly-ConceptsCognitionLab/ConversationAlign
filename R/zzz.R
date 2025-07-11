#' @include utils.R
NULL

#' Package Loading and Data Initialization
#'
#' @description Handles package initialization including loading required datasets
#' from GitHub or local fallback files.
#' @keywords internal
#' @importFrom utils download.file
#' @importFrom tools R_user_dir
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Create package environment
  pkg_env <- asNamespace(pkgname)

  # Critical datasets
  critical_datasets <- c("MIT_stops", "lookup_Jul25", "SMART_stops",
                         "CA_orig_stops", "Temple_stops25")

  # Try loading from GitHub first
  loaded_from <- tryCatch({
    repo_url <- "https://raw.githubusercontent.com/Reilly-ConceptsCognitionLab/ConversationAlign_Data/main/data/"
    temp_dir <- tempdir()

    for(ds in critical_datasets) {
      temp_file <- file.path(temp_dir, paste0(ds, ".rda"))
      utils::download.file(
        url = paste0(repo_url, ds, ".rda"),
        destfile = temp_file,
        mode = "wb",
        quiet = TRUE
      )
      load(temp_file, envir = pkg_env)
      unlink(temp_file)
    }
    "github"
  }, error = function(e) {
    # Fallback to cache
    cache_dir <- tools::R_user_dir(pkgname, which = "cache")
    cached_files <- file.path(cache_dir, paste0(critical_datasets, ".rda"))

    available <- file.exists(cached_files)
    if(any(available)) {
      for(cf in cached_files[available]) {
        load(cf, envir = pkg_env)
      }
      "cache"
    } else {
      "none"
    }
  })

  # Set package option
  options(ConversationAlign.data_source = loaded_from)

  # Only show message if critical datasets are missing
  still_missing <- setdiff(critical_datasets, ls(envir = pkg_env))
  if(length(still_missing) > 0) {
    # Construct appropriate message based on source
    msg_type <- if(loaded_from == "none") "error" else "warning"
    msg <- switch(
      msg_type,
      "error" = paste(
        "Critical data missing:", paste(still_missing, collapse = ", "),
        "\nPlease use refresh_data() or contact maintainers"
      ),
      "warning" = paste(
        "Using cached data (missing:", paste(still_missing, collapse = ", "), ")",
        "\nSome features unavailable - try refresh_data()"
      )
    )

    # Use packageStartupMessage for non-error cases
    if(msg_type == "error") {
      warning(msg, call. = FALSE, immediate. = TRUE)
    } else {
      packageStartupMessage(msg)
    }
  }

  # Silent success case requires no message
}
