.onLoad <- function(libname, pkgname) {
  # Load all required datasets
  pkg_env <- asNamespace(pkgname)

  # List of required datasets
  required_datasets <- c("lookup_Jul25", "MIT_stops", "SMART_stops", "replacements_25", "CA_orig_stops", "Temple_stops25") # Add all needed datasets

  # Try loading from GitHub first
  tryCatch({
    load_github_data(envir = pkg_env)
  }, error = function(e) {
    warning("GitHub data load failed: ", e$message)
  })

  # Verify all datasets are loaded
  missing_data <- setdiff(required_datasets, ls(envir = pkg_env))
  if (length(missing_data) > 0) {
    warning("Missing datasets: ", paste(missing_data, collapse = ", "),
            "\nSome functions may not work properly.")
  }
}
