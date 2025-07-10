#' Load all .rda files from a GitHub data folder into the package environment
#'
#' @param repo GitHub repository (e.g., "username/repo")
#' @param branch Branch name (default: "main")
#' @param data_folder Remote folder containing .rda files (default: "data/")
#' @param envir Environment to load into (default: package namespace)
#' @return Invisible TRUE if successful
#'
load_github_data <- function(repo, branch = "main", data_folder = "data/", envir = asNamespace(pkgname)) {
  # Construct GitHub API URL to list files in the data folder
  api_url <- sprintf("https://api.github.com/repos/%s/contents/%s?ref=%s", repo, data_folder, branch)

  # Fetch file list (requires internet)
  response <- tryCatch(
    jsonlite::fromJSON(api_url),
    error = function(e) stop("Failed to fetch GitHub files. Check repo, branch, or internet connection.")
  )

  # Filter for .rda files
  rda_files <- response$name[grepl("\\.rda$", response$name, ignore.case = TRUE)]
  if (length(rda_files) == 0) stop("No .rda files found in the specified GitHub folder.")

  # Download and load each file
  for (file in rda_files) {
    raw_url <- sprintf("https://github.com/%s/raw/%s/%s%s", repo, branch, data_folder, file)
    temp_file <- tempfile(fileext = ".rda")
    utils::download.file(raw_url, temp_file, mode = "wb", quiet = TRUE)
    load(temp_file, envir = envir)
    unlink(temp_file)  # Clean up
  }

  invisible(TRUE)
}
