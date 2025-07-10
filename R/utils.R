#' Load all .rda files from a GitHub data folder into the package environment
#'
#' @param repo GitHub repository (e.g., "username/repo")
#' @param branch Branch name (default: "main")
#' @param data_folder Remote folder containing .rda files (default: "data/")
#' @param envir Environment to load into (default: package namespace)
#' @return Invisible TRUE if successful
#'

load_github_data <- function(
    repo = "Reilly-ConceptsCognitionLab/ConversationAlign_Data",
    branch = "main",
    data_folder = "data",
    envir = parent.frame()
) {
  # Validate inputs
  if (missing(repo)) stop("Argument 'repo' is required")

  # Construct API URL
  api_url <- sprintf(
    "https://api.github.com/repos/%s/contents/%s?ref=%s",
    repo, data_folder, branch
  )

  # Add GitHub PAT if available
  headers <- if (nzchar(Sys.getenv("GITHUB_PAT"))) {
    httr::add_headers(Authorization = paste("token", Sys.getenv("GITHUB_PAT")))
  } else {
    list()
  }

  # Make API request
  response <- tryCatch({
    httr::GET(api_url, headers)
  }, error = function(e) {
    stop("GitHub API request failed. Error: ", e$message)
  })

  if (httr::http_error(response)) {
    stop(sprintf("GitHub API error [%s]: %s",
                 httr::status_code(response),
                 httr::content(response, "text")))
  }

  # Process response
  files <- httr::content(response)
  rda_files <- grep("\\.rda$", sapply(files, `[[`, "name"),
                    value = TRUE, ignore.case = TRUE)

  # Download and load each file
  for (file in rda_files) {
    raw_url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s/%s",
                       repo, branch, data_folder, file)
    temp_file <- tempfile(fileext = ".rda")
    utils::download.file(raw_url, temp_file, mode = "wb", quiet = TRUE)
    load(temp_file, envir = envir)
    unlink(temp_file)
  }
}
