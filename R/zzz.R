.onLoad <- function(libname, pkgname) {
  # Load all .rda files from GitHub into the package namespace
  tryCatch({
    load_github_data(
      repo = "Reilly-ConceptsCognitionLab/ConversationAlign_Data",  # Replace with your repo
      branch = "main",                                       # Replace if needed
      data_folder = "data/",                                # Remote folder containing .rda files
      envir = asNamespace(ConversationAlign)               # Load into package namespace
    )
    packageStartupMessage("Successfully loaded external data from GitHub.")
  }, error = function(e) {
    packageStartupMessage("Failed to load external data: ", e$message)
  })
}
