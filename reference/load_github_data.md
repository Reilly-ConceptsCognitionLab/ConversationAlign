# Load all .rda files from a GitHub data folder into the package environment

Load all .rda files from a GitHub data folder into the package
environment

## Usage

``` r
load_github_data(
  repo = "Reilly-ConceptsCognitionLab/ConversationAlign_Data",
  branch = "main",
  data_folder = "data",
  envir = parent.frame()
)
```

## Arguments

- repo:

  GitHub repository (e.g., "username/repo")

- branch:

  Branch name (default: "main")

- data_folder:

  Remote folder containing .rda files (default: "data/")

- envir:

  Environment to load into (default: package namespace)

## Value

nothing, loads data (as rda files) from github repository needed for
other package functions
