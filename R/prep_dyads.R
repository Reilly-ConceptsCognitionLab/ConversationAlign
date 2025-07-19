#' prep_dyads
#'
#' Cleans, vectorizes and appends lexical norms to all content words in a language corpus.
#' User guides options for stopword removal and lemmatization. User selects up to three psycholinguistic dimensions to yoke norms
#' on each content word in the original conversation transcript.
#' @name prep_dyads
#' @param dat_read dataframe produced from read_dyads() function
#' @param omit_stops option to remove stopwords, default TRUE
#' @param lemmatize logical, should words be lemmatized (switched to base morphological form), default is TRUE
#' @param which_stoplist user-specified stopword removal method with options including "none", "SMART", "MIT_stops", "CA_OriginalStops", or "Temple_Stopwords25".
#' "Temple_Stopwords25 is the default list
#' @param verbose display detailed output such as error messages and progress (default is TRUE)
#' @returns
#' dataframe with text cleaned and vectorized to a one word per-row format.
#' Lexical norms and metadata are appended to each content word. Cleaned text appears under a new column
#' called 'Text_Clean'. Any selected dimensions (e.g., word length) and metadata are also appended to each word along
#' with speaker identity, turn, and Event_ID (conversation identifier).
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom stats lag
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringi stri_replace_all_regex
#' @importFrom stringr str_trim
#' @importFrom stringr str_squish
#' @importFrom stringr str_replace_all
#' @importFrom textstem lemmatize_strings
#' @importFrom tidyr separate_rows
#' @importFrom tidyselect all_of
#' @importFrom tidyselect any_of
#' @importFrom tidyselect contains
#' @importFrom utils select.list
#' @export

prep_dyads <- function(dat_read, lemmatize = TRUE, omit_stops = TRUE,
                       which_stoplist = "Temple_stops25", verbose = TRUE) {
  # Verification steps
  if (nrow(dat_read) == 0) {
    stop("Input dataframe is empty.")
  }
  if (!"Text_Raw" %in% names(dat_read)) {
    stop("Column 'Text_Raw' not found.")
  }
  if (!"Participant_ID" %in% names(dat_read)) {
    stop("Column 'Participant_ID' not found.")
  }

  # Define stopword lists
  stopwords_lists <- list(MIT_stops = MIT_stops,
                          Temple_stops25 = Temple_stops25,
                          SMART_stops = SMART_stops,
                          CA_orig_stops = CA_orig_stops)

  # Validate stoplist selection
  if (omit_stops && !which_stoplist %in% names(stopwords_lists)) {
    stop("Invalid stoplist specified. Choose from: ", paste(names(stopwords_lists), collapse = ", "))
  }

  # Only prompt for stoplist if omit_stops is TRUE and which_stoplist is NULL
  if (omit_stops && is.null(which_stoplist)) {
    if (verbose) {
      message("Available stopword lists:")
      message("1. Temple_stops25\n2. MIT_stops\n3. SMART_stops\n4. CA_orig_stops")
    }
    choice <- readline(prompt = "Enter number of stoplist to use (1-4): ")
    which_stoplist <- c("Temple_stops25", "MIT_stops", "SMART_stops", "CA_orig_stops")[as.integer(choice)]
  }

  # [Rest of the data processing steps remain unchanged until variable selection]

  # Variable selection
  possible_vars <- setdiff(colnames(lookup_Jul25), "word")

  repeat {
    if (verbose) {
      message("Available variables:")
      for (i in seq_along(possible_vars)) {
        message(sprintf("%d. %s", i, possible_vars[i]))
      }
      message("\nSelect up to 3 variables you want to analyze alignment on.")
      message("Enter the numbers separated by spaces (e.g., 1 3 5) then hit enter: ")
    }

    input <- readline()
    selected_indices <- suppressWarnings(as.numeric(unlist(strsplit(trimws(input), "\\s+"))))

    if (length(selected_indices) == 0 || any(is.na(selected_indices))) {
      warning("Invalid input. Please enter numbers only, separated by spaces.")
      next
    }
    if (any(selected_indices < 1 | selected_indices > length(possible_vars))) {
      warning("Invalid selection. Please enter numbers between 1 and ", length(possible_vars))
      next
    }
    if (length(selected_indices) > 3) {
      warning("You selected more than 3 variables. Please select 3 or fewer.")
      next
    }

    myvars <- possible_vars[selected_indices]
    break
  }

  # [Rest of the function remains unchanged]
  var_selected <- lookup_Jul25 %>% dplyr::select(word, tidyselect::all_of(myvars))

  # Join with psycholinguistic measures
  df_prep <- df_prep %>% left_join(var_selected, by = c("Text_Clean" = "word")) %>%
    mutate(Exchange_Count = ceiling(Turn_Count / 2))

  # Reorder columns
  df_prep <- df_prep %>% select(Event_ID, Participant_ID, Exchange_Count, Turn_Count,
                                Text_Prep, Text_Clean, all_of(myvars), everything())

  return(df_prep)
}
