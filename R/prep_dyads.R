#' prep_dyads
#'
#' Cleans, vectorizes and appends lexical norms to all content words in a language corpus. User guides options for stopword removal and lemmatization. User selects up to three psycholinguistic dimensions to yoke norms on each content word in the transcript.
#' @name prep_dyads
#' @param dat_read data frame produced from the read_dyads() function
#' @param omit_stops remove stopwords, default TRUE
#' @param lemmatize logical, should words be lemmatized (switched to base morphological form), default is TRUE
#' @param which_stoplist user specifies stopword removal method with options including "none", "SMART", "MIT_stops", "CA_OriginalStops", or "Temple_Stopwords25". "Temple_Stopwords25 is the default list
#' @return dataframe with cleaned text data, formatted with one word per row
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
#' @importFrom tidyselect any_of
#' @importFrom tidyselect contains
#' @importFrom utils install.packages
#' @importFrom utils select.list
#' @export

prep_dyads <- function(dat_read, lemmatize = TRUE, omit_stops = TRUE, which_stoplist = "Temple_stops25") {
  # Load required packages
  my_packages <- c("dplyr", "magrittr", "purrr", "stringi", "stringr", "textstem", "tidyr", "tidyselect", "utils")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }

  # Verification steps
  if (!"RawText" %in% names(dat_read)) {
    stop("Column 'RawText' not found.")
  }
  if (!"Participant_ID" %in% names(dat_read)) {
    stop("Column 'Participant_ID' not found.")
  }

  # Define stopword lists
  stopwords_lists <- list(MIT_stops = MIT_stops,
                          Temple_stops25 = Temple_stops25,
                          SMART_stops = SMART_stops,
                          CA_orig_stops = CA_orig_stops)

  # Only prompt for stoplist if omit_stops is TRUE and which_stoplist is NULL
  if (omit_stops && is.null(which_stoplist)) {
    cat("Available stopword lists:\n")
    cat("1. Temple_stops25\n2. MIT_stops\n3. SMART_stops\n4. CA_orig_stops\n")
    choice <- readline(prompt = "Enter number of stoplist to use (1-4): ")
    which_stoplist <- c("Temple_stops25", "MIT_stops", "SMART_stops", "CA_orig_stops")[as.integer(choice)]
  }

  # Work on new dataframe
  dat_prep <- dat_read

  # create Turn_Count variable, Create a switchmarker for when Participant_ID changes
  dat_prep <- dat_prep %>%
    group_by(Event_ID) %>%
    mutate(switch_mark = Participant_ID != dplyr::lag(Participant_ID, default = dplyr::first(Participant_ID)),
           # Cumulative sum of changes to create TurnCount, Start at 1, remove switch marker
           Turn_Count = cumsum(switch_mark) + 1) %>%
    select(-switch_mark) %>%
    ungroup()

  # Text Processing Pipeline
  dat_prep <- dat_prep %>% mutate(Participant_ID = as.factor(Participant_ID),
                                  Event_ID = as.factor(Event_ID)) %>% mutate(Text_Prep = tolower(RawText))

  # Standardize apostrophes
  dat_prep <- dat_prep %>%
    dplyr::mutate(
      Text_Prep = stringi::stri_replace_all_regex(Text_Prep, "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]", "'"))

  # Remove all non-alphabetic characters except apostrophes
  dat_prep <- dat_prep %>%
    dplyr::mutate(Text_Prep = stringi::stri_replace_all_regex(Text_Prep, "[^a-zA-Z']", " "))

  # Replace multiple whitespaces with one, then trim any leading or following whitespaces from letter chars
  dat_prep$Text_Prep <- stringr::str_squish(gsub("\\s+", " ", dat_prep$Text_Prep))

  # Split rows by any sequence of whitespace
  dat_prep <- dat_prep %>% tidyr::separate_rows(Text_Prep, sep = "[[:space:]]+")

  # Remove any extraneous chars or whitespace
  dat_prep$Text_Prep <- stringi::stri_replace_all_regex(
    dat_prep$Text_Prep, pattern = "[^a-z']", replacement = "", vectorize_all = FALSE)

  # Convert ASCII and remove any remaining non-visible characters
  dat_prep$Text_Prep <- iconv(dat_prep$Text_Prep, to = "ASCII//TRANSLIT", sub = "")
  dat_prep$Text_Prep <- stringi::stri_replace_all_regex(
    dat_prep$Text_Prep,
    pattern = "[^[:alnum:]']",  # Remove any non-alphanumeric except apostrophes
    replacement = "",
    vectorize_all = FALSE)

  # apply internal function of substitutions for lots of contractions
  dat_prep <- replacements_25(dat=dat_prep, wordcol="Text_Prep")

  # Split rows by any sequence of whitespace after expanding contractions
  dat_prep <- dat_prep %>%
    tidyr::separate_rows(Text_Prep, sep = "[[:space:]]+")

  # kill RawText
  dat_prep <- dat_prep %>%
    select(-RawText)

  # Final processing
  df_clean <- dat_prep %>%
    mutate(Text_Clean = Text_Prep) %>%
    # Lemmatization
    mutate(Text_Clean = if (lemmatize) textstem::lemmatize_strings(Text_Clean) else Text_Clean,
           Text_Clean = ifelse(stringi::stri_isempty(Text_Clean), NA, Text_Clean))

  # Modified stopword removal - replaces stopwords with NA instead of filtering
  if (omit_stops) {
    if (!which_stoplist %in% names(stopwords_lists)) {
      stop("Invalid stoplist specified. Choose from: ", paste(names(stopwords_lists), collapse = ", "))
    }

    stopwords <- stopwords_lists[[which_stoplist]]$word
    df_clean <- df_clean %>%
      mutate(Text_Clean = ifelse(Text_Clean %in% stopwords, NA, Text_Clean))
  }

  # PSYCHOLINGUISTIC VALUES TO DF_CLEAN$Text_Clean
  # extract all possible variable names in lookup database (excluding 'word')
  possible_vars <- setdiff(colnames(lookup_Jul25), "word")

  # Prompt user to select variables with validation
  repeat {
    # Display available variables with numbers
    cat("Available variables:\n")
    for (i in seq_along(possible_vars)) {
      cat(sprintf("%d. %s\n", i, possible_vars[i]))
    }

    # Get user input
    cat("\nSelect up to 3 variables you want to analyze alignment on.\n")
    cat("Enter the numbers separated by spaces (e.g., 1 3 5): ")
    input <- readline()

    # Process input - convert to numeric without requiring quotes
    selected_indices <- suppressWarnings(as.numeric(unlist(strsplit(trimws(input), "\\s+"))))

    # Validate selection
    if (length(selected_indices) == 0 || any(is.na(selected_indices))) {
      message("Invalid input. Please enter numbers only, separated by spaces.")
      next
    }

    if (any(selected_indices < 1 | selected_indices > length(possible_vars))) {
      message("Invalid selection. Please enter numbers between 1 and ", length(possible_vars))
      next
    }

    if (length(selected_indices) > 3) {
      message("You selected more than 3 variables. Please select 3 or fewer.")
      next
    }

    myvars <- possible_vars[selected_indices]
    break
  }

  var_selected <- lookup_Jul25 %>%
    dplyr::select(c("word", tidyselect::all_of(myvars)))

  # Create variable containing the column names of each variable to be aligned
  var_aligners <- colnames(var_selected)

  # Join measures of each variable to each word in each transcript
  df_aligned <- df_clean %>%
    dplyr::left_join(var_selected,
                     by = c("Text_Clean" = "word"),
                     multiple = "all",
                     relationship = "many-to-one") %>%
    # Remove the 'word' column if it exists (from the join)
    select(-any_of("word"))

  # Add exchange count - divides each turn number by 2 then rounds up
  df_aligned$Exchange_Count <- ceiling(df_aligned$Turn_Count / 2)

  # Reorder columns
  df_aligned <- df_aligned %>%
    dplyr::select(tidyselect::all_of(c('Event_ID', 'Participant_ID', 'Exchange_Count',
                                       'Turn_Count', 'Text_Prep', 'Text_Clean')),

                  # Then select all columns from var_aligners (exact matches only)
                  tidyselect::all_of(var_aligners),

                  # grab all else
                  tidyselect::everything()
    )

  return(df_aligned)
}
