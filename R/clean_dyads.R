#' clean_dyads
#'
#' Cleans and formats language transcripts from the read stage. Removes non-alphabetic characters and stopwords. Language transcripts can be lemmatized by calling lemmatize = TRUE. Vectorizes each utterance and reports the total word count and mean word length by interlocutor in each dyad. Also reports the number of words in each turn.
#' @name clean_dyads
#' @param read_ts_df data frame produced from the read_dyads() function
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
#' @importFrom utils install.packages
#' @export

clean_dyads <- function(read_ts_df, lemmatize = TRUE, omit_stops = TRUE, which_stoplist = "Temple_stops25") {
  # Load required packages
  my_packages <- c("dplyr", "magrittr", "stringi", "stringr", "tm", "textstem", "tidyr", "purrr")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }

  # Make sure all lookup databases are loaded and available to the fn:
  MIT_stops <- getFromNamespace("MIT_stops", "ConversationAlign")
  Temple_stops25 <- getFromNamespace("Temple_stops25", "ConversationAlign")
  SMART_stops <- getFromNamespace("SMART_stops", "ConversationAlign")
  CA_orig_stops <- getFromNamespace("CA_orig_stops", "ConversationAlign")

  # Verification steps
  if (!"RawText" %in% names(read_ts_df)) {
    stop("Column 'RawText' not found.")
  }
  if (!"Participant_ID" %in% names(read_ts_df)) {
    stop("Column 'Participant_ID' not found.")
  }

  # Define stopword lists
  stopwords_lists <- list(MIT_stops = MIT_stops, Temple_stops25 = Temple_stops25, SMART_stops = SMART_stops,
                          CA_orig_stops = CA_orig_stops)

  # Only prompt for stoplist if omit_stops is TRUE and which_stoplist is NULL
  if (omit_stops && is.null(which_stoplist)) {
    cat("Available stopword lists:\n")
    cat("1. Temple_stops25\n2. MIT_stops\n3. SMART_stops\n4. CA_orig_stops\n")
    choice <- readline(prompt = "Enter number of stoplist to use (1-4): ")
    which_stoplist <- c("Temple_stops25", "MIT_stops", "SMART_stops", "CA_orig_stops")[as.integer(choice)]
  }

  # Work on new dataframe
  prep_df <- read_ts_df

  # create TurnCount variable, Create a switchmarker for when Participant_ID changes
  prep_df <- prep_df %>% group_by(Event_ID) %>%
    mutate(switch_mark = Participant_ID != dplyr::lag(Participant_ID, default = dplyr::first(Participant_ID)),
           # Cumulative sum of changes to create TurnCount, Start at 1, remove switch marker
           TurnCount = cumsum(switch_mark) + 1) %>% select(-switch_mark) %>% ungroup()

  # Text Processing Pipeline
  prep_df <- prep_df %>% mutate(Participant_ID = as.factor(Participant_ID), Event_ID = as.factor(Event_ID))
  prep_df <- prep_df %>% mutate(Text_Prep = tolower(RawText))

  # Standardize apostrophes
  prep_df <- prep_df %>% dplyr::mutate(
    Text_Prep = stringi::stri_replace_all_regex(Text_Prep, "[\u2018\u2019\u02BC\u201B\uFF07\u0092\u0091\u0060\u00B4\u2032\u2035]", "'"))

  # Remove all non-alphabetic characters except apostrophes
  prep_df <- prep_df %>% dplyr::mutate(Text_Prep =
                                         stringi::stri_replace_all_regex(Text_Prep, "[^a-zA-Z']", " "))

  # Replace multiple whitespaces with one, then trim any leading or following whitespaces from letter chars
  prep_df$Text_Prep <- stringr::str_squish(gsub("\\s+", " ", prep_df$Text_Prep))

  # Split rows by any sequence of whitespace
  prep_df <- prep_df %>% tidyr::separate_rows(Text_Prep, sep = "[[:space:]]+")

  # Remove any extraneous chars or whitespace
  prep_df$Text_Prep <- stringi::stri_replace_all_regex(
    prep_df$Text_Prep, pattern = "[^a-z']", replacement = "", vectorize_all = FALSE)

  # Convert ASCII and remove any remaining non-visible characters
  prep_df$Text_Prep <- iconv(prep_df$Text_Prep, to = "ASCII//TRANSLIT", sub = "")
  prep_df$Text_Prep <- stringi::stri_replace_all_regex(prep_df$Text_Prep,
                                                       pattern = "[^[:alnum:]']",  # Remove any non-alphanumeric except apostrophes
                                                       replacement = "", vectorize_all = FALSE)

  # apply internal function of substitutions for lots of contractions, string replacement occurs in place
  # overwriting original strings with new strings
  prep_df <- replacements_25(prep_df, "Text_Prep")

  # Split rows by any sequence of whitespace after expanding contractions
  prep_df <- prep_df %>% tidyr::separate_rows(Text_Prep, sep = "[[:space:]]+")

  # kill RawText
  prep_df<- prep_df %>% select(-RawText)

  # Final processing
  df_clean <- prep_df %>% mutate(Text_Clean = Text_Prep) %>%
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

  return(df_clean)
}



