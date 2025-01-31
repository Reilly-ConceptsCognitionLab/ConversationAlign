#' clean_dyads
#'
#' Cleans and formats language transcripts from the read stage. Removes non-alphabetic characters and stopwords. Language transcripts can be lemmatized by calling lemmatize = TRUE. Vectorizes each utterance and reports the total word count and mean word length by interlocutor in each dyad. Also reports the number of words in each turn.
#' @name clean_dyads
#' @param read_ts_df produced from the read_dyads() function
#' @param lemmatize logical, should words be lemmatized (switched to base morphological form)
#' @param stop_words_df defaults to built in list of stopwords, otherwise supply a file path to a cvs file with a column of stopwords titles 'Word'
#' @return dataframe with cleaned text data, formatted with one word per row
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr consecutive_id
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom textclean replace_contraction
#' @importFrom tm removeWords
#' @importFrom stringr str_squish
#' @importFrom tm stripWhitespace
#' @importFrom textstem lemmatize_strings
#' @importFrom stringi stri_count_words
#' @importFrom tidyr separate_rows
#' @export

clean_dyads <- function(read_ts_df, lemmatize=TRUE, stop_words_df = "default") {
  default <- TRUE
  # if not default and not proper file path (aside from default), throw an error
  if (class(stop_words_df) != "character" | (!grepl(".csv", stop_words_df) & stop_words_df != "default")) {
    stop("The argument 'stop_words_df' takes a string type, and should be a filepath leading to a csv file.")
  }
  # otherwise, if not default and also a csv path
  else if (stop_words_df != "default") {
    # read the filepath to a csv if given, can also supply a data frame
    stop_words_df <- read.csv(stop_words_df)
    default <- FALSE
    # throw an error if there is no column called 'word'
    if (all(grepl("word", colnames(stop_words_df), ignore.case = T) == FALSE)) {
      stop("The given stopwords data frame must have a column called 'word' or 'Word")
    }
    colnames(stop_words_df) <- tolower(colnames(stop_words_df))
  }

  #set Event_ID  and speaker names as factors
  read_ts_df$Participant_ID <- as.factor(read_ts_df$Participant_ID) #convert variables to factor
  read_ts_df$Event_ID <- as.factor(read_ts_df$Event_ID)

  if(lemmatize==TRUE) {
    clean <- function(x) {
      x <- tolower(x) #to lower
      x <- gsub("\"", " ", x)
      x <- gsub("\n", " ", x)
      x <- gsub("`", "'", x)  # replaces tick marks with apostrophe for contractions
      x <- gsub("can't", "can not", x)
      x <- gsub("won't", "will not", x)
      x <- gsub("n't", " not", x) #replace contraction with full word not
      x <- textclean::replace_contraction(x) #replace contractions
      x <- gsub("-", " ", x) #replace all hyphens with spaces

      if (default == TRUE) {
        x <- tm::removeWords(x, ReillyLab_Stopwords_25$word)
      }
      else {
        x <- tm::removeWords(x, stop_words_df$word)
      }

      x <- gsub("\\d+(st|nd|rd|th)", " ", x) #omits 6th, 23rd, ordinal numbers
      x <- gsub("[^a-zA-Z]", " ", x) #omit non-alphabetic characters
      x <- gsub("\\b[a]\\b{1}", " ", x)
      x <- tm::stripWhitespace(x)
      x <- stringr::str_squish(x)
      x <- textstem::lemmatize_strings(x) #lemmatize
    }
  }

  if(lemmatize==FALSE) {
    clean <- function(x) {
      x <- tolower(x) #to lower
      x <- gsub("\"", " ", x)
      x <- gsub("\n", " ", x)
      x <- gsub("`", "'", x)  # replaces tick marks with apostrophe for contractions
      x <- gsub("can't", "can not", x)
      x <- gsub("won't", "will not", x)
      x <- gsub("n't", " not", x) #replace contraction with full word not
      x <- textclean::replace_contraction(x) #replace contractions
      x <- gsub("-", " ", x) #replace all hyphens with spaces

      if (default == TRUE) {
        x <- tm::removeWords(x, ReillyLab_Stopwords_25$word)
      }
      else {
        x <- tm::removeWords(x, stop_words_df$word)
      }

      x <- gsub("\\d+(st|nd|rd|th)", " ", x) #omits 6th, 23rd, ordinal numbers
      x <- gsub("[^a-zA-Z]", " ", x) #omit non-alphabetic characters
      x <- gsub("\\b[a]\\b{1}", " ", x)
      x <- tm::stripWhitespace(x)
      x <- stringr::str_squish(x)
    }
  }

  read_ts_df$RawText <- stringr::str_squish(read_ts_df$RawText) #remove unneeded white space from text

  dfclean <- read_ts_df %>%
    dplyr::mutate(CleanText = clean(RawText)) %>%  #run clean function on text, making a new column
    dplyr::group_by(Event_ID) %>% # group by event to compute turn
    dplyr::mutate(TurnCount = dplyr::consecutive_id(Participant_ID), .after = Participant_ID) %>% # create a turn count
    dplyr::group_by(TurnCount, .add = TRUE) %>% #group and take word count by turn for both raw and clean utterances
    dplyr::mutate(NWords_ByPersonTurn_RAW = stringi::stri_count_words(paste(RawText, collapse = " ")),
                  NWords_ByPersonTurn_CLEAN = stringi::stri_count_words(paste(CleanText,
                                                                              collapse = " "))) %>%
    dplyr::select(-RawText) %>% # remove the raw text column
    dplyr::ungroup()

  dfclean_sep <- tidyr::separate_rows(dfclean, CleanText) # create row for each word in clean text

  return(dfclean_sep)
}
