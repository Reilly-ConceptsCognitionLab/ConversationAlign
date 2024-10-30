#' clean_dyads
#'
#' Cleans and formats language transcripts from the read stage. Removes non-alphabetic characters and stopwords. Language transcripts can be lemmatized by calling lemmatize = TRUE. Vectorizes each utterance and reports the total word count and mean word length by interlocutor in each dyad. Also reports the number of words in each turn.
#' @name clean_dyads
#' @param dataframe produced from the read_dyads() function
#' @return dataframe with stopwords omitted, lemmatized words one per row
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr consecutive_id
#' @importFrom dplyr ungroup
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom textclean replace_contraction
#' @importFrom tm removeWords
#' @importFrom stringr str_squish
#' @importFrom tm stripWhitespace
#' @importFrom textstem lemmatize_strings
#' @importFrom stringr str_split_1
#' @importFrom stringr str_split
#' @importFrom stringi stri_remove_empty
#' @importFrom stringi stri_count_words
#' @importFrom tidyr separate_rows
#' @export clean_dyads

clean_dyads <- function(read_ts_df, lemmatize=TRUE) {
  # ADD LAPPLY TO RECOGNIZE DYADS WITH MORE THAT TWO INTERLOCUTORS AND THROWS A WARNING WITH PROBLEM DYADS

  #set Event_ID  and speaker names as factors
  read_ts_df$Participant_ID <- as.factor(read_ts_df$Participant_ID) #convert variables to factor
  read_ts_df$Event_ID <- as.factor(read_ts_df$Event_ID)

  if (any(grepl("Time", colnames(read_ts_df), ignore.case = TRUE)) == TRUE){
    #convert Time from hh:mm:ss or mm:ss to milliseconds
    read_ts_df$Time <- sapply(read_ts_df$Time, function(x){
      if (any(grepl(":", x)) == TRUE) {  #checks for colons, indicative of mm:ss
        x <- as.numeric(unlist(str_split(x, ":"))) #breaks string into vector by colon placement
        if (length(x) == 2) { #shows just mm, ss
          sum((x[1]*60000), (x[2]*1000))
        }
        else if ( length(xvec) == 3) { #shows hh, mm, ss
          sum((x[1]*3600000), (x[2]*60000), (x[3]*1000))
        }}
      else {
        x
      }
    })
  }

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
      x <- tm::removeWords(x, omissions_dyads23$word)
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
      x <- tm::removeWords(x, omissions_dyads23$word)
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
                  NWords_ByPersonTurn_CLEAN = stringi::stri_count_words(paste(CleanText, collapse = " "))) %>%
    dplyr::select(-RawText) %>% # remove the raw text column
    dplyr::ungroup()

  dfclean_sep <- tidyr::separate_rows(dfclean, CleanText) # create row for each word in clean text

  return(dfclean_sep)
}
