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
#' @importFrom tidyr separate_rows
#' @export clean_dyads

clean_dyads <- function(read_ts_df, lemmatize=TRUE) {
  #set event_id  and speaker names as factors
  read_ts_df$Participant_ID <- as.factor(read_ts_df$Participant_ID) #convert variables to factor
  read_ts_df$event_id <- as.factor(read_ts_df$event_id)

  if (any(grepl("time", colnames(read_ts_df), ignore.case = TRUE)) == TRUE){
    #convert time from hh:mm:ss or mm:ss to milliseconds
    read_ts_df$time <- sapply(read_ts_df$time, function(x){
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

  read_ts_df$rawtext <- stringr::str_squish(read_ts_df$rawtext) #remove unneeded white space from text


  #code that adds word count and mean word length by dyad by speaker
  read_data_frame <- read_ts_df %>%
    dplyr::group_by(event_id, Participant_ID) %>% #group and take word count and length
    dplyr::mutate(wordcount_raw = length(stringr::str_squish(stringr::str_split_1(paste(rawtext, collapse = " "), " "))),
                  mean_word_length_raw = mean(nchar(stringr::str_squish(stringr::str_split_1(paste(rawtext, collapse = " "), " "))))) %>%
    dplyr::ungroup()


  dfclean <- read_data_frame %>%
    dplyr::mutate(cleantext = clean(rawtext)) %>%  #run clean function on text, making a new column
    dplyr::select(!rawtext) %>%
    dplyr::group_by(event_id, Participant_ID) %>% #group and take clean word count and length
    dplyr::mutate(wordcount_clean = length(stringr::str_squish(stringr::str_split_1(paste(cleantext, collapse = " "), " "))),
                  mean_word_length_clean = mean(nchar(stringr::str_squish(stringr::str_split_1(paste(cleantext, collapse = " "), " ")))),
                  word_removed_clean = wordcount_raw - wordcount_clean) %>%
    dplyr::ungroup()


  dfclean_sep <- tidyr::separate_rows(dfclean, cleantext) # create row for each word in clean text

  dfclean_filtered <- dfclean_sep %>%     #remove rows where text is an empty string
    dplyr::filter(cleantext != "") %>%
    dplyr::group_by(event_id) %>% #add a turn count per dyad, which counts by speaker change
    dplyr::mutate(turncount = dplyr::consecutive_id(Participant_ID), .before = 1) %>%
    dplyr::group_by(turncount, .add = TRUE) %>%
    dplyr::mutate(wordcount_clean_turn = length(cleantext)) %>% # take the word count of cleaned utterances at each turn
    dplyr::ungroup()

  return(dfclean_filtered)
}
