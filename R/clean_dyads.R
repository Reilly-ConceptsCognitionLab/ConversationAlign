#' clean_dyads
#'
#' Cleans and Formats raw language transcripts, removing stopwords and formatting dataframe for alignment steps
#' @name clean_dyads
#' @param read_ts_df formatted dataframe ported from the read_dyads() step
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom textclean replace_contraction
#' @importFrom tm removeWords
#' @importFrom stringr str_squish
#' @importFrom tm stripWhitespace
#' @importFrom textstem lemmatize_words
#' @importFrom tidyverse rowwise
#' @importFrom tidyverse mutate
#' @importFrom tidyverse str_split_1
#' @importFrom tidyverse str_split
#' @importFrom stringi stri_remove_empty
#' @importFrom tidyr separate_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @export clean_dyads

clean_dyads <- function(read_ts_df) {
  #specify a group of speaker names that should be automatically removed from the transcript
  s_remove <- c("Unknown", "unknown", "Speaker", "speaker", "Other", "other", "E", "e", "Experimenter", "experimenter", "Assistant", "assistant")

  #removes rows from the transcript that have the speaker as specified in the remove
  if (any(read_ts_df$speaker_names_raw %in% s_remove) == TRUE){ #conditional in case no matches
    read_ts_df <- read_ts_df[-which(read_ts_df$speaker_names_raw %in% s_remove),]
  }

  #set event_id  and speaker names as factors
  read_ts_df$speaker_names_raw <- as.factor(read_ts_df$speaker_names_raw) #convert variables to factor
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
    x <- textstem::lemmatize_words(x)
  }

  read_ts_df$rawtext <- stringr::str_squish(read_ts_df$rawtext) #remove unneeded white space from text


  #code that adds word count and mean word length by dyad by speaker
  read_data_frame <- read_ts_df %>%
    dplyr::group_by(event_id, speaker_names_raw) %>% #group and take word count and length
    dplyr::mutate(an_wordcount_raw = length(stringr::str_squish(stringr::str_split_1(paste(rawtext, collapse = " "), " "))),
                  an_mean_word_length_raw = mean(nchar(stringr::str_squish(stringr::str_split_1(paste(rawtext, collapse = " "), " "))))) %>%
    dplyr::ungroup()


  dfclean <- read_data_frame %>%
    dplyr::mutate(cleantext = clean(rawtext)) %>%  #run clean function on text, making a new column
    dplyr::select(!rawtext) %>%
    dplyr::group_by(event_id, speaker_names_raw) %>% #group and take clean word count and length
    dplyr::mutate(an_wordcount_clean = length(stringr::str_squish(stringr::str_split_1(paste(cleantext, collapse = " "), " "))),
                  an_mean_word_length_clean = mean(nchar(stringr::str_squish(stringr::str_split_1(paste(cleantext, collapse = " "), " ")))),
                  an_word_removed_clean = an_wordcount_raw - an_wordcount_clean) %>%
    dplyr::ungroup()


  dfclean_sep <- tidyr::separate_rows(dfclean, cleantext) # create row for each word in clean text

  dfclean_filtered <- dfclean_sep %>%     #remove rows where text is an empty string
    dplyr::filter(cleantext != "") %>%
    #new stuff for testing the word count analytic fun
    dplyr::group_by(event_id) %>% #add a turn count per dyad, which counts by speaker change
    dplyr::mutate(turncount = dplyr::consecutive_id(speaker_names_raw), .before = 1) %>%
    ungroup()

  return(dfclean_filtered)
}
