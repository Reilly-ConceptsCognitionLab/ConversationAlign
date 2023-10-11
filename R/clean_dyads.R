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
  read_data_frame <- read_ts_df


  # read_data_frame <- read_ts_df %>%
  #   dplyr::filter(speaker_names_raw != "Unknown") %>% #filter out common unwanted speaker names
  #   filter(speaker_names_raw != "unknown") %>%
  #   filter(speaker_names_raw != "speaker") %>%
  #   filter(speaker_names_raw != "speaker") %>%
  #   filter(speaker_names_raw != "Other") %>%
  #   filter(speaker_names_raw != "other") %>%
  #   filter(speaker_names_raw != "E") %>%
  #   filter(speaker_names_raw != "e") %>%
  #   filter(speaker_names_raw != "Experimenter") %>%
  #   filter(speaker_names_raw != "experimenter") %>%
  #   filter(speaker_names_raw != "Assistant") %>%
  #   filter(speaker_names_raw != "assistant")
  read_data_frame$speaker_names_raw <- as.factor(read_data_frame$speaker_names_raw) #convert variables to factor
  read_data_frame$event_id <- as.factor(read_data_frame$event_id)

  #ADD TO

  if (any(grepl("time", colnames(read_data_frame), ignore.case = TRUE)) == TRUE){
    #convert time from hh:mm:ss or mm:ss to milliseconds
    read_data_frame$time <- sapply(read_data_frame$time, function(x){
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

  read_data_frame$rawtext <- stringr::str_squish(read_data_frame$rawtext) #remove unneeded white space from text
  #pull all utterances into one string then split them by word into a vector
  total_raw <- paste(read_data_frame$rawtext, collapse = " ")
  total_raw_s <- str_squish(str_split_1(total_raw, " "))

  dfclean <- read_data_frame %>%
    dplyr::mutate(cleantext = clean(rawtext)) %>%  #run clean function on text, making a new column
    dplyr::select(!rawtext)# remove old raw text and grouping column
  #pull all cleaned utterances into a single string then split into a vector by word
  total_clean <- paste(dfclean$cleantext, collapse = " ")
  total_clean_s <- str_squish(str_split_1(total_clean, " "))

  dfclean_sep <- tidyr::separate_rows(dfclean, cleantext) # create row for each word in clean text

  dfclean_filtered <- dfclean_sep %>%     #remove rows where text is an empty string
    dplyr::filter(cleantext != "") %>%
    #new stuff for testing the word count analytic fun
    dplyr::group_by(event_id) %>% #add a turn count per dyad, which counts by speaker change
    dplyr::mutate(turncount = dplyr::consecutive_id(speaker_names_raw), .before = 1) %>%
    ungroup() %>%
    dplyr::mutate(an_wordcount_raw = length(total_raw_s),
                  an_wordcount_clean = length(total_clean_s),
                  an_word_removed_clean = an_wordcount_raw - an_wordcount_clean,
                  an_mean_word_length_raw = mean(nchar(total_raw_s)),
                  an_mean_word_length_clean = mean(nchar(total_clean_s)))

  return(dfclean_filtered)
}
