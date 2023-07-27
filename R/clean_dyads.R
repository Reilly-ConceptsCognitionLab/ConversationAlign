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
#' @importFrom stringi stri_remove_empty
#' @importFrom tidyr separate_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @export clean_dyads

clean_dyads <- function(read_ts_df) {
  read_data_frame <- read_ts_df %>%
    filter(Speaker_names_raw != "Unknown") %>% #filter out common unwanted speaker names
    filter(Speaker_names_raw != "unknown") %>%
    filter(Speaker_names_raw != "Speaker") %>%
    filter(Speaker_names_raw != "speaker") %>%
    filter(Speaker_names_raw != "Other") %>%
    filter(Speaker_names_raw != "other") %>%
    filter(Speaker_names_raw != "E") %>%
    filter(Speaker_names_raw != "e") %>%
    filter(Speaker_names_raw != "Experimenter") %>%
    filter(Speaker_names_raw != "experimenter") %>%
    filter(Speaker_names_raw != "Assistant") %>%
    filter(Speaker_names_raw != "assistant")
  read_data_frame$Speaker_names_raw <- as.factor(read_data_frame$Speaker_names_raw) #convert variables to factor
  read_data_frame$Doc_id <- as.factor(read_data_frame$Doc_id)

  #convert time from hh:mm:ss or mm:ss to milliseconds
  read_data_frame$Time <- sapply(read_data_frame$Time, function(x){
    if (any(grepl(":", x)) == TRUE) {  #checks for colons, indicative of mm:ss
      x <- as.numeric(unlist(str_split(x, ":"))) #breaks string into vector by colon placement
      if (length(x) == 2) { #shows just mm, ss
        sum((x[1]*60000), (x[2]*1000))      }
      else if ( length(xvec) == 3) { #shows hh, mm, ss
        sum((x[1]*3600000), (x[2]*60000), (x[3]*1000))}}
    else if (is.na(x) == TRUE) { #keeps NA time values as NA - may be a better way to do this?
      NA}})

  load("data/omissions_dyads23.rda") #load in omissions database

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

  read_data_frame$RawText <- str_squish(read_data_frame$RawText) #remove unneeded white space from text

  df_with_word_count <- read_data_frame %>%
    rowwise() %>% #group by individual row
    mutate(Analytics_wordcount_raw = length(stri_remove_empty(str_split_1(paste(RawText, collapse = " "), " "))), #create new column of word count by row
           Analytics_mean_word_length_raw = mean(nchar(stri_remove_empty(str_split_1(paste(RawText, collapse = " "), pattern = " "))))) %>% #create new column of average word length by row
    ungroup()

  dfclean <- df_with_word_count %>%
    mutate(CleanText = clean(RawText)) %>%  #run clean function on raw text, making a new column
    rowwise() %>% #group by individual row
    mutate(Analytics_wordcount_clean = length(stri_remove_empty(str_split_1(paste(CleanText, collapse = " "), " "))), # create word count column for cleaned text
           Analytics_mean_word_length_clean = mean(nchar(stri_remove_empty(str_split_1(paste(CleanText, collapse = " "), pattern = " "))))) %>% #create mean word length column for clean text
    ungroup() %>%
    select(!RawText)# remove old raw text and grouping column

  dfclean_sep <- tidyr::separate_rows(dfclean, CleanText) # create row for each word in clean text

  dfclean_filtered <- dfclean_sep %>%
    filter(CleanText != "")#remove rows where text is an empty string

  #calculate words removed from the difference between the raw word count and clean word count
  dfclean_filtered$Analytics_words_removed <- dfclean_filtered$Analytics_wordcount_raw - dfclean_filtered$Analytics_wordcount_clean

  return(dfclean_filtered)
}