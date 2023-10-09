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
  read_data_frame <- read_ts_df %>%
    dplyr::filter(speaker_names_raw != "Unknown") %>% #filter out common unwanted speaker names
    filter(speaker_names_raw != "unknown") %>%
    filter(speaker_names_raw != "speaker") %>%
    filter(speaker_names_raw != "speaker") %>%
    filter(speaker_names_raw != "Other") %>%
    filter(speaker_names_raw != "other") %>%
    filter(speaker_names_raw != "E") %>%
    filter(speaker_names_raw != "e") %>%
    filter(speaker_names_raw != "Experimenter") %>%
    filter(speaker_names_raw != "experimenter") %>%
    filter(speaker_names_raw != "Assistant") %>%
    filter(speaker_names_raw != "assistant")
  read_data_frame$speaker_names_raw <- as.factor(read_data_frame$speaker_names_raw) #convert variables to factor
  read_data_frame$event_id <- as.factor(read_data_frame$event_id)

  #ADD TO

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

  read_data_frame$rawtext <- stringr::str_squish(read_data_frame$rawtext) #remove unneeded white space from text

  df_with_word_count <- read_data_frame %>%
    dplyr::rowwise() %>% #group by individual row
    dplyr::mutate(an_wordcount_raw = length(stri_remove_empty(str_split_1(paste(rawtext, collapse = " "), " "))), #create new column of word count by row
                  an_mean_word_length_raw = mean(nchar(stri_remove_empty(str_split_1(paste(rawtext, collapse = " "), pattern = " "))))) %>% #create new column of average word length by row
    dplyr::ungroup()

  dfclean <- df_with_word_count %>%
    dplyr::mutate(cleantext = clean(rawtext)) %>%  #run clean function on text, making a new column
    dplyr::rowwise() %>% #group by individual row
    dplyr::mutate(an_wordcount_clean = length(stri_remove_empty(str_split_1(paste(cleantext, collapse = " "), " "))), # create word count column for cleaned text
                  an_mean_word_length_clean = mean(nchar(stri_remove_empty(str_split_1(paste(cleantext, collapse = " "), pattern = " "))))) %>% #create mean word length column for clean text
    dplyr::ungroup() %>%
    dplyr::select(!rawtext)# remove old raw text and grouping column

  dfclean_sep <- tidyr::separate_rows(dfclean, cleantext) # create row for each word in clean text

  dfclean_filtered <- dfclean_sep %>%     #remove rows where text is an empty string
    dplyr::mutate(an_words_removed = an_wordcount_raw - an_wordcount_clean) %>%
    dplyr::filter(cleantext != "") %>%
    #new stuff for testing the word count analytic fun
    dplyr::group_by(event_id) %>% #add a turn count per dyad, which counts by speaker change
    dplyr::mutate(turncount = dplyr::consecutive_id(speaker_names_raw), .before = 1) %>%
    dplyr::group_by(turncount, .add = TRUE) %>% #group by new turncount to aggregate word counts
    dplyr::mutate(an_wordcount_raw = ifelse(an_wordcount_clean[1] == length(an_wordcount_clean),
                                            an_wordcount_raw,
                                            #if turns were combined, iterate over each unique clean wc value, then each raw wc value in that and sum
                                            sum(sapply(unique(an_wordcount_clean), function(clean_num){
                                              u_raw_for_c <- unique(an_wordcount_raw[which(an_wordcount_clean == clean_num)])
                                              total_raw_for_c <- 0
                                              for (raw_num in u_raw_for_c){ #iterate over unique raw numbers in the clean numbers
                                                rows <- sum(an_wordcount_clean == clean_num & an_wordcount_raw == raw_num)
                                                total_raw <- raw_num * rows
                                                final_raw <- total_raw / rows # control for the number of rows (only add what is shown)
                                                total_raw_for_c <- total_raw_for_c + final_raw
                                              }
                                              total_raw_for_c #return the total raw word count for all raw words with x amount of clean
                                            }))),
                  an_wordcount_clean = n(), #aggregate clean word count to the number of rows occupied
                  an_words_removed = an_wordcount_raw - an_wordcount_clean, #create words removed by turn
                  an_mean_word_length_clean = mean(an_mean_word_length_clean), #aggregate word length stats
                  an_mean_word_length_raw = mean(an_mean_word_length_raw)) %>% ungroup()
  return(dfclean_filtered)
}
