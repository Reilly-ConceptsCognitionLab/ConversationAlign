#' describe_my_corpus
#'
#' Cleans and formats language transcripts from the read stage. Removes non-alphabetic characters and stopwords. Language transcripts can be lemmatized by calling lemmatize = TRUE. Vectorizes each utterance and reports the total word count and mean word length by interlocutor in each dyad. Also reports the number of words in each turn.
#' @name describe_my_corpus
#' @param df_clean takes dataframe produced from the clean_dyads() function
#' @param granularity provides analytics by person or by conversation, default is granularity="conversation"
#' @return dataframe with summary analytics for a conversation corpus
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom utils install.packages
#' @export

#TextColumns:
#Text_Prep (just with contractions split)
#Text_Clean (no stopwords)
#Text_CleanAligned (no stopwords, aligned with psycholinguistic dimensions)
#Export_as full summary dataframe for additional analyses or publkication ready table (flextable)
#Figure out which dataframe to give it, should have raw, cleaned no stops, cleaned no stops and no matches in lookup
#collapse_by- 'all' collapse to provide analytics averaged across all converations, "person" = produces separate analytics
# for each coversation (Event_ID)
# Analytics:
# Maybe break columns into three: raw, cleaned, aligned dimenensions
# Values:
# WordCount_Total
# ContentRatio
# WordCount_PerTurn
# NTurns
# TTR
# Morphemes_per_word
# Letters_per_word
# Syllables_per_word

summarize_wordcount <- function(df, granularity = "none") {
  #Rename for each condition, Yoke Text_Prep to lookup_db syllable count, word length, word frequency
  #create smaller dataframe
  lookup_4textprep <- lookup_db %>%
    mutate(FreqLog10_TextPrep=lex_wordfreqlg10_raw,
           FreqLog10_TextClean=lex_wordfreqlg10_raw,
           FreqLog10_TextCleanAlign=lex_wordfreqlg10_raw,
           WordLength_NLett_TextPrep= lex_letter_count_raw,
           WordLength_NLett_TextClean= lex_letter_count_raw,
           WordLength_NLett_TextCleanAlign= lex_letter_count_raw)
  %>% select(Freq_Text_Prep)

  df <- df %>% left_join(lookup_small, b=c("text_prep"="word"))
  df <- df %>% left_join(lookup_small, by=c("text_clean"="word"))
  df <- df %>% left_join(lookup_small, by=c("text_clean_align"="word"))

  # Group by the specified level
  if (granularity == "none") {
    # Don't group, count complete observations for both text columns
    result <- df %>%
      filter(!is.na(Text_Prep)) %>% dplyr::summarize(
        WordCount_Raw = n(), WordCount_NoStops = sum(!is.na(Text_Clean)))
  } else if (granularity == "conversation") {
    # Group by Event_ID and summarize both counts
    result <- df %>%
      filter(!is.na(Text_Prep)) %>%
      group_by(Event_ID) %>%
      summarize(
        WordCount_Raw = n(),
        WordCount_NoStops = sum(!is.na(Text_Clean)))
  } else if (granularity == "fine") {
    # Group by both Event_ID and Participant_ID and summarize
    result <- df %>%
      filter(!is.na(Text_Prep)) %>%
      group_by(Event_ID, Participant_ID) %>%
      summarize(
        WordCount_Raw = n(),
        WordCount_NoStops = sum(!is.na(Text_Clean)),
        .groups = "drop")
  } else {
    stop("Invalid granularity argument. Must be 'none', 'conversation', or 'fine'")
  }

  return(result)
}
}
