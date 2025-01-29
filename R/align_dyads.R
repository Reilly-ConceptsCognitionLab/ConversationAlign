#' align_dyads
#'
#' Yokes user-specified semantic, affective, and phonological values to each word in a cleaned language transcript. Values are aligned by each individual word, and words that are not present in the database are dropped. The number of words dropped is reported by interlocutor in each dyad. Reports an exchange count, which counts by each pair of turns.
#'
#' @name align_dyads
#' @param clean_ts_df a dataframe cleaned and formatted during the clean_dyads() function
#' @return a dataframe one-word-per-row format with variables of interest appended
#' @importFrom magrittr %>%
#' @importFrom tidyselect any_of
#' @importFrom tidyselect contains
#' @importFrom tidyselect everything
#' @importFrom dplyr group_by
#' @importFrom dplyr  select
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr  mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr ungroup
#' @importFrom dplyr consecutive_id
#' @importFrom stringr str_split_1
#' @importFrom stringr str_squish
#' @importFrom stringi stri_count_words
#' @importFrom stats complete.cases
#' @importFrom utils read.csv
#' @importFrom utils select.list
#' @export

align_dyads <- function(clean_ts_df) {
  #allow the user to select what variables they want to align, or provide their own database(s) and subset them
  myvars <- select.list(c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                          "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness",
                          "aff_stress", "aff_surprise", "aff_trust", "aff_valence",
                          "lex_age_acquisition", "lex_letter_count_raw",
                          "lex_morphemecount_raw", "lex_prevalence", "lex_senses_polysemy",
                          "lex_wordfreqlg10_raw", "sem_arousal", "sem_concreteness",
                          "sem_diversity", "sem_neighbors"), preselect = NULL, multiple = TRUE,
                        title = writeLines("Select any number of variables you would like to align your conversation transcripts on."),
                        graphics = FALSE)
  var_selected <- lookup_db %>% #select desired columns from lookup_db
    dplyr::select(matches("^word$"), tidyselect::contains(myvars))
  #create variable containing the column names of each variable to be aligned
  var_aligners <- colnames(var_selected)[-grep("^word$", colnames(lookup_db), ignore.case = TRUE)]

  #join measures of each variable to each word in each transcript
  df_aligned <- dplyr::left_join(clean_ts_df, var_selected, by = c("CleanText" = "word"), multiple = "first")

  # index rows where a word could not be aligned on all dimensions and remove that word from the clean text
  df_aligned$CleanText[which(rowSums(is.na(df_aligned[,which(colnames(df_aligned) %in% var_aligners)])) == length(var_aligners))] <- ""
  # important to note here that the row is preserved, the text is subbed for an empty string

  # group on event and turn, then take the number of words in that turn which could be aligned on
  df_aligned_wordcount <- df_aligned %>%
    dplyr::group_by(Event_ID, TurnCount) %>%
    dplyr::mutate(NWords_ByPersonTurn_CLEAN_ALIGNED = stringi::stri_count_words(paste(CleanText, collapse = " "))) %>%
    dplyr::ungroup()

  #add an exchange count variable, which is one turn from each interlocutor
  df_aligned_wordcount$ExchangeCount <- ceiling(df_aligned_wordcount$TurnCount / 2)
  #rearrange the columns to be more readable
  df_aligned_ec <- df_aligned_wordcount %>%
    dplyr::select(tidyselect::any_of(c("Event_ID", "Participant_ID", "ExchangeCount", "TurnCount", "CleanText", "Time")),
                  tidyselect::contains(var_aligners), tidyselect::everything())
  return(df_aligned_ec)
}
