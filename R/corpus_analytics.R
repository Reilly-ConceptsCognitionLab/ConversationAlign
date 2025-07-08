#' corpus_analytics
#'
#' Produces a table of corpus analytics including numbers of complete observations at each step, word counts, lexical diversity (e.g., TTR), stopword ratios, etc. Granularity of the summary statistics are guided by the user (e.g., by conversation, by conversation and speaker, collapsed all)
#' @name corpus_analytics
#' @param dat_prep takes dataframe produced from the df_prep() function
#' @return dataframe with summary analytics for a conversation corpus
#' @importFrom dplyr across
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr matches
#' @importFrom dplyr mutate
#' @importFrom dplyr n_distinct
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @importFrom stats sd
#' @importFrom stats na.omit
#' @importFrom stringr str_subset
#' @importFrom stringr str_trim
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate
#' @importFrom tidyselect where
#' @importFrom utils install.packages
#' @export

#Text_Prep (just with contractions split), Text_Clean (no stopwords), Any dimension in dataframe counts (emo_, phono_, lex_, sem_)
#Group_by Event_ID for most stats
# WordCount, TTR, Morphemes_per_word, Letters_per_word, Exchanges, NSyll, Words_Per_Turn

#Complete this summarize function so that the grouped dataframe outputs new variables:
# 'conversations (total)' = number of distinct levels of 'Event_ID'
# 'tokens (all conversations) = sum of all complete obs of Text_Prep across all levels of Event_ID
# 'exchanges-per-conversation (mean, sd, min, max) = exchanges per 'Event_ID' derived by the maximum exchange_count for each Event_ID.
# words-per-conversation_raw (mean, sd, min, max): complete observations of 'Text_Prep' for each level of Event_ID
# words-per-conversation_clean (mean, sd, min, max): complete observations of 'Text_Clean' for each level of Event_ID
# retention rate post cleaning = Number of complete observations for: Text_Clean / Text_Prep
# morphemes-per-word (mean, sd, min, max): across all complete obs of 'lex_n_morphemes' by level of Event_ID
# letter count (mean, sd, min, max): across all complete obs of 'phon_n_letters' by level of Event_ID
# word freq (mean, sd, min, max): across all complete obs of 'lex_freqlg10' by level of Event_ID
# words-per-turn (raw) (mean, sd, min, max): group_by turn_count, Event_ID, count complete obs per level of turn_count in Text_Prep
# words-per-turn (clean) (mean, sd, min, max): group_by turn_count, Event_ID, count complete obs per level of turn_count in Text_Clean
# TTR (raw): Group by Event_ID, distinct Text_Prep divided by Text_Prep
# TTR (clean): Group by Event_ID, distinct Text_Clean divided by Text_Clean

corpus_analytics <- function(dat_prep) {
  # Load required packages
  my_packages <- c("dplyr", "magrittr", "stringr", "tibble", "tidyr", "purrr", "stats", "tidyselect")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }

  # Select and prepare data
  dat_prep <- dat_prep %>%
    dplyr::select(Event_ID, Participant_ID, Exchange_Count, Turn_Count, Text_Prep, Text_Clean,
                  dplyr::matches("^emo_|^phon_|^sem_|^lex_"))

  lookup <- lookup_Jul25 %>% dplyr::select(word, phon_n_lett, phon_nsyll, lex_freqlg10, lex_n_morphemes)

  # Join dat_prep with psycholing vars norms for table (NO ROUNDING)
  dat_prep_plusvals <- dat_prep %>%
    dplyr::left_join(lookup, by = c("Text_Clean" = "word"))
  # Removed: mutate(across(where(is.numeric), ~round(., 2)))

  # Calculate totals counts (n-conversations, n-tokens ALL)
  total_tokens_raw <- sum(!is.na(dat_prep_plusvals$Text_Prep))
  total_tokens_clean <- sum(!is.na(dat_prep_plusvals$Text_Clean))
  n_conversations <- n_distinct(dat_prep_plusvals$Event_ID)

  # First calculate words per turn for each turn in each conversation
  words_per_turn_stats <- dat_prep_plusvals %>%
    group_by(Event_ID, Turn_Count) %>%
    summarize(
      words_per_turn_raw = sum(!is.na(Text_Prep)),
      words_per_turn_clean = sum(!is.na(Text_Clean)),
      .groups = 'drop'
    ) %>%
    group_by(Event_ID) %>%
    summarize(
      words_per_turn_raw = mean(words_per_turn_raw, na.rm = TRUE),
      words_per_turn_clean = mean(words_per_turn_clean, na.rm = TRUE),
      .groups = 'drop'
    )

  # Stats for each conversation (NO ROUNDING)
  conversation_stats <- dat_prep_plusvals %>%
    group_by(Event_ID) %>%
    summarize(
      total_exchanges = max(Exchange_Count, na.rm = TRUE),
      words_raw = sum(!is.na(Text_Prep)),
      words_clean = sum(!is.na(Text_Clean)),
      retention_rate = sum(!is.na(Text_Clean))/sum(!is.na(Text_Prep)),
      morphemes = mean(lex_n_morphemes, na.rm = TRUE),
      letters = mean(phon_n_lett, na.rm = TRUE),
      freq = mean(lex_freqlg10, na.rm = TRUE),
      ttr_raw = dplyr::n_distinct(Text_Prep, na.rm = TRUE)/sum(!is.na(Text_Prep)),
      ttr_clean = dplyr::n_distinct(Text_Clean, na.rm = TRUE)/sum(!is.na(Text_Clean)),
      .groups = 'drop'
    ) %>%
    left_join(words_per_turn_stats, by = "Event_ID")
  # Removed: mutate(across(where(is.numeric), ~round(., 2)))

  # Verify we have multiple conversations to compute SD
  if (nrow(conversation_stats) < 2) {
    warning("Insufficient conversations (n < 2) to compute meaningful standard deviations")
  }

  # Calculate summary statistics across all conversations (NO ROUNDING)
  result <- purrr::map_dfr(
    list(
      "exchange count (by conversation)" = conversation_stats$total_exchanges,
      "word count raw (by conversation)" = conversation_stats$words_raw,
      "word count clean (by conversation)" = conversation_stats$words_clean,
      "cleaning retention rate (by conversation)" = conversation_stats$retention_rate,
      "morphemes-per-word (by conversation)" = conversation_stats$morphemes,
      "letters-per-word (by conversation)" = conversation_stats$letters,
      "lexical frequency lg10 (by conversation)" = conversation_stats$freq,
      "words-per-turn raw (by conversation)" = conversation_stats$words_per_turn_raw,
      "words-per-turn clean (by conversation)" = conversation_stats$words_per_turn_clean,
      "TTR raw (by conversation)" = conversation_stats$ttr_raw,
      "TTR clean (by conversation)" = conversation_stats$ttr_clean
    ),
    function(x) {
      tibble(
        mean = mean(x, na.rm = TRUE),
        stdev = ifelse(length(na.omit(x)) > 1, sd(x, na.rm = TRUE), NA_real_),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    },
    .id = "measure"
  )
  # Removed: mutate(across(c(mean, stdev, min, max), ~round(., 2)))

  # Add summary rows for corpus-level totals
  summary_rows <- tibble(
    measure = c("total number of conversations", "token count all conversations (raw)", "token count all conversations (post-cleaning)"),
    mean = c(n_conversations, total_tokens_raw, total_tokens_clean),
    stdev = NA_real_,
    min = NA_real_,
    max = NA_real_
  )

  # Combine results
  final_result <- bind_rows(summary_rows, result) %>%
    select(measure, mean, stdev, min, max)

  return(final_result)
}
