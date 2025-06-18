#' describe_corpus
#'
#' Produces a table of corpus analytics including numbers of complete observations at each step, word counts, lexical diversity (e.g., TTR), stopword ratios, etc. Granularity of the summary statistics are guided by the user (e.g., by conversation, by conversation and speaker, collapsed all)
#' @name describe_corpus
#' @param df_aligned takes dataframe produced from the df_prep() function
#' @param granularity provides analytics by person or by conversation, default is granularity="conversation"
#' @return dataframe with summary analytics for a conversation corpus
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_cols
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom stringr str_subset
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom utils install.packages
#' @export

#TextColumns:
#Text_Prep (just with contractions split), Text_Clean (no stopwords), Any dimension in dataframe counts (emo_, phono_, lex_, sem_)
#Export_as full summary dataframe for additional analyses or publkication ready table (flextable)
# Break output by raw (Text_Prep) and cleaned (Text_Clean) and all aligned dimensions
#Analytics
# WordCount_Total, TTR, Morphemes_per_word, Letters_per_word, NTurns, NSyll, Words_Per_Turn, TTR, ContentRatio,
#Missing Observations after alignment (% retained) by each dimension


describe_corpus <- function(df, granularity = "none", export_table = TRUE) {
  # Load required packages
  my_packages <- c("dplyr", "magrittr", "purrr", 'rlang', "stringr", "tibble")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }


# Join lookup for morpheme count, nletters, word frequency
  lookup_small <- lookup_Jul25 %>%
    dplyr::select(word, phon_n_lett, lex_freqlg10, lex_n_morphemes,
                  dplyr::matches("^emo_|^phon_|^sem_|^lex_")) %>%
    dplyr::rename(morpheme_count = lex_n_morphemes,letter_count = phon_n_lett,
                  freq_lg10 = lex_freqlg10, syllable_count = phon_nsyll)

  df_text_prep <- df %>% dplyr::left_join(lookup_small, by = c('Text_Prep' = 'word'))
  df_text_clean <- df %>% dplyr::left_join(lookup_small, by = c('Text_Clean' = 'word'))

  # Function to calculate stats for special columns
  calculate_special_stats <- function(data, group_vars = NULL) {
    special_cols <- names(data) %>%
      stringr::str_subset("^(emo_|phon_|sem_|lex_)")

    if (length(special_cols) == 0) return(NULL)

    if (is.null(group_vars)) {
      data %>% dplyr::summarize(dplyr::across(dplyr::all_of(special_cols),
                                       list(complete = ~sum(!is.na(.)),
                                         missing = ~sum(is.na(.))
                                       ),
                                       .names = "{.col}_{.fn}")) %>%
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = c("measure", "stat"), names_sep = "_",
                            values_to = "value") %>% tidyr::unite("measure", measure, stat, sep = "_")
    } else {
      data %>% dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        dplyr::summarize(dplyr::across(dplyr::all_of(special_cols),
                                       list(complete = ~sum(!is.na(.)),
                                         missing = ~sum(is.na(.))
                                       ),
                                       .names = "{.col}_{.fn}"),
                         .groups = "drop") %>%
        tidyr::pivot_longer(cols = -dplyr::all_of(group_vars),
                            names_to = c("measure", "stat"),
                            names_sep = "_",
                            values_to = "value") %>%
        tidyr::unite("measure", measure, stat, sep = "_")
    }
  }

  # fn to calculate stats for a given text column
  calculate_stats <- function(data, text_col, group_vars = NULL) {
    text_col_sym <- rlang::sym(text_col)

    if (is.null(group_vars)) {
      data %>%
        dplyr::summarize(
          # Basic text stats
          Total_Observations = sum(!is.na(!!text_col_sym)),
          Missing_Observations = sum(is.na(!!text_col_sym)),
          Unique_Observations = dplyr::n_distinct(!!text_col_sym, na.rm = TRUE),
          Stopword_Count = sum(is.na(!!text_col_sym)),
          Stopword_Percentage = sum(is.na(!!text_col_sym)) / dplyr::n() * 100,
          TTR = dplyr::n_distinct(!!text_col_sym, na.rm = TRUE) / sum(!is.na(!!text_col_sym)),
          Turn_Count = max(Turn_Count, na.rm = TRUE),
          Words_Per_Turn = sum(!is.na(!!text_col_sym)) / max(Turn_Count, na.rm = TRUE),

          # Letter count stats
          Letters_Per_Word_mean = mean(letter_count, na.rm = TRUE),
          Letters_Per_Word_min = min(letter_count, na.rm = TRUE),
          Letters_Per_Word_max = max(letter_count, na.rm = TRUE),

          # Frequency stats
          Freqlg10_Per_Word_mean = mean(freq_lg10, na.rm = TRUE),
          Freqlg10_Per_Word_min = min(freq_lg10, na.rm = TRUE),
          Freqlg10_Per_Word_max = max(freq_lg10, na.rm = TRUE),

          # Syllable stats
          Syllables_Per_Word_mean = mean(syllable_count, na.rm = TRUE),
          Syllables_Per_Word_min = min(syllable_count, na.rm = TRUE),
          Syllables_Per_Word_max = max(syllable_count, na.rm = TRUE),
          .groups = "drop")
    } else {
      data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        dplyr::summarize(
          # Basic text stats
          Total_Observations = sum(!is.na(!!text_col_sym)),
          Missing_Observations = sum(is.na(!!text_col_sym)),
          Unique_Observations = dplyr::n_distinct(!!text_col_sym, na.rm = TRUE),
          Stopword_Count = sum(is.na(!!text_col_sym)),
          Stopword_Percentage = sum(is.na(!!text_col_sym)) / dplyr::n() * 100,
          TTR = dplyr::n_distinct(!!text_col_sym, na.rm = TRUE) / sum(!is.na(!!text_col_sym)),
          Turn_Count = max(Turn_Count, na.rm = TRUE),
          Words_Per_Turn = sum(!is.na(!!text_col_sym)) / max(Turn_Count, na.rm = TRUE),

          # Morpheme stats
          Morphemes_Per_Word_mean = mean(morpheme_count, na.rm = TRUE),
          Morphemes_Per_Word_min = min(morpheme_count, na.rm = TRUE),
          Morphemes_Per_Word_max = max(morpheme_count, na.rm = TRUE),

          # Letter count stats
          Letters_Per_Word_mean = mean(letter_count, na.rm = TRUE),
          Letters_Per_Word_min = min(letter_count, na.rm = TRUE),
          Letters_Per_Word_max = max(letter_count, na.rm = TRUE),

          # Frequency stats
          Freqlg10_Per_Word_mean = mean(freq_lg10, na.rm = TRUE),
          Freqlg10_Per_Word_min = min(freq_lg10, na.rm = TRUE),
          Freqlg10_Per_Word_max = max(freq_lg10, na.rm = TRUE),

          # Syllable stats
          Syllables_Per_Word_mean = mean(syllable_count, na.rm = TRUE),
          Syllables_Per_Word_min = min(syllable_count, na.rm = TRUE),
          Syllables_Per_Word_max = max(syllable_count, na.rm = TRUE),
          .groups = "drop")
    }
  }

  # Determine grouping variables based on granularity
  group_vars <- dplyr::case_when(
    granularity == "none" ~ NULL,
    granularity == "conversation" ~ "Event_ID",
    granularity == "fine" ~ c("Event_ID", "Participant_ID"),
    TRUE ~ stop("Invalid granularity argument. Must be 'none', 'conversation', or 'fine'")
  )

  # Calculate stats for both text columns
  stats_prep <- calculate_stats(df_text_prep, "Text_Prep", group_vars)
  stats_clean <- calculate_stats(df_text_clean, "Text_Clean", group_vars)

  # Calculate special stats
  special_stats_prep <- calculate_special_stats(df_text_prep, group_vars)
  special_stats_clean <- calculate_special_stats(df_text_clean, group_vars)

  # Combine results
  if (is.null(group_vars)) {
    # For non-grouped results
    result <- dplyr::bind_cols(
      tibble::tibble(
        measure = c(
          "Total_Observations", "Missing_Observations", "Unique_Observations",
          "Stopword_Count", "Stopword_Percentage", "TTR", "Turn_Count", "Words_Per_Turn",
          "Letters_Per_Word_mean", "Letters_Per_Word_min", "Letters_Per_Word_max",
          "Freqlg10_Per_Word_mean", "Freqlg10_Per_Word_min", "Freqlg10_Per_Word_max",
          "Syllables_Per_Word_mean", "Syllables_Per_Word_min", "Syllables_Per_Word_max",
          if (!is.null(special_stats_prep)) unique(special_stats_prep$measure)
        )
      ),
      tibble::tibble(
        Text_Prep = c(
          unlist(stats_prep[1, ]),
          if (!is.null(special_stats_prep)) special_stats_prep$value
        ),
        Text_Clean = c(
          unlist(stats_clean[1, ]),
          if (!is.null(special_stats_clean)) special_stats_clean$value
        )
      )
    )
  } else {
    # For grouped results
    result <- dplyr::full_join(
      stats_prep %>%
        tidyr::pivot_longer(cols = -dplyr::all_of(group_vars),
                            names_to = "measure",
                            values_to = "Text_Prep"),
      stats_clean %>%
        tidyr::pivot_longer(cols = -dplyr::all_of(group_vars),
                            names_to = "measure",
                            values_to = "Text_Clean"),
      by = c(group_vars, "measure")
    )

    if (!is.null(special_stats_prep) && !is.null(special_stats_clean)) {
      special_stats <- dplyr::full_join(
        special_stats_prep %>% dplyr::rename(Text_Prep = value),
        special_stats_clean %>% dplyr::rename(Text_Clean = value),
        by = c(group_vars, "measure"))

        result <- dplyr::bind_rows(result, special_stats)
    }
  }

  return(result)
}
