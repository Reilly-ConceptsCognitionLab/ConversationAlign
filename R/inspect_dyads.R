#' inspect_dyads
#'
#' Creates a grouped dataframe of word analytics by speaker and dyad, as well as a faceted graph showing each dyadic time series that is saved to the local disc.
#'
#' @name inspect_dyads
#' @param aligned_ts_df aligned dataframe ported from the align_dyads() step
#' @param dimensions character vector of aligned dimensions to inspect. default selects all.
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join
#' @importFrom tidyverse mutate
#' @importFrom tidyverse group_by
#' @importFrom tidyverse ungroup
#' @importFrom tidyverse summarise
#' @importFrom tidyverse summarise(across())
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggsave
#' @export inspect_dyads

inspect_dyads <- function(aligned_ts_df, dimensions = "all") {
  inspect_analytics_output_df <- aligned_ts_df %>%
    dplyr::select(Event_id, ExchangeCount, Speaker_names_raw, starts_with("Analytics_"),
                  starts_with("Metadata_")) %>% #select only analytic, demo, and grouping columns
    #calculate how large a percentage of raw word were removed
    dplyr::mutate(Percent_stopword_removed = (Analytics_words_removed / Analytics_wordcount_raw)*100) %>%
    dplyr::group_by(Event_id, Speaker_names_raw) %>%
    dplyr::mutate(N_turns = max(ExchangeCount)) %>% #make number of turns the highest ExC from each speaker
    dplyr::ungroup() %>%
    dplyr::select(!c(ExchangeCount, Analytics_words_removed)) %>%
    tidyr::pivot_longer(cols = starts_with("Analytics_"), #pivot all analytic columns to 3 columns
                        names_to = c("Measure", "Version"),      #pivots to version (clean vs raw) and
                        names_prefix = "Analytics_",             #measurement (ie word count) + the value
                        names_pattern = "(.+)_([A-Za-z]+$)",
                        values_to = "Score") %>%
    dplyr::mutate(Version = str_to_sentence(Version), #make elements of new columns sentence case
                  Measure = str_to_sentence(Measure)) %>%
    dplyr::group_by(Measure) %>% #group and create a grouped sequence to aid pivot wider
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = Measure, values_from = Score) %>% #pivot measurements and scores
    dplyr::group_by(Event_id, Speaker_names_raw, Version) %>%
    dplyr::summarise(N_turns = first(N_turns), #summarise over dyad, speaker, and version
                     Wordcount_total = sum(Wordcount), #computes total WC, mean WC, mean word length
                     Words_per_turn_mean = mean(Wordcount),
                     Percent_stopword_removed = mean(Percent_stopword_removed),#preserves other measures
                     Mean_word_length = mean(Mean_word_length),
                     across(starts_with("Metadata_"), first),
                     .groups = "drop")
  #visualization portion of inspect
  aligned <- aligned_ts_df
  align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                        "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                        "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                        "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                        "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                        "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                        "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                        "sem_concreteness", "sem_diversity", "sem_neighbors")
  # for the dimensions argument, if it is all (default) then it is ignored, else it only selects given
  if (any(grepl("all", dimensions, ignore.case = TRUE)) == TRUE) {
    aligned <- aligned
  }
  else {
    all_dims <- colnames(dplyr::select(aligned, starts_with(align_dimensions) & ends_with(align_dimensions)))
    aligned <- aligned %>% dplyr::select(!setdiff(all_dims, dimensions))
  }
  #pivot data frame by every column with a specified dimension name
  align_long <- aligned %>%
    tidyr::pivot_longer(names_to = "Dimension", cols = any_of(align_dimensions),
                        values_to="Salience")
  align_long$Dimension <- as.factor(align_long$Dimension)
  align_long <- align_long %>%
    dplyr::rename("Speaker"="Speaker_names_raw")
  align_long$Speaker <- as.factor(align_long$Speaker)
  align_long$Event_id <- as.factor(align_long$Event_id)

  align_long_pairings <- align_long %>%
    dplyr::group_by(Event_id) %>%
    dplyr::mutate(Speaker_pair = as.factor(paste(unique(Speaker), collapse = "-"))) %>%
    dplyr::ungroup()

  alignplots <-  ggplot2::ggplot(align_long_pairings, ggplot2::aes(ExchangeCount, Salience, group=Speaker)) +
    ggplot2::geom_path(size= 0.2, linejoin = "round") +
    ggplot2::ylim(0, 10) +
    ggplot2::geom_line(ggplot2::aes(color=Speaker), size = 0.25) +
    jamie.theme +
    ggplot2::geom_text(mapping = ggplot2::aes(max(ExchangeCount)/2, y = 9, label = Speaker_pair), size = 3.25, color = "black") +
    ggplot2::facet_grid(Event_id ~ Dimension) +
    ggplot2::ggtitle("Plots facetted by dyad and dimension")
  #save a pdf file of the faceted graphs to the users disc
  ggplot2::ggsave("alignplots.pdf",width = 8, height = 15, dpi = 300)
  #create a list of the analytics data frame and the plot so both can be returned
  total_inspect_output <- list(inspect_analytics_output_df, alignplots)
  names(total_inspect_output) <- c("Inspect_analytics", "Inspect_alignment_visualization")
  return(total_inspect_output)
}

