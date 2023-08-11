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
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggsave
#' @export inspect_dyads

inspect_dyads <- function(aligned_ts_df, dimensions = "all", include_plot = FALSE) {
  inspect_analytics_output_df <- aligned_ts_df %>%
    dplyr::select(event_id, exchangecount, speaker_names_raw, starts_with("an_"),
                  starts_with("m_")) %>% #select only analytic, demo, and grouping columns
    #calculate how large a percentage of raw word were removed
    dplyr::mutate(percent_stopword_removed = (an_words_removed / an_wordcount_raw)*100) %>%
    dplyr::group_by(event_id, speaker_names_raw) %>%
    dplyr::mutate(n_turns = max(exchangecount)) %>% #make number of turns the highest ExC from each speaker
    dplyr::ungroup() %>%
    dplyr::select(!c(exchangecount, an_words_removed)) %>%
    tidyr::pivot_longer(cols = starts_with("an_"), #pivot all analytic columns to 3 columns
                        names_to = c("measure", "version"),      #pivots to version (clean vs raw) and
                        names_prefix = "an_",             #measurement (ie word count) + the value
                        names_pattern = "(.+)_([A-Za-z]+$)",
                        values_to = "score") %>%
    dplyr::mutate(version = version, #make elements of new columns sentence case
                  measure = measure) %>%
    dplyr::group_by(measure) %>% #group and create a grouped sequence to aid pivot wider
    dplyr::mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = measure, values_from = score) %>% #pivot measurements and scores
    dplyr::group_by(event_id, speaker_names_raw, version) %>%
    dplyr::summarise(n_turns = first(n_turns), #summarise over dyad, speaker, and version
                     wordcount_total = sum(wordcount), #computes total WC, mean WC, mean word length
                     words_per_turn_mean = mean(wordcount),
                     percent_stopword_removed = mean(percent_stopword_removed),#preserves other measures
                     mean_word_length = mean(mean_word_length),
                     across(starts_with("m_"), first),
                     .groups = "drop")

  if (include_plot == FALSE) {
    return(inspect_analytics_output_df)
  }
  else if (include_plot == TRUE) {
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
      dplyr::rename("speaker"="speaker_names_raw")
    align_long$speaker <- as.factor(align_long$speaker)
    align_long$event_id <- as.factor(align_long$event_id)

    align_long_pairings <- align_long %>%
      dplyr::group_by(event_id) %>%
      dplyr::mutate(speaker_pair = as.factor(paste(unique(speaker), collapse = "-"))) %>%
      dplyr::ungroup()

    alignplots <-  ggplot2::ggplot(align_long_pairings, ggplot2::aes(exchangecount, Salience, group=speaker)) +
      ggplot2::geom_path(size= 0.2, linejoin = "round") +
      ggplot2::ylim(0, 10) +
      ggplot2::geom_line(ggplot2::aes(color=speaker), size = 0.25) +
      ggplot2::geom_text(mapping = ggplot2::aes(max(exchangecount)/2, y = 9, label = speaker_pair), size = 3.25, color = "black") +
      ggplot2::facet_grid(event_id ~ Dimension) +
      ggplot2::ggtitle("Plots facetted by dyad and dimension")
    #save a pdf file of the faceted graphs to the users disc
    ggplot2::ggsave("alignplots.pdf",width = 8, height = 15, dpi = 300)
    #create a list of the analytics data frame and the plot so both can be returned
    total_inspect_output <- list(inspect_analytics_output_df, alignplots)
    names(total_inspect_output) <- c("Inspect_analytics", "Inspect_alignment_visualization")
    return(total_inspect_output)
  }
  else {stop("include_plot argument must be type logical", call. = FALSE)}
}

