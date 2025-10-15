#' generate_shams
#'
#' Generates a permutation of each individual dyad. Shuffled dyads may act as controls to their originals.
#'
#' @param df_prep Output dataframe of prep_dyads().
#' @param seed (Optional) a seed for reproducibility in random sampling
#' @returns
#' A dataframe similar to prepped dyads, with each participant's time series randomly shuffled.
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @export

generate_shams <- function(df_prep, seed = NULL) {
  # if a seed is given, set it
  if (is.null(seed)) { # if not given, pick a random seed
    seed = sample(1:100000, size = 1)
  }

  # summarize down to turn means
  turn_mean_df <- df_prep %>%
    dplyr::group_by(Event_ID, Exchange_Count, Participant_ID) %>%
    dplyr::summarize(
      dplyr::across(
        matches("^(emo_|lex_|phon_|sem_|df_)"),
        ~mean(.x, na.rm = T)
      ),
      # these can be included as a sanity check
      Text_Prep = paste(Text_Prep, collapse = " "),
      Text_Clean = paste(Text_Clean, collapse = " "),
      .groups = "drop"
    )

  # define function that will allow each column to be sampled identically
  sample_seed <- function(x, seed) {
    set.seed(seed)
    return(sample(x, size = length(x), replace = F))
  }

  # shuffle each participant's time series
  sham_df <- turn_mean_df %>%
    dplyr::group_by(Event_ID, Participant_ID) %>%
    dplyr::mutate(
      dplyr::across(
        c(matches("^(emo_|lex_|phon_|sem_|df_)"), Text_Prep, Text_Clean),
        ~sample_seed(.x, seed = seed)
      )
    ) %>%
    dplyr::group_by(Event_ID) %>%
    dplyr::mutate(Turn_Count = 1:dplyr::n(), .after = Event_ID)

  return(sham_df)
}
