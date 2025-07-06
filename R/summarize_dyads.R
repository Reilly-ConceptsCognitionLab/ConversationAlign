#' summarize_dyads
#'
#' Calculates and appends 3 measures for quantifying alignment. Appends the mean score for each dimension by turn. Calculates and Spearman's rank correlation between interlocutor time series and appends by transcript. Calculates the area under the curve of the absolute difference time series between interlocutor time series. The length of the difference time series can be standardized the shortest number of exchanges present in the group using an internally defined resampling function, called with resample = TRUE. Spearman's rank correlation and area under the curve become less reliable for dyads under 30 exchanges.
#'
#' @name summarize_dyads
#' @param df_prep produced in the align_dyads function
#' @param additional_lags integer vector, should any lags be added in addition to -2, 0, 2
#' @importFrom DescTools AUC
#' @importFrom dplyr distinct
#' @importFrom dplyr first
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename_with
#' @importFrom dplyr rename_at
#' @importFrom dplyr across
#' @importFrom dplyr first
#' @importFrom stats cor.test
#' @importFrom stringr str_replace
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom tidyselect starts_with
#' @importFrom tidyselect ends_with
#' @importFrom tidyselect contains
#' @importFrom tidyselect everything
#' @importFrom utils tail
#' @importFrom zoo na.approx
#' @export summarize_dyads

summarize_dyads <- function(df_prep, additional_lags = NULL) {
  my_packages <- c("dplyr", "magrittr", "stringr", "stats", "tidyr", "tidyselect", "utils", "zoo")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }

  # Extract and Summarize Meta-Data - with safe handling
  df_meta <- df_prep %>%
    dplyr::select(
      Event_ID, Participant_ID,
      -c(Exchange_Count, Turn_Count, Text_Prep, Text_Clean),
      -matches("^(emo_|lex_|phon_|sem_|df_)"))

  # Check if there are any columns left besides Event_ID and Participant_ID
  meta_cols <- setdiff(names(df_meta), c("Event_ID", "Participant_ID"))

  # Only create summary if there are metadata columns to summarize
  if (length(meta_cols) > 0) {
    df_meta_sum <- df_meta %>%
      dplyr::group_by(Event_ID, Participant_ID) %>%
      dplyr::summarize(dplyr::across(.cols = everything(),
                                     .fns = ~ dplyr::first(.x),
                                     .names = "{.col}_summary"),
                       .groups = "drop")
  } else {
    df_meta_sum <- df_meta %>%
      dplyr::select(Event_ID, Participant_ID) %>%
      dplyr::distinct()
  }

  # Rest of the function remains the same until the final join...

  # Generate summary data by conversation and participant
  av_df <- df_prep %>%
    dplyr::select(Event_ID, Participant_ID, Exchange_Count, Turn_Count, Text_Clean, matches("^(sem_|lex_|emo_|phon_)")) %>%
    dplyr::group_by(Event_ID, Participant_ID) %>%
    dplyr::summarize(
      across(
        matches("^(sem_|lex_|emo_|phon_)"),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_mean"
      ),
      across(c(Exchange_Count, Turn_Count, Text_Clean), dplyr::first),
      .groups = "drop"
    ) %>%
    dplyr::select(Event_ID, Participant_ID, contains("mean")) %>%
    tidyr::pivot_longer(
      cols = matches("^(sem_|lex_|emo_|phon_).*_mean"),
      names_to = "Dimension",
      names_pattern = "(.*)_mean",
      values_to = "Average_Score"
    )

  # Clean up factors
  df_prep <- droplevels(df_prep)

  # Add participant pairs
  df_prep <- df_prep %>%
    dplyr::group_by(Event_ID) %>%
    dplyr::mutate(Participant_Pair = paste(sort(unique(Participant_ID)), collapse = "---")) %>%
    dplyr::ungroup()

  # Compute AUC and lag correlations
  auc_df <- compute_auc(df_prep = df_prep) %>%
    dplyr::select(c("Event_ID", contains("AUC")))

  user_lags <- c(-2, 0, 2)
  if (!is.null(additional_lags)) {
    user_lags <- sort(unique(append(user_lags, additional_lags)))
  }
  covar_df <- compute_lagcorr(df_prep, lags = user_lags)

  # Reshape AUC data
  auc_df_long <- auc_df %>%
    tidyr::pivot_longer(
      contains("AUC"),
      names_to = c("Dimension", "reshaped"),
      names_pattern = "AUC_(.*)_(raw|standard)",
      values_to = "AUC"
    ) %>%
    tidyr::pivot_wider(
      names_from = reshaped,
      names_prefix = "AUC_",
      values_from = AUC
    )

  # Combine all data frames - with conditional join
  output <- av_df %>%
    dplyr::left_join(auc_df_long, by = c("Event_ID", "Dimension")) %>%
    dplyr::left_join(covar_df, by = c("Event_ID", "Dimension"))

  # Only join metadata if there were columns to summarize
  if (length(meta_cols) > 0) {
    output <- output %>%
      dplyr::left_join(df_meta_sum, by = c("Event_ID", "Participant_ID"))
  }

  # Remove row names
  row.names(output) <- NULL

  return(output)
}
