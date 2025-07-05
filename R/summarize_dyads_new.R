#' summarize_dyads
#'
#' Calculates and appends 3 measures for quantifying alignment. Appends the mean score for each dimension by turn. Calculates and Spearman's rank correlation between interlocutor time series and appends by transcript. Calculates the area under the curve of the absolute difference time series between interlocutor time series. The length of the difference time series can be standardized the shortest number of exchanges present in the group using an internally defined resampling function, called with resample = TRUE. Spearman's rank correlation and area under the curve become less reliable for dyads under 30 exchanges.
#'
#' @name summarize_dyads
#' @param df_prep produced in the align_dyads function
#' @param additional_lags integer vector, should any lags be added in addition to -2, 0, 2
#' @importFrom DescTools AUC
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

summarize_dyads <- function(df_prep, additional_lags=NULL) {
  my_packages <- c("dplyr", "magrittr", "stringr", "stats", "tidyr", "tidyselect", "utils", "zoo")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }

# generate summary data by conversation and participant on each dimension for main effects (binds at end)
# remove all non-dimension columns (Keep Exchagne_Count, Turn_Count, Text_Clean and any dimension vars)
  av_df <- df_prep %>%
  dplyr::select(Exchange_Count, Turn_Count, Text_Clean,  tidyselect::matches("^(sem_|lex_|emo_|phon_)")) %>%
  dplyr::group_by(Event_ID, Participant_ID) %>%
  dplyr::summarize(dplyr::across(
        tidyselect::matches("^(sem_|lex_|emo_|phon_)"), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"),
      dplyr::across(c(Exchange_Count, Turn_Count, Text_Clean), dplyr::first), .groups = "drop") %>%
  dplyr::select(Event_ID, Participant_ID, tidyselect::contains("mean")) %>%
  tidyr::pivot_longer(cols = tidyselect::matches("^(sem_|lex_|emo_|phon_).*_mean"),
      names_to = "Dimension", names_pattern = "(.*)_mean", values_to = "Average_Score")

#remove empty levels of all factors in the data frame - specifically for any removed transcript event ids
df_prep <- droplevels(df_prep)

#mutates an interlocutor pair variable, and preserves all columns
df_prep <- df_prep %>% dplyr::group_by(Event_ID) %>% #add a column of concatenated interlocutor names by transcript
dplyr::mutate(Participant_Pair = paste(sort(unique(Participant_ID)), collapse = "---")) %>% dplyr::ungroup()

#runs the 'compute_auc' internal function
auc_df <- comppute_auc(df_prep = df_prep)
auc_df <- auc_df %>% dplyr::select(c("Event_ID", tidyselect::contains("AUC")))

#runs the 'compute_lagg_corr' internal function with default lags -2,0,2

covar_df <- compute_lagg_corr(df_prep) # bind the results into dataframe

# reshape dataframe
covar_df <- covar_df %>% dplyr::select(!c(Participant_ID)) %>%
  # pivot just by the DIMENSION
  tidyr::pivot_longer(cols = tidyselect::contains(c("SpearR", "PRho")),
  names_pattern = "(.*)-(.*)", # match up to but not include the dimensions
  names_to = c(".value", "Dimension")) %>%
  dplyr::left_join(metaDf, by = "Event_ID") # bind metadata to output by event

#remove row names
row.names(covar_df) <- NULL
return(covar_df)
}

# set lags based on user input
user_lags <- c(-2, 0, 2)
if (!is.null(additional_lags)) {
  # organize into ascending order as well
  user_lags <- sort(unique(append(user_lags, additional_lags)))
}
covar_df <- summarize_dyads_covar(aligned_ts_df, lags = user_lags)

# pivot auc df longer by dimension
auc_df_long <- auc_df %>%
  tidyr::pivot_longer(
    tidyselect::contains("AUC"),
    names_to = c("Dimension", "reshaped"),
    names_pattern = "AUC_(.*)_(raw|standard)",
    values_to = "AUC"
  ) %>%
  tidyr::pivot_wider(names_from = reshaped, names_prefix = "AUC_", values_from = AUC)
# bind the auc and covariance data frames
output <- av_df %>%
  dplyr::left_join(auc_df_long, by = c("Event_ID", "Dimension")) %>%
  dplyr::left_join(covar_df, by = c("Event_ID", "Dimension"))

return(output)
}
