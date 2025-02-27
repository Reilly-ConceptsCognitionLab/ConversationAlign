#' summarize_dyads_slope
#'
#' Calculates the intercept and slope of a simple linear regression for each interlocutor and the difference time series between them.
#'
#' @name summarize_dyads_slope
#' @param aligned_ts_df data frame produced in the align_dyads function
#' @return A data frame containing the intercept and slope of a linear regression of difference and interlocutor time series for each dimension
#' @importFrom DescTools AUC
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr across
#' @importFrom dplyr first
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr fill
#' @importFrom tidyselect ends_with
#' @importFrom tidyselect contains
#' @importFrom zoo na.approx
#' @importFrom stats lm
#' @export

summarize_dyads_slope <- function(aligned_ts_df) {
  # set variables to null to prevent notes
  Event_ID <- Participant_ID <- ExchangeCount <- participant_var <- Participant_Pair <- S1 <- S2 <- Difference <- Dimension <- Time_Series <- Score <- Results <- NULL
  #remove empty levels of all factors in the data frame - specifically for any removed transcript event ids
  aligned_ts_df <- droplevels(aligned_ts_df)

  #initial check for transcripts that do not have two interlocutors
  check_pnum <- aligned_ts_df %>%
    dplyr::group_by(Event_ID) %>%
    dplyr::summarize(p_count = length(unique(Participant_ID)),
                     .groups = "drop") #generate dataframe with number of interlocutors in each transcript

  err_ts_vec <- check_pnum$Event_ID[which(check_pnum$p_count != 2)]
  #check if any transcripts were identified and throw an error with the specific transcripts
  if (length(err_ts_vec) != 0) {
    stop("summarize_dyads requires that all transcripts have exactly two interlocutors.\ntranscripts with less than or greater than 2 interlocutors:\n",
         paste(err_ts_vec, collapse = "\n"))
  }

  #mutates an interlocutor pair variable, and preserves all columns
  aligned_ts_df <- aligned_ts_df %>%
    dplyr::group_by(Event_ID) %>% #add a column of concatenated interlocutor names by transcript
    dplyr::mutate(Participant_Pair = paste(sort(unique(Participant_ID)), collapse = "---")) %>%
    dplyr::ungroup()

  #DEFINE FUNCTION FOR COMPUTING SLOPE AND P OF LM
  get_lm_results <- function(colx) {
    # run simple linear regression over a count
    model <- stats::lm(colx ~ c(1:length(colx)))
    # pull out slope and p value of slope
    slope <- model$coefficients[2]
    intercept <- model$coef[1]
    # concatonate both into a string to be returned as a single column
    return_string = paste(slope, intercept, sep = "--")
    return(return_string)
  }
  #END DEFINE FUNCTION FOR COMPUTING SLOPE AND P OF LM

  align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                        "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                        "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                        "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                        "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                        "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                        "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                        "sem_concreteness", "sem_diversity", "sem_neighbors")

  align_var <- colnames(aligned_ts_df)[colnames(aligned_ts_df) %in% align_dimensions]
  # split the data frame into a list by event id
  df_list <- split(aligned_ts_df, f = aligned_ts_df$Event_ID)
  # iterate over each event, replacing participant names with a transient filler variable
  df_list_speakvar <- lapply(df_list, function(df){
    svec <- unique(as.character(df$Participant_ID))
    df <- df %>%
      dplyr::mutate(participant_var = ifelse(as.character(Participant_ID) == svec[1], "S1", "S2"),
                    Participant_Pair = paste(sort(svec), collapse = "---"))
  })

  df_speakvar <- dplyr::bind_rows(df_list_speakvar)

  # generate data frame with just dyad, PIDs, and associated speaker variable to bind later
  pid_key_df <- df_speakvar %>%
    dplyr::group_by(Event_ID, Participant_ID) %>%
    dplyr::summarize(participant_var = dplyr::first(participant_var), .groups = "drop")

  # group by turn then take the average score for each turn count,then pivot on pids
  df_wide <- df_speakvar %>%
    dplyr::group_by(Event_ID, ExchangeCount, Participant_ID) %>%
    dplyr::summarise(dplyr::across(tidyselect::contains(align_var), ~ mean(.x, na.rm = TRUE)),
                     participant_var = dplyr::first(participant_var),
                     Participant_Pair = dplyr::first(Participant_Pair),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = c("participant_var"), values_from = any_of(align_dimensions))

  # if there is only one aligned variable manually add that variable name to participant columns
  if (length(align_var) == 1){
    colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))] = paste(align_var[1], colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))], sep = "_")
  }

  # should remove uneven final exchanges here - need to grab the last exchange count of each row and if it is uneven, remove it.
  for (eid in unique(as.character(df_wide$Event_ID))){
    final_exc_rows <- df_wide[which(df_wide$Event_ID == eid & df_wide$ExchangeCount == max(df_wide$ExchangeCount[which(df_wide$Event_ID == eid)])),]
    if (nrow(final_exc_rows) == 1){
      df_wide <- df_wide[-which(df_wide$Event_ID == eid & df_wide$ExchangeCount == max(df_wide$ExchangeCount[which(df_wide$Event_ID == eid)])),]
    }
  }

  # here is the big iteration:
  split_pid_df_list <- sapply(c("S1", "S2"), function(temp_pid){
    # grab the columns that are needed for grouping or contain just on participant's scores
    p_df <- df_wide %>%
      dplyr::select(c("Event_ID", "ExchangeCount", "Participant_Pair") | c(ends_with(paste0("_", temp_pid))))

    # take the first column that matches an aligned variable
    first_var_col <- p_df[,which(colnames(p_df) %in% paste(align_var, temp_pid, sep = "_"))[1]]
    # index the rows that are either not NA or are NaN values (numbers and NaN)
    p_df <- p_df[which(is.na(first_var_col) == FALSE | is.nan(unlist(first_var_col)) == TRUE),]
    p_df_list <- split(p_df, f = p_df$Event_ID)

    p_df_altered <- dplyr::bind_rows(p_df_list)
    # interpolate here - fill forward for end, fill back for front, linear interpolation for the middle

    # grouped by event id, then each function is run across all the time series individually
    p_df_interp <- p_df_altered %>%
      dplyr::group_by(Event_ID) %>% # interpolates all missing values between two points
      dplyr::mutate(dplyr::across(tidyselect::contains(align_var),
                                  ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
      tidyr::fill(tidyselect::contains(align_var), .direction = "downup") %>%
      dplyr::ungroup()

    # wrap the data frame in a list to preserve structure (lapply will pull it into a list of lists)
    df_interp_names_list <- list(p_df_interp)
  })

  # join the two participant data frame together by dyad and exchangecount
  widedf <- dplyr::left_join(split_pid_df_list[[1]], split_pid_df_list[[2]], by = c("Event_ID", "Participant_Pair", "ExchangeCount"))

  lm_df <- widedf %>% # pivot longer by dimension but preserve individual interlocutor columns
    tidyr::pivot_longer(cols = tidyselect::contains(c("S1", "S2")),
                        names_to = c("Dimension", "Interlocutor"), names_pattern = "(.+)_(S\\d)",
                        values_to = "Score") %>%
    # compute difference time series
    tidyr::pivot_wider(names_from = "Interlocutor", values_from = "Score") %>%
    dplyr::mutate(Difference = abs(S1 - S2)) %>%
    # pivot longer by time series (interlocutors and difference)
    tidyr::pivot_longer(cols = c(S1, S2, Difference),
                        names_to = "Time_Series", values_to = "Score") %>%
    # compute lm for each time series and report values in a single column
    dplyr::group_by(Event_ID, Dimension, Time_Series) %>%
    dplyr::summarize(Results = get_lm_results(colx = Score), .groups = "drop") %>%
    #dplyr::rowwise() %>%
    dplyr::mutate(Slope = as.numeric(sub("--(.+)", "", Results)),
                  Intercept = as.numeric(sub("(.+)--", "", Results))) %>%
    dplyr::select(-Results)

  lm_df_pid <- lm_df %>% # join PIDs by dyad and speaker variable
    dplyr::left_join(pid_key_df, by = c("Event_ID", "Time_Series"="participant_var")) %>%
    # replace NAs with Difference
    dplyr::mutate(Time_Series = ifelse(is.na(Participant_ID),
                                       "Difference", as.character(Participant_ID))) %>%
    dplyr::select(-Participant_ID) # remove old PID column

  return(lm_df_pid)
}
