#' summarize_dyads_slope
#'
#' Calculates the area under the curve of the absolute difference time series between interlocutor time series. The length of the difference time series can be standardized the shortest number of exchanges present in the group using an internally defined resampling function, called with resample = TRUE. Area under the curve become less reliable for dyads under 30 exchanges.
#'
#' @name summarize_dyads_slope
#' @param aligned_ts_df produced in the align_dyads function
#' @param resample logical stating whether time series should be downsampled to the shortest length present
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

summarize_dyads_slope <- function(aligned_ts_df, resample = TRUE) {
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


  #DEFINE TIME SERIES RESCALER
  resample_time_series <- function(df_list, threshold) {
    # set variables to null to prevent notes
    ExchangeCount <- Grouper <- NULL
    align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                          "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                          "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                          "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                          "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                          "sem_concreteness", "sem_diversity", "sem_neighbors")

    # if the data is not in a list, then it splits by Event_ID
    if (typeof(df_list) != "list"){
      df_list <- df_list %>% droplevels()
      df_list = split(df_list, f = df_list$Event_ID)
    }

    if (length(names(df_list)) == 0) {
      names(df_list) <- seq(1:length(df_list)) #assigns numeric name to list if no names are present
    }
    names(df_list) <- as.character(names(df_list))
    dflistscaled <- lapply(names(df_list), function(x){

      xname <- x
      x <- df_list[[match(xname, names(df_list))]]
      x <- data.frame(x)
      ts_select <- x %>%
        dplyr::select(tidyselect::contains(align_dimensions)) %>% #select emotion columns
        dplyr::mutate(ExchangeCount = seq(from = 0, to = (nrow(x)-1))) #add exchange count
      # here ExC gets reduced by 1 so that zero is the floor - this makes the calculations easier and the threshold is also reduced by one so that no new time points are added

      #if the ts has 3 or fewer exchange counts it shouldn't  be resampled, so will report NA
      if (max(ts_select$ExchangeCount) <= 2){
        final_ts <- ts_select %>%
          dplyr::mutate(Event_ID = unique(x$Event_ID),
                        Participant_Pair = unique(x$Participant_Pair))

        final_ts[,colnames(final_ts) %in% align_dimensions] <- NA
        final_ts
      }
      else{
        #computes how many points must be calculated from each currently present point
        scale_ratio <- threshold / max(ts_select$ExchangeCount)

        #if the transcript is the desired length already
        if (scale_ratio == 1) {
          rescale_final <- ts_select %>% #assign to var so that it will be smoothed
            dplyr::select(!ExchangeCount) #remove exchange count
        }
        # only down samples each time series
        else if (scale_ratio < 1) {
          grouper_var <- 1 / scale_ratio #take inverse of ratio to find size of groups
          grouper_var_floor <- floor(grouper_var) #round group size to nearest integer
          #creates a sequence to group Time counts, and adds up to the max Time if the count is not even.
          grouper_counter <- rep(seq(1:(nrow(ts_select)/grouper_var_floor)), each=grouper_var_floor)
          if (length(grouper_counter) < nrow(ts_select)) {
            grouper_counter[(length(grouper_counter)+1):nrow(ts_select)] <- max(grouper_counter) + 1
          }

          agg_int <- ts_select %>%
            dplyr::mutate(Grouper = grouper_counter) %>%
            dplyr::group_by(Grouper) %>% #create and group by the integer needed to downscale
            dplyr::summarise(dplyr::across(tidyselect::contains(colnames(x)), ~ mean(.x, na.rm = TRUE)), #aggregate by group
                             .groups = "drop") %>%
            dplyr::select(!Grouper)

          if (nrow(agg_int) == threshold + 1) {
            rescale_final <- agg_int
            rescale_final
          }
          else {
            #compute the number of points to remove after aggregating by lowest integer ratio
            point_diff <- (nrow(agg_int) - 1) - threshold
            #creates a new factor variable to distribute where aggregation occurs
            splitter <- (nrow(agg_int) - 1) / point_diff
            #evenly split indexes for aggregation across the Time series
            index_subs <-  floor(seq(from = splitter, to = nrow(agg_int), by = splitter))
            #generate a grouping sequence and substitute at indexes to create groups of two that will be aggregated
            second_grouper <- seq(1:nrow(agg_int))
            second_grouper[index_subs - 1] <- index_subs

            agg_final <- agg_int %>%
              dplyr::mutate(Grouper = second_grouper) %>%
              dplyr::group_by(Grouper) %>% #create and group by the integer needed to downscale
              dplyr::summarise(dplyr::across(tidyselect::contains(colnames(x)), ~ mean(.x, na.rm = TRUE)), #aggregate by group
                               .groups = "drop") %>%
              dplyr::select(!Grouper)
            rescale_final <- agg_final
          }
        } #end of down scaling section

        final_ts <- rescale_final %>%
          dplyr::mutate(ExchangeCount = seq(from = 1, to = threshold + 1), #create exchange count column
                        Event_ID = unique(x$Event_ID),
                        Participant_Pair = unique(x$Participant_Pair))
        #bind all new columns together into one data frame with an exchange count
        final_ts <- data.frame(final_ts)
      }
    })
    #returns a list of scaled and smoothed data frames
    return(dflistscaled)
  }
  #END DEFINE TIME SERIES RESCALER

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

  # before wrangling data, find the threshold if resampling
  if (is.logical(resample) == TRUE) {
    #mutate the max ExchangeCount for each dyad
    min_exc_max <- df_wide %>%
      dplyr::group_by(Event_ID) %>%
      dplyr::summarize(exc_max = max(ExchangeCount),
                       .groups = "drop")
    #take the minimum of the max exchange count by dyad variable after converting to a vector
    min_exc <- min(min_exc_max$exc_max)
    # reduce the threshold by one because the resample function will add an exchange
    threshold <- min_exc - 1

    if (resample == TRUE){
      if (threshold < 30) {
        warning(writeLines("the threshold for resampling is below 30 exchanges.\narea under the curve  becomes a less valid measures of alignment below 30 exchanges"))
      }
    }
    else {
      small_dyads <- unique(min_exc_max$Event_ID[which(min_exc_max$exc_max < 30)])
      warning(writeLines(paste("The following dyads are shorter than 30 exchange counts.\nArea under the curve becomes less valid below 30 exchanges", paste(small_dyads, collapse = "\n"), sep = "\n")))
      threshold <- 3
    }
  }
  else {
    stop("Argument resample must be logical")
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

    if (resample == TRUE) {
      p_df_list <- resample_time_series(p_df_list, threshold = threshold)
    }

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
