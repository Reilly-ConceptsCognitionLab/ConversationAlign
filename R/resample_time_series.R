#' resample_time_series
#'
#' Resample each time series in a list to a supplied threshold
#'
#' @name resample_time_series
#' @param df_list collection of time series data frames
#' @param threshold integer to resample each time series length to
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr across
#' @importFrom magrittr %>%
#' @importFrom tidyselect starts_with
#' @importFrom tidyselect ends_with
#' @importFrom tidyselect contains
#' @keywords internal
#' @noRd

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
  dflistscaled <- lapply(names(df_list), function(x){ #uses names of the
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
