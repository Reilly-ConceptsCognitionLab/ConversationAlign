#' summarize_dyads_covar
#'
#' Calculates Spearman rank correlation and lagged Pearson correlation between interlocutor time series for each dimension and transcript.
#'
#' @name summarize_dyads_covar
#' @param aligned_ts_df Dataframe produced in the align_dyads function
#' @param lags A vector of signed integers specifying lags that should be included for Pearson correlation. Negative integers are used for leads.
#' @return A data frame containing the Spearman and Pearson correlation for each transcript. Results are organized as column and data is pivoted longer by dimension for readability.
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename_at
#' @importFrom dplyr vars
#' @importFrom dplyr across
#' @importFrom dplyr first
#' @importFrom stringr str_replace
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom tidyr fill
#' @importFrom tidyselect ends_with
#' @importFrom tidyselect contains
#' @importFrom tidyselect everything
#' @importFrom stats cor.test
#' @importFrom zoo na.approx
#' @importFrom YRmisc cor.lag
#' @export


summarize_dyads_covar <- function(aligned_ts_df, lags = c(-3, -2, -1, 0, 1, 2, 3)) {
  # set variables to null to prevent notes:
  Participant_ID <- CleanText <- TurnCount <- ExchangeCount <- Event_ID <- NULL

  #remove empty levels of all factors in the data frame - specifically for any removed transcript event ids
  aligned_ts_df <- droplevels(aligned_ts_df)

  align_vars <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                  "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                  "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                  "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                  "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                  "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                  "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                  "sem_concreteness", "sem_diversity", "sem_neighbors")

  # pull out metadata into a seperate df
  metaDf <- aligned_ts_df %>%
    dplyr::select(-c(Participant_ID, contains(align_vars),
                     contains("Nwords"), CleanText, TurnCount, ExchangeCount)) %>%
    dplyr::group_by(Event_ID) %>%
    dplyr::summarize(across(tidyselect::everything(), dplyr::first))

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

  #DEFINE SPEARMAN'S CORRELATION FUNCTION
  spearmans_corr_dyads <- function(aligned_ts_df) {
    # set varibles to null to prevent note:
    Event_ID <- ExchangeCount <- Participant_ID <- dimension <- score <- NULL
    #first, flexibly find the dimensions the user has aligned on
    align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt",
                          "aff_happiness", "aff_hope", "aff_hostility", "aff_politeness",
                          "aff_sadness", "aff_stress", "aff_surprise", "aff_trust",
                          "aff_valence", "lex_age_acquisition",
                          "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                          "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                          "sem_concreteness", "sem_diversity", "sem_neighbors")

    align_var <- colnames(aligned_ts_df[,which(colnames(aligned_ts_df) %in% align_dimensions)])

    df_list <- split(aligned_ts_df, f = aligned_ts_df$Event_ID)
    #iterate over each newly split data frame.
    output_df_list <- lapply(df_list, function(df){

      #establish raw participant name and S1/S2 'key' - denoted by order of utterance
      participantvec <- unique(df$Participant_ID)

      #substitute participant names with transient variable to be replaced with real names later
      names(participantvec) <- c("S1", "S2")
      df$Participant_ID <- gsub(participantvec[1], names(participantvec)[1], df$Participant_ID)
      df$Participant_ID <- gsub(participantvec[2], names(participantvec)[2], df$Participant_ID)

      #create a wide data frame with each row containing both participants' turn aggregated scores
      df_wide <- df %>%
        dplyr::group_by(Event_ID, ExchangeCount, Participant_ID, .add = FALSE) %>%
        dplyr::summarise(dplyr::across(tidyselect::contains(align_var), ~ mean(.x, na.rm = TRUE)),
                         .groups = "drop")%>%
        tidyr::pivot_wider(names_from = tidyselect::contains("Participant_ID"),
                           values_from = align_var) #%>%

      # if there is only one aligned variable manually add that variable name to participant columns
      if (length(align_var) == 1){
        colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))] = paste(align_var[1], colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))], sep = "_")
      }

      df_wide <- df_wide %>%
        dplyr::select(Event_ID, ExchangeCount, tidyselect::contains(align_var))

      rows_with_na_ind <- apply(df_wide[,which(colnames(df_wide) %in% paste(align_var, "S1", sep = "_") | colnames(df_wide) %in% paste(align_var, "S2", sep = "_"))], 1, function(x){
        any(is.na(x) == TRUE & is.nan(x) == FALSE)
      })
      # only slice out rows if there is at least one, otherwise it will not index any rows
      if (any(rows_with_na_ind)) {
        df_wide <- df_wide[!rows_with_na_ind, ]
      }

      # interpolate over turns that are fully missing (if all words in the utterance have been removed)
      interp_df <- df_wide %>%
        dplyr::mutate(dplyr::across(tidyselect::contains(align_var),
                                    ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
        tidyr::fill(names(df_wide[, which(colnames(df_wide) %in% paste(align_var, "S1", sep = "_")|
                                            colnames(df_wide) %in% paste(align_var, "S2", sep = "_"))]), .direction = "updown") # fills up or down for initial and ending values

      # select the columns for one participant and remove suffixes
      x_vars <- interp_df %>%
        dplyr::select(c('Event_ID') | c(tidyselect::ends_with("_S1"))) %>%
        dplyr::rename_with(~stringr::str_replace(., "_S1", ""),
                           tidyselect::ends_with("_S1"))
      y_vars <- interp_df %>%
        select(c('Event_ID') | c(tidyselect::ends_with("_S2"))) %>%
        dplyr::rename_with(~stringr::str_replace(., "_S2", ""),
                           tidyselect::ends_with("_S2"))

      # function to calculate and format spearman correlation
      calculate_spearman_corr <- function(x_vars, y_vars, dim, align_var) {
        tryCatch({
          # pull the values for each participant out of the data frame as a vector
          dim_x_vars <- x_vars[[dim]]
          dim_y_vars <- y_vars[[dim]]

          #run spearman corr and format rho and p value into a data frame - with complete cases
          sc_results <- cor.test(dim_x_vars, dim_y_vars,
                                 method = "spearman", exact = F,
                                 na.action = "na.omit")

          sc_results_df <- data.frame(SpearR = sc_results$estimate)
          sc_results_df
        },
        error = function(e){
          # print file name and dimension that are behaving unexpectedly (TODO)
          cat(paste("Results for spearman correlation will be filled with NA.\n\tTranscript: ",
                    df$Event_ID, "\n\tDimension: ", dim, "\n", sep = ""))
          # fill results with NA
          sc_results_df <- data.frame(SpearR = NA)
          sc_results_df
        })
      }

      # function to calculate and format lagged Pearson correlation
      calculate_lag_corr <- function(x_vars, y_vars, dim, align_var) {
        # pull the values for each participant out of the data frame as a vectors
        dim_x_vars <- x_vars[[dim]]
        dim_y_vars <- y_vars[[dim]]

        # get the amount of lag and lead
        lagTimes <- lags[lags > 0]
        leadTimes <- lags[lags < 0]
        # set both at least equal to one so that the function works
        if (length(lagTimes) == 0) {
          lagTimes <- c(1)
        }
        if (length(leadTimes) == 0) {
          leadTimes <- c(1)
        }
        # calculate lagged correlation
        suppressWarnings((
          lo_full <- YRmisc::cor.lag(dim_x_vars, dim_y_vars,
                                     max(lagTimes), max(abs(leadTimes)))

        ))

        # get the index of column name with zero
        which0 <- which(colnames(lo_full) == "0")
        # grab just the lag times, add a negative if a lead
        colnames(lo_full)[0:which0] <- gsub("lag", "", colnames(lo_full)[0:which0])
        # same for lead
        colnames(lo_full)[which0:length(colnames(lo_full))] <- gsub("lead", "-", colnames(lo_full)[which0:length(colnames(lo_full))])
        colnames(lo_full) <- paste0("PRho_lag_", colnames(lo_full))

        # make a vector of names from the given lags
        selectNames <- paste0("PRho_lag_", lags)
        if (0 %in% lags) {selectNames <- append(selectNames, "0")}
        # select columns given by user
        lo_select <- lo_full %>%
          dplyr::select(tidyselect::contains(selectNames))
        lo_select
      }

      #iterate over variables to calculate spearman's correlation
      dyad_dim_sc_list <- lapply(align_var, function(dim){
        # run the trycatch function - either gives Rhos or NAs if there would be an issue
        sc_results_df <- calculate_spearman_corr(x_vars, y_vars, dim, align_var)
        # run the lagged covariance function
        lag_results_df <- calculate_lag_corr(x_vars, y_vars, dim, align_var)
        # put both results in  a single data frame
        covar_results_df <- dplyr::bind_cols(sc_results_df, lag_results_df)

        #add participant column only if it is the first iteration
        colnames(covar_results_df) <- paste(colnames(covar_results_df), dim, sep = "-")
        if (match(dim, align_var) == 1) {
          covar_results_df$Participant_ID <- paste(participantvec, collapse = "---")
        }
        covar_results_df
      })

      #bind and add event id to spearman correlation df
      dyad_covar <- dplyr::bind_cols(dyad_dim_sc_list, Event_ID = unique(df$Event_ID))
      dyad_covar$Who_lagged <- participantvec[2]
      # report the dyad data frame
      dyad_covar
    })
    all_dyad_df <- dplyr::bind_rows(output_df_list)
    return(all_dyad_df)
  }
  #END DEFINE SPEARMAN'S CORRELATION FUNCTIONs
  covar_df <- spearmans_corr_dyads(aligned_ts_df = aligned_ts_df) # bind the results into dataframe
  # order columns

  covar_df <- covar_df %>%
    dplyr::select(!c(Participant_ID)) %>%
    # pivot just by the DIMENSION
    tidyr::pivot_longer(cols = tidyselect::contains(c("SpearR", "PRho")),
                        names_pattern = "(.*)-(.*)", # match up to but not include the dimensions
                        names_to = c(".value", "Dimension")) %>%
    dplyr::left_join(metaDf, by = "Event_ID") # bind metadata to output by event

  #remove row names
  row.names(covar_df) <- NULL
  return(covar_df)
}
