#' summarize_dyads_auc
#'
#' Calculates the area under the curve of the absolute difference time series between interlocutor time series. The length of the difference time series can be standardized the shortest number of exchanges present in the group using an internally defined resampling function, called with resample = TRUE. Area under the curve become less reliable for dyads under 30 exchanges.
#'
#' @name summarize_dyads_auc
#' @param aligned_ts_df data frame produced in the align_dyads function
#' @param resample logical, should each transcript time series be downsampled to the shortest length of the corpus
#' @return A data frame associating each transcript with the dAUC calculated for each dimension
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
#' @importFrom dplyr rename_at
#' @importFrom dplyr vars
#' @importFrom dplyr across
#' @importFrom dplyr first
#' @importFrom stringr str_replace
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom tidyr fill
#' @importFrom tidyselect starts_with
#' @importFrom tidyselect ends_with
#' @importFrom tidyselect contains
#' @importFrom tidyselect everything
#' @importFrom zoo na.approx
#' @export

summarize_dyads_auc <- function(aligned_ts_df, resample = TRUE) {
  # set variables to null to prevent notes:
  Event_ID <- Participant_ID <- NULL
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

  #DEFINE FIND DYAD AREA UNDER THE CURVE FUNCTION
  find_auc_dyads <- function(aligned_df, resample = TRUE) {
    # set variables to null to prevent note:
    Participant_ID <- Event_ID <- ExchangeCount <- participant_var <- Participant_Pair <- score <- NULL

    align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                          "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                          "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                          "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                          "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                          "sem_concreteness", "sem_diversity", "sem_neighbors")

    align_var <- colnames(aligned_df)[colnames(aligned_df) %in% align_dimensions]
    # split the data frame into a list by event id
    df_list <- split(aligned_df, f = aligned_df$Event_ID)
    # iterate over each event, replacing participant names with a transient filler variable
    df_list_speakvar <- lapply(df_list, function(df){
      svec <- unique(as.character(df$Participant_ID))
      df <- df %>%
        dplyr::mutate(participant_var = ifelse(as.character(Participant_ID) == svec[1], "S1", "S2"),
                      Participant_Pair = paste(sort(svec), collapse = "---"))
    })

    df_speakvar <- dplyr::bind_rows(df_list_speakvar)

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
      # grouped by event id, then each function is run across all the Time series individually

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

    #iterate over each aligned dimension, selecting only the scores for that dimension and pulling a difference value and subbing it in for the actual values
    for (dimension in align_var){
      both_participant_cols <- widedf %>% select(starts_with(dimension))
      absdiffcol <- data.frame(dimension = abs(both_participant_cols[,1] - both_participant_cols[,2]))
      widedf[which(colnames(widedf) %in% paste(dimension, c("S1", "S2"), sep = "_"))] <- absdiffcol
    }

    long_diff_df <- widedf %>%
      tidyr::pivot_longer(cols = c(ends_with("_S1") | ends_with("_S2")),
                          names_to = c("dimension", "Participant_ID"),
                          names_pattern = "(.*)_([^_]+)$",
                          values_to = "score") %>% # pivot longer by dimension and pid
      # pivot each dimension to a column
      tidyr::pivot_wider(names_from = dimension, values_from = score) %>%
      dplyr::filter(Participant_ID == "S1") %>% # remove every second row per turn
      dplyr::select(-Participant_ID) # remove pid column
    # split the difference data frame into a list based on event id
    long_diff_df_list <- split(long_diff_df, f = long_diff_df$Event_ID)

    # grab the aligned dimensions as a vector to iterate over
    xdimensions <- colnames(long_diff_df)[which(colnames(long_diff_df) %in% align_dimensions)]
    domain_auc_list <- lapply(xdimensions, function(dimension){ #iterate over emotion
      # now iterate over each dyad in the corpus

      # function to calculate AUC for single time series, return NA if there is an error
      calculate_auc <- function(domain_ts, doc_name, dimension) {
        tryCatch({
          # if time series has fewer points than the threshold, fill with NA
          if (max(domain_ts$ExchangeCount) < threshold) {
            #create a single row, single column dataframe with one empty value to fill in the AUC
            doc_domain_auc_df <- data.frame(domain_auc = NA)
          }
          else {
            domain_ts <- data.frame(domain_ts) #make single emotion Time series a data frame
            domain_auc <- DescTools::AUC(x = domain_ts[,1], y = domain_ts[,2], method = "trapezoid")
            doc_domain_auc_df <- data.frame(domain_auc) #make data frame of AUC, replicated once
          }
          doc_domain_auc_df
        },
        error = function(e) {
          # print file name and dimension that are behaving unexpectedly
          cat(paste("Results for dAUC will be filled with NA.\n\tTranscript: ",
                    doc_name, "\n\tDimension: ", dimension, "\n", sep = ""))
          # fill the result cell with NA
          doc_domain_auc_df <- data.frame(domain_auc = NA)
          doc_domain_auc_df
        })
      }

      # iterate over each document
      single_doc_auc <- lapply(long_diff_df_list, function(aligned_ts_df){


        domain_ts <- aligned_ts_df %>%
          dplyr::select(ExchangeCount,
                        tidyselect::contains(dimension)) # take dimension and time

        # put the function in here
        single_doc_domain_auc <- calculate_auc(domain_ts,
                                               doc_name = as.character(aligned_ts_df$Event_ID)[1],
                                               dimension = dimension)
      })
      #bind all docs AUCs for emotion into one column and add column prefix
      all_doc_domain_auc_df <- dplyr::bind_rows(single_doc_auc)
      colnames(all_doc_domain_auc_df) <- paste("AUC", dimension, sep = "_")
      all_doc_domain_auc_df
    })

    all_domain_df <- dplyr::bind_cols(domain_auc_list, data.frame(Event_ID = names(long_diff_df_list))) #bind all columns of AUCs into one data frame

    return(all_domain_df)
  }
  #END DEFINE FIND DYAD AREA UNDER THE CURVE FUNCTION

  auc_df <- find_auc_dyads(aligned_df = aligned_ts_df, resample = resample)
  auc_df <- auc_df %>%
    dplyr::select(c("Event_ID", tidyselect::contains("AUC")))
  return(auc_df)
}
