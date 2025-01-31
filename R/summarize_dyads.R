#' summarize_dyads
#'
#' Calculates and appends 3 measures for quantifying alignment. Appends the mean score for each dimension by turn. Calculates and Spearman's rank correlation between interlocutor time series and appends by transcript. Calculates the area under the curve of the absolute difference time series between interlocutor time series. The length of the difference time series can be standardized the shortest number of exchanges present in the group using an internally defined resampling function, called with resample = TRUE. Spearman's rank correlation and area under the curve become less reliable for dyads under 30 exchanges.
#'
#' @name summarize_dyads
#' @param aligned_ts_df produced in the align_dyads function
#' @param resample logical, should each transcript time series be downsampled to the shortest length of the corpus
#' @importFrom DescTools AUC
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom dplyr arrange
#' @importFrom dplyr rename_with
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
#' @importFrom tidyselect starts_with
#' @importFrom tidyselect ends_with
#' @importFrom tidyselect contains
#' @importFrom tidyselect everything
#' @importFrom utils tail
#' @importFrom stats cor.test
#' @importFrom zoo na.approx
#' @export summarize_dyads


summarize_dyads <- function(aligned_ts_df, resample = TRUE) {
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
  #END DEFINE TIME SERIES RESCALER

  #DEFINE FIND DYAD AREA UNDER THE CURVE FUNCTION
  find_auc_dyads <- function(aligned_df, resample = TRUE) {
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
          warning(writeLines("the threshold for resampling is below 30 exchanges.\narea under the curve and spearman's rank correlation become less valid measures of alignment below 30 exchanges"))
        }
      }
      else {
        small_dyads <- unique(min_exc_max$Event_ID[which(min_exc_max$exc_max < 30)])
        warning(writeLines(paste("The following dyads are shorter than 30 exchange counts.\nMeasures of area under the curve and spearman's rank correlation become less valid below 30 exchanges", paste(small_dyads, collapse = "\n"), sep = "\n")))

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
        dplyr::group_by(Event_ID) %>% # groups and  interpolates all missing values between two points
        dplyr::mutate(dplyr::across(tidyselect::contains(align_var), ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
        tidyr::fill(tidyselect::contains(align_var), .direction = "downup") %>%
        dplyr::ungroup()

      # wrap the data frame in a list to preserve the structure (lapply will pull it out into a list of lists)
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
      tidyr::pivot_wider(names_from = dimension, values_from = score) %>% # pivot so that each dimension has a column
      dplyr::filter(Participant_ID == "S1") %>% # it is the difference score now, so remove every second row per turn
      dplyr::select(-Participant_ID) # remove pid column, as it is one value per turn
    # split the difference data frame into a list based on event id
    long_diff_df_list <- split(long_diff_df, f = long_diff_df$Event_ID) #split the data into a list to iterate over dyads
    # grab the aligned dimensions as a vector to iterate over
    xdimensions <- colnames(long_diff_df)[which(colnames(long_diff_df) %in% align_dimensions)]
    domain_auc_list <- lapply(xdimensions, function(dimension){ #iterate over emotion
      # now iterate over each dyad in the corpus
      single_doc_auc <- lapply(long_diff_df_list, function(aligned_ts_df){ #iterate over each document
        domain_ts <- aligned_ts_df %>%
          dplyr::select(ExchangeCount, tidyselect::contains(dimension)) #select only desired emotion and Time count
        # conditionally skip AUC calculation for a dyad if it has fewer ExchangeCounts than what was sampled to.
        # If resampling is turned off it will set to fill those with less than 3 ExchangeCounts (done by setting n = 3)
        if (max(domain_ts$ExchangeCount) < threshold){
          #create a single row, single column dataframe with one empty value to fill in the AUC
          doc_domain_auc_df <- data.frame(domain_auc = NA)
          doc_domain_auc_df
        }
        else {
          domain_ts <- data.frame(domain_ts) #make single emotion Time series a data frame
          domain_auc <- DescTools::AUC(x = domain_ts[,1], y = domain_ts[,2], method = "trapezoid") #take AUC of Time series

          doc_domain_auc_df <- data.frame(domain_auc) #make data frame of AUC, replicated once
        }
      })
      all_doc_domain_auc_df <- dplyr::bind_rows(single_doc_auc) #bind all docs AUCs for emotion into one column
      colnames(all_doc_domain_auc_df) <- paste("AUC", dimension, sep = "_") # add auc as colname prefix
      all_doc_domain_auc_df
    })

    all_domain_df <- dplyr::bind_cols(domain_auc_list, data.frame(Event_ID = names(long_diff_df_list))) #bind all columns of AUCs into one data frame

    return(all_domain_df)
  }
  #END DEFINE FIND DYAD AREA UNDER THE CURVE FUNCTION

  #DEFINE SPEARMAN'S CORRELATION FUNCTION
  spearmans_corr_dyads <- function(aligned_ts_df) {
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
      #establish raw participant name and S1/S2 'key' - done alphabetically
      participantvec <- sort(unique(df$Participant_ID))
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

      # check the maximum number of exchanges and if it is less than three, fill the dyad's results with NA
      if (nrow(df_wide) <= 3) {
        dyad_sc <- df_wide %>%
          tidyr::pivot_longer(cols = c(ends_with("S1") | ends_with("S2")),
                              names_to = c("dimension", "Participant_ID"),
                              names_pattern = "(.*)_([^_]+)$",
                              values_to = "score") %>%
          dplyr::distinct(Participant_ID, dimension, .keep_all = TRUE) %>%
          dplyr::mutate(score = NA,
                        Participant_ID = replace(Participant_ID,
                                                 which(Participant_ID == "S1"), as.character(participantvec)[1]),
                        Participant_ID = replace(Participant_ID,
                                                 which(Participant_ID == "S2"),
                                                 as.character(participantvec)[2]),
                        Participant_ID = as.factor(Participant_ID)) %>%
          tidyr::pivot_wider(names_from = dimension, values_from = score)
        #create a transient variables from the align dimension
        rho_cols <- dyad_sc %>% dplyr::select(tidyselect::contains(align_var))

        #add column prefixes to the rho columns
        rho_cols <- rho_cols %>%
          dplyr::rename_with(~stringr::str_c("SpearR_", .), .cols = tidyselect::everything())

        #bind the empty rho columns to dyad identifiers so that it can be bound to to total data frame.
        dyad_sc <- dyad_sc %>%
          dplyr::select(-c(ExchangeCount, tidyselect::contains(align_var)))
        dyad_sc <- dplyr::bind_cols(list(dyad_sc, rho_cols)) #bind rho cols together

        dyad_sc
      }
      else {
        # interpolate over turns that are fully missing (all words in the utterance have been cleaned out)
        interp_df <- df_wide %>%
          dplyr::mutate(dplyr::across(tidyselect::contains(align_var), ~ zoo::na.approx(.x, na.rm = FALSE))) %>%
          tidyr::fill(names(df_wide[, which(colnames(df_wide) %in% paste(align_var, "S1", sep = "_") |
                                              colnames(df_wide) %in% paste(align_var, "S2", sep = "_"))]),
                      .direction = "updown") # fills up or down for initial and ending values respectively

        # select the columns for one participant and remove suffixes
        x_vars <- interp_df %>% dplyr::select(c('Event_ID') | c(ends_with("_S1"))) %>%
          dplyr::rename_at(dplyr::vars(matches("_S1")), ~stringr::str_replace(., "_S1", ""))
        y_vars <- interp_df %>% select(c('Event_ID') | c(ends_with("_S2"))) %>%
          dplyr::rename_at(dplyr::vars(matches("_S2")), ~stringr::str_replace(., "_S2", ""))

        #iterate over variables to calculate spearmans correlation
        dyad_dim_sc_list <- lapply(align_var, function(dim){
          # pull the values for each participant out of the data frame as a vector
          dim_x_vars <- unlist(x_vars[,colnames(x_vars) %in% dim])
          dim_y_vars <- unlist(y_vars[,colnames(y_vars) %in% dim])

          #run spearman corr and format rho and p value into a data frame - with complete cases
          sc_results <- cor.test(dim_x_vars, dim_y_vars,
                                 method = "spearman", exact = F,
                                 na.action = "na.omit")

          sc_results_df <- data.frame(SpearR = rep(sc_results$estimate, 2))

          #add participant column only if it is the first iteration
          colnames(sc_results_df) <- paste(colnames(sc_results_df), dim, sep = "_")
          if (match(dim, align_var) == 1) {
            sc_results_df$Participant_ID <- participantvec
          }
          sc_results_df
        })

        #bind and add event id to spearman correlation df
        dyad_sc <- dplyr::bind_cols(dyad_dim_sc_list, Event_ID = unique(df$Event_ID))
      }
      dyad_sc
    })
    all_dyad_df <- dplyr::bind_rows(output_df_list)
    return(all_dyad_df)
  }
  #END DEFINE SPEARMAN'S CORRELATION FUNCTION

  auc_df <- find_auc_dyads(aligned_df = aligned_ts_df,
                           resample = resample)

  scorr_df <- spearmans_corr_dyads(aligned_ts_df = aligned_ts_df)
  #manually left join each summarize data frame together by participant pair and transcript
  output_df <- dplyr::left_join(aligned_ts_df, auc_df, by=c("Event_ID"))
  output_df <- dplyr::left_join(output_df, scorr_df, by = c("Event_ID", "Participant_ID"))
  return(output_df)
}
