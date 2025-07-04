#' summarize_dyads
#'
#' Calculates and appends 3 measures for quantifying alignment. Appends the mean score for each dimension by turn. Calculates and Spearman's rank correlation between interlocutor time series and appends by transcript. Calculates the area under the curve of the absolute difference time series between interlocutor time series. The length of the difference time series can be standardized the shortest number of exchanges present in the group using an internally defined resampling function, called with resample = TRUE. Spearman's rank correlation and area under the curve become less reliable for dyads under 30 exchanges.
#'
#' @name summarize_dyads
#' @param aligned_ts_df produced in the align_dyads function
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

summarize_dyads <- function(aligned_ts_df, additional_lags=NULL) {

  align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                        "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                        "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                        "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                        "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                        "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                        "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                        "sem_concreteness", "sem_diversity", "sem_neighbors")

  # summarize average dimensions by participant
  av_df <- aligned_ts_df %>% # remove all non-dimension columns
    dplyr::select(-c(ExchangeCount, TurnCount, CleanText, NWords_ByPersonTurn_RAW, NWords_ByPersonTurn_CLEAN, NWords_ByPersonTurn_CLEAN_ALIGNED)) %>%
    dplyr::group_by(Event_ID, Participant_ID) %>%
    dplyr::summarize(dplyr::across(tidyselect::contains(align_dimensions), ~ mean(.x, na.rm = T), .names = "{.col}_mean"),
                     dplyr::across(dplyr::everything(), dplyr::first), .groups = "drop") %>%
    dplyr::select(Event_ID, Participant_ID, tidyselect::contains("mean")) %>%
    tidyr::pivot_longer(
      cols = tidyselect::contains(align_dimensions),
      names_to = "Dimension",
      names_pattern = "(.*)_mean",
      values_to = "Average_Score"
    )

  # compute difference time series, interpolate, and compute AUC
  ## AUC function should take a 'who_first' argument that will be reported
  ## should throw warning for transcripts with ExC < 50

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
  find_auc_dyads <- function(aligned_df) {

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
      both_participant_cols <- widedf %>%
        dplyr::select(starts_with(dimension))
      # add a count to the data frame (x-axis)

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
          if (max(domain_ts$ExchangeCount) < 3) { # hard coded to three exchanges
            #create a single row, single column dataframe with one empty value to fill in the AUC
            doc_domain_auc_df <- data.frame(domain_auc = NA)
          }
          else {
            domain_ts <- data.frame(domain_ts) #make single emotion Time series a data frame
            domain_auc <- DescTools::AUC(x = domain_ts[,1], y = domain_ts[,2], method = "trapezoid")
            doc_domain_auc_df <- data.frame(domain_auc,
                                            Exchanges = max(domain_ts$ExchangeCount)) #make data frame of AUC, replicated once
          }
          doc_domain_auc_df
        },
        error = function(e) {
          # print file name and dimension that are behaving unexpectedly
          cat(paste("Results for dAUC will be filled with NA.\n\tTranscript: ",
                    doc_name, "\n\tDimension: ", dimension, "\n", sep = ""))
          # fill the result cell with NA
          doc_domain_auc_df <- data.frame(domain_auc = NA, Exchanges = max(domain_ts$ExchangeCount))
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
      # drop exchange column for all but first dimension
      if (dimension != xdimensions[1]) {
        all_doc_domain_auc_df <- data.frame(all_doc_domain_auc_df[,1])
      }
      colnames(all_doc_domain_auc_df)[1] <- paste("AUC", dimension, sep = "_")
      all_doc_domain_auc_df
    })

    all_domain_df <- dplyr::bind_cols(domain_auc_list, data.frame(Event_ID = names(long_diff_df_list))) #bind all columns of AUCs into one data frame
    # throw warning if any dyads are fewer than 50 exchanges
    small_dyads <- all_domain_df[which(all_domain_df$Exchanges < 50), "Event_ID"]
    if (length(small_dyads) > 0) {
      warning(paste0("Some conversations are shorter than 50 exchanges (100 turns). It is recomended that conversations are longer than 50 exchanges. Attached is a list of conversations with fewer than 50 exchanges:\n",
                     paste(small_dyads, collapse = "\n")))
    }
    # standardize each AUC to 50
    all_domain_df_s <- all_domain_df %>%
      dplyr::mutate(dplyr::across(tidyselect::contains("AUC"), ~ (50 * .x) / Exchanges)) %>%
      dplyr::select(-Exchanges)

    output_auc <- dplyr::left_join(all_domain_df, all_domain_df_s, by = "Event_ID", suffix = c("_raw", "_standard")) %>%
      dplyr::select(c(Event_ID, Exchanges, dplyr::everything()))

    return(output_auc)
  }
  #END DEFINE FIND DYAD AREA UNDER THE CURVE FUNCTION

  auc_df <- find_auc_dyads(aligned_df = aligned_ts_df)
  auc_df <- auc_df %>%
    dplyr::select(c("Event_ID", tidyselect::contains("AUC")))

  # compute spearman correlation and lagged pearson correlations

  summarize_dyads_covar <- function(aligned_ts_df, lags = c(-3, -2, -1, 0, 1, 2, 3)) {
    #remove empty levels of all factors in the data frame - specifically for any removed transcript event ids
    aligned_ts_df <- droplevels(aligned_ts_df)

    # pull out metadata into a separate df
    metaDf <- aligned_ts_df %>%
      dplyr::select(-c(Participant_ID, contains(align_dimensions),
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

        x_vars <- interp_df %>%
          dplyr::select(c('Event_ID') | c(tidyselect::ends_with("_S1"))) %>%
          dplyr::rename_with(~stringr::str_replace(., "_S1", ""),
                             tidyselect::ends_with("_S1"))
        y_vars <- interp_df %>%
          dplyr::select(c('Event_ID') | c(tidyselect::ends_with("_S2"))) %>%
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
            # print file name and dimension that are behaving unexpectedly
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

  covar_df <- summarize_dyads_covar(aligned_ts_df, lags = c(-2, 0, 2))

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


