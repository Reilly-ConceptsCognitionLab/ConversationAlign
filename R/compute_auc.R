#' compute_auc
#'
#' computes auc between conversation partners for each dyad
#' @name compute_spearman
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @keywords internal
#' @noRd


compute_auc <- function(df_prep) {
  #selects align_var by greppin on possible prefixes of dimensions
  align_var <- grep("^(emo_|lex_|sem_|phon_)", colnames(df_prep), value = TRUE, ignore.case = TRUE)

  # split the data frame into a list by event id
  df_list <- split(df_prep, f = df_prep$Event_ID)

  # iterate over each event, replacing participant names with a transient filler variable
  df_list_speakvar <- lapply(df_list, function(df){
    svec <- unique(as.character(df$Participant_ID))
    df <- df %>% dplyr::mutate(participant_var = ifelse(as.character(Participant_ID) == svec[1], "S1", "S2"),
                               Participant_Pair = paste(sort(svec), collapse = "---"))
  })

  df_speakvar <- dplyr::bind_rows(df_list_speakvar)

  # group by turn then take the average score for each turn count,then pivot on pids
  df_wide <- df_speakvar %>% dplyr::group_by(Event_ID, Exchange_Count, Participant_ID) %>%
    dplyr::summarise(dplyr::across(tidyselect::contains(align_var), ~ mean(.x, na.rm = TRUE)),
                     participant_var = dplyr::first(participant_var), Participant_Pair = dplyr::first(Participant_Pair),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = c("participant_var"), values_from = any_of(align_var))

  # if there is only one aligned variable manually add that variable name to participant columns
  if (length(align_var) == 1){
    colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))] = paste(align_var[1], colnames(df_wide)[which(colnames(df_wide) %in% c("S1", "S2"))], sep = "_")
  }

  # should remove uneven final exchanges here - need to grab the last exchange count of each row and if it is uneven, remove it.
  for (eid in unique(as.character(df_wide$Event_ID))){
    final_exc_rows <- df_wide[which(df_wide$Event_ID == eid & df_wide$Exchange_Count == max(df_wide$Exchange_Count[which(df_wide$Event_ID == eid)])),]
    if (nrow(final_exc_rows) == 1){
      df_wide <- df_wide[-which(df_wide$Event_ID == eid & df_wide$Exchange_Count == max(df_wide$Exchange_Count[which(df_wide$Event_ID == eid)])),]
    }
  }

  # here is the big iteration:
  split_pid_df_list <- sapply(c("S1", "S2"), function(temp_pid){
    # grab the columns that are needed for grouping or contain just on participant's scores
    p_df <- df_wide %>%
      dplyr::select(c("Event_ID", "Exchange_Count", "Participant_Pair") | c(ends_with(paste0("_", temp_pid))))

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
  # join the two participant data frame together by dyad and Exchange_Count
  widedf <- dplyr::left_join(split_pid_df_list[[1]], split_pid_df_list[[2]], by = c("Event_ID", "Participant_Pair", "Exchange_Count"))

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
  xdimensions <- colnames(long_diff_df)[which(colnames(long_diff_df) %in% align_var)]

  domain_auc_list <- lapply(xdimensions, function(dimension){ #iterate over emotion
    # now iterate over each dyad in the corpus

    # function to calculate AUC for single time series, return NA if there is an error
    calculate_auc <- function(domain_ts, doc_name, dimension) {
      tryCatch({
        # if time series has fewer points than the threshold, fill with NA
        if (max(domain_ts$Exchange_Count) < 3) { # hard coded to three exchanges
          #create a single row, single column dataframe with one empty value to fill in the AUC
          doc_domain_auc_df <- data.frame(domain_auc = NA,
                                          Exchanges = max(domain_ts$Exchange_Count))
        }
        else {
          domain_ts <- data.frame(domain_ts) #make single emotion Time series a data frame
          domain_auc <- DescTools::AUC(x = domain_ts[,1], y = domain_ts[,2], method = "trapezoid")
          doc_domain_auc_df <- data.frame(domain_auc,
                                          Exchanges = max(domain_ts$Exchange_Count)) #make data frame of AUC, replicated once
        }
        doc_domain_auc_df
      },
      error = function(e) {
        # print file name and dimension that are behaving unexpectedly
        cat(paste("Results for dAUC will be filled with NA.\n\tTranscript: ",
                  doc_name, "\n\tDimension: ", dimension, "\n", sep = ""))
        # fill the result cell with NA
        doc_domain_auc_df <- data.frame(domain_auc = NA, Exchanges = max(domain_ts$Exchange_Count))
        doc_domain_auc_df
      })
    }

    # iterate over each document
    single_doc_auc <- lapply(long_diff_df_list, function(df_prep){

      domain_ts <- df_prep %>%
        dplyr::select(Exchange_Count,
                      tidyselect::contains(dimension)) # take dimension and time

      # put the function in here
      single_doc_domain_auc <- calculate_auc(domain_ts,
                                             doc_name = as.character(df_prep$Event_ID)[1],
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

  output_auc <- dplyr::left_join(all_domain_df, all_domain_df_s, by = "Event_ID", suffix = c("_raw", "_scaled100")) %>%
    dplyr::select(c(Event_ID, Exchanges, dplyr::everything()))

  return(output_auc)
}
