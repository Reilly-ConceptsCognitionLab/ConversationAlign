#' summarize_dyads
#'
#' appends AUC and Spearman Rank Correlation indices to each dyad (event_id) using a resampling algoirthm that defaults to the minimum number of exchanges across all documents entered
#'
#' @name summarize_dyads
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom DescTools AUC
#' @importFrom stats cor.test
#' @export summarize_dyads


summarize_dyads <- function(aligned_ts_df, resample_yes_or_no = TRUE, resample_n = "min") {
  #DEFINE FIND MAIN EFFECT FUNCTION
  find_main_effect_dyads <- function(aligned_ts_df, aggregate_the_data = TRUE) {
    align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt",
                          "aff_happiness", "aff_hope", "aff_hostility", "aff_politeness",
                          "aff_sadness", "aff_stress", "aff_surprise", "aff_trust",
                          "aff_valence", "lex_age_acquisition",
                          "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                          "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                          "sem_concreteness", "sem_diversity", "sem_neighbors")

    align_var <- colnames(select(aligned_ts_df, starts_with(align_dimensions) & ends_with(align_dimensions)))
    #mutates new mean columns for each aligned variable, adds a speaker pair variable, and preserves all columns
    df_averaged_long <- aligned_ts_df %>%
      dplyr::group_by(event_id, speaker_names_raw) %>% #group by speaker and dyad
      dplyr::mutate(across(starts_with(align_var), mean, .names = "mean_{col}")) %>%
      group_by(event_id, .add = FALSE) %>% #add a column of concatenated speaker names by transcript
      mutate(speaker_pair = paste(sort(unique(speaker_names_raw)), collapse = "---")) %>%
      ungroup()
    return(df_averaged_long)

  }
  #END DEFINE MAIN EFFECT FUNCTION
  find_diff_ts <- function(aligned_df = aligned_ts_df){
    #define the built in variables that we can align with
    align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                          "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                          "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                          "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                          "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                          "sem_concreteness", "sem_diversity", "sem_neighbors")

    align_var <- colnames(aligned_df)[colnames(aligned_df) %in% align_dimensions]

    df_list <- split(aligned_df, f = aligned_df$event_id)

    df_list_speakvar <- lapply(df_list, function(df){
      svec <- unique(as.character(df$speaker_names_raw))
      df <- df %>%
        mutate(speaker_var = ifelse(as.character(speaker_names_raw) == svec[1], "S1", "S2"),
               speaker_pair = paste(sort(svec), collapse = "---"))
    })

    df_speakvar <- bind_rows(df_list_speakvar)

    # pivot wider by speaker code
    df_wide <- df_speakvar %>%
      group_by(event_id, exchangecount, speaker_var) %>%
      dplyr::summarize(across(contains(align_var), mean),
                       #speaker_var = first(speaker_var),
                       speaker_pair = first(speaker_pair),
                       .groups = "drop") %>%
      pivot_wider(names_from = c("speaker_var"), values_from = any_of(align_dimensions))

    # wrangle
    speaker1 <- df_wide %>% dplyr::select(c("event_id", "exchangecount", "speaker_pair") | c(ends_with("_S1"))) %>% drop_na()
    speaker2 <- df_wide %>% dplyr::select(c("event_id", "exchangecount") | c(ends_with("_S2"))) %>% drop_na()
    widedf <- merge(speaker1, speaker2, by=c("event_id", "exchangecount")) %>%
      arrange(event_id, exchangecount)

    #iterate over each aligned dimension, selecting only the scores for that dimension and pulling a difference value and subbing it in for the actual values
    for (dimension in align_var){
      both_speaker_cols <- widedf %>% select(starts_with(dimension))
      absdiffcol <- data.frame(dimension = abs(both_speaker_cols[,1] - both_speaker_cols[,2]))
      widedf[which(colnames(widedf) %in% paste(dimension, c("S1", "S2"), sep = "_"))] <- absdiffcol
    }

    longdf <- widedf %>%
      pivot_longer(cols = c(ends_with("_S1") | ends_with("_S2")), names_to = c("dimension", "speaker_names_raw"), names_pattern = "(.*)_([^_]+)$", values_to = "score") %>%
      pivot_wider(names_from = dimension, values_from = score) %>%
      filter(speaker_names_raw == "S1") %>%
      select(-speaker_names_raw)
    return(longdf)
  }
  #DEFINE ABSOLUTE DIFFERENCE TIME SERIES FUNCTION:


  #END DEFINE ABOSOLUTE DIFFERENE TIME SERIES FUNCTION

  #DEFINE time SERIES RESCALER
  resample_time_series <- function(df_list, resample_n = "min") {

    align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                          "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                          "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                          "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                          "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                          "sem_concreteness", "sem_diversity", "sem_neighbors")
    # if the data is not in a list, then it splits by event_id
    if (typeof(df_list) != "list"){
      df_list <- df_list %>% droplevels()
      df_list = split(df_list, f = df_list$event_id)
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
        dplyr::select(contains(align_dimensions)) %>% #select emotion columns
        dplyr::mutate(exchangecount = seq(from = 0, to = (nrow(x)-1))) #add exchange count

      #if the ts has 3 or fewer exchange counts it shouldn't  be resampled, so will report NA
      if (max(ts_select$exchangecount) <= 2){
        final_ts <- ts_select %>%
          mutate(event_id = unique(x$event_id),
                 speaker_pair = unique(x$speaker_pair))
        #data.frame(exchangecount = seq(from = 0, to = nrow(ts_select) - 1),
        #event_id = unique(x$event_id),
        #speaker_pair = unique(x$speaker_pair))
        final_ts[,colnames(final_ts) %in% align_dimensions] <- NA
        final_ts
      }
      else{
        #computes how many points must be calculated from each currently present point
        scale_ratio <- resample_n / max(ts_select$exchangecount)

        #if the transcript is the desired length already
        if (scale_ratio == 1) {
          rescale_final <- ts_select %>% #assign to var so that it will be smoothed
            dplyr::select(!exchangecount) #remove exchange count
        }
        else if (scale_ratio > 1) {
          #test if ratio is an integer
          scale_ratio_floor <- floor(scale_ratio)
          #moves the intervals with values on the new scale
          ts_select$exchangecount <- ts_select$exchangecount * scale_ratio_floor
          #creates a new frame that counts to the new scale
          newdf <- data.frame(exchangecount = 0:max(ts_select$exchangecount))
          interpol_integer <- newdf %>%
            dplyr::left_join(ts_select, by=("exchangecount")) %>%
            dplyr::select(!exchangecount)#bind measured values to the sequence

          interpol_integer[,1:ncol(interpol_integer)] <- lapply(interpol_integer[,1:ncol(interpol_integer)], function(col_select){
            col_select <- zoo::na.approx(col_select)
          })

          if (nrow(interpol_integer) == resample_n +1) {
            rescale_final <- interpol_integer
          }
          else {
            #compute a decimal to select n number of points to fill
            point_diff <- resample_n - (nrow(interpol_integer) - 1)
            #creates a new factor variable to split the data frame - rounds down to evenly separate rows into n group
            splitter <- floor(nrow(interpol_integer) / point_diff)
            #binds a sequence to the data frame that sections rows into groups by splitter
            counter <- rep(1:(nrow(interpol_integer)/splitter), each = splitter)
            #if counter does not span the whole data frame it fills in the rest to prevent bad groupings
            if (length(counter) < nrow(interpol_integer)) {
              counter[(length(counter)+1):nrow(interpol_integer)] <- max(counter) + 1
            }
            interpol_new <- cbind(Count = counter, interpol_integer)
            #splits the data frame into a list by the sequence, each group size is specified by splitter
            interpol_list <- split(interpol_new, f = interpol_new$Count)

            #for each data frame in the list - add a row of NA if the sequence count is less that the difference between 150 and the current Exchange Count. If not, leave it alone.
            interpol_list_added <- lapply(interpol_list, function(y){
              y <- data.frame(y)
              if (first(y$Count) <= point_diff) {
                y[nrow(y)+1,] <- NA
                y
              }
              else {
                y
              }
            })
            interpol_added <- bind_rows(interpol_list_added) #binds the data frame list together

            #if last row is NA, interpolation will not work, so it switches last two rows
            if (is.na(tail(interpol_added[,2], 1)) == TRUE) {
              interpol_added[nrow(interpol_added),] <- interpol_added[nrow(interpol_added)-1,]
              interpol_added[nrow(interpol_added)-1,]  <- NA
            }
            rescale_final <- interpol_added %>%
              select(!Count) #remove grouping variable
            #interpolate over NAs in the time series
            rescale_final[,1:ncol(rescale_final)] <- lapply(rescale_final[,1:ncol(rescale_final)],
                                                            function(col_select){
                                                              col_select <- zoo::na.approx(col_select)
                                                            })
          }
        }
        else if (scale_ratio < 1) {
          grouper_var <- 1 / scale_ratio #take inverse of ratio to find size of groups
          grouper_var_floor <- floor(grouper_var) #round group size to nearest integer
          #creates a sequence to group time counts, and adds up to the max time if the count is not even.
          grouper_counter <- rep(seq(1:(nrow(ts_select)/grouper_var_floor)), each=grouper_var_floor)
          if (length(grouper_counter) < nrow(ts_select)) {
            grouper_counter[(length(grouper_counter)+1):nrow(ts_select)] <- max(grouper_counter) + 1
          }

          agg_int <- ts_select %>%
            mutate(Grouper = grouper_counter) %>%
            group_by(Grouper) %>% #create and group by the integer needed to downscale
            summarise(across(contains(colnames(x)), mean), #aggregate measurements by group
                      .groups = "drop") %>%
            select(!Grouper)

          if (nrow(agg_int) == resample_n + 1) {
            rescale_final <- agg_int
            rescale_final
          }
          else {
            #compute the number of points to remove after aggregating by lowest integer ratio
            point_diff <- (nrow(agg_int) - 1) - resample_n
            #creates a new factor variable to distribute where aggregation occurs
            splitter <- (nrow(agg_int) - 1) / point_diff
            #evenly split indexes for aggregation across the time series
            index_subs <-  floor(seq(from = splitter, to = nrow(agg_int), by = splitter))
            #generate a grouping sequence and substitute at indexes to create groups of two that will be aggregated
            second_grouper <- seq(1:nrow(agg_int))
            second_grouper[index_subs - 1] <- index_subs

            agg_final <- agg_int %>%
              dplyr::mutate(Grouper = second_grouper) %>%
              dplyr::group_by(Grouper) %>% #create and group by the integer needed to downscale
              dplyr::summarise(dplyr::across(contains(colnames(x)), mean), #aggregate measurements by group
                               .groups = "drop") %>%
              dplyr::select(!Grouper)
            rescale_final <- agg_final
          }
        } #end of down scaling section

        final_ts <- rescale_final %>%
          mutate(exchangecount = seq(from = 0, to = resample_n), #create exchange count column
                 event_id = unique(x$event_id),
                 speaker_pair = unique(x$speaker_pair))
        #bind all new columns together into one data frame with an exchange count
        final_ts <- data.frame(final_ts)
      }
    })
    #returns a list of scaled and smoothed data frames
    return(dflistscaled)
  }
  #END DEFINE time SERIES RESCALER

  #DEFINE FIND DYAD AREA UNDER THE CURVE FUNCTION
  find_auc_dyads <- function(aligned_ts_df, resample_yes_or_no = TRUE, resample_n = "min") {
    #run the difference time series function on the aligned dataframe
    computed_df <- find_diff_ts(aligned_df = aligned_ts_df) #convert to difference time series
    computed_df <- computed_df %>% droplevels()
    computed_df_list <- split(computed_df, f = computed_df$event_id)
    #conditionally resample every time series based on the given argument
    if (resample_yes_or_no == TRUE) {
      #check if argument for resampling is set to min:
      if (resample_n == "min"){
        #mutate the max exchangecount for each dyad and then select only that variable
        min_exc_max <- computed_df %>%
          dplyr::group_by(event_id) %>%
          dplyr::mutate(exc_max = max(exchangecount)) %>%
          dplyr::summarize(exc_max = first(exc_max), .groups = "drop") %>%
          dplyr::select(exc_max)

        #take the minimum of the max exchange count by dyad variable after converting to a vector
        min_exc <- min(min_exc_max$exc_max)
        #set this new min exchangecount to resample n
        resample_n <- min_exc
      }
      #if resample n is not min default string or an integer (number, not type) throw an error
      else if (is.numeric(resample_n) == FALSE || resample_n %% 1 != 0) {
        stop("resample_n argument must be default 'min' string value or an integer")
      }
      computed_df_list <- resample_time_series(df_list = computed_df_list, resample_n = resample_n)
    }
    else if (resample_yes_or_no == FALSE) {
      #if no resampling is desired, make exc start from zero
      # computed_df_list <- lapply(computed_df_list, function(df_select){
      #   df_select$exchangecount <-  seq(from = 0, to = nrow(df_select)-1)
      #   df_select <- data.frame(df_select)
      #})
      #^^^ should check if that is necessary ^^^
      resample_n <- 3 #if not resampling sets the var to 3 so that dyads with less than that will get NA
    }
    else {
      stop("Argument resample_yes_or_no must be logical")
    }

    #create a separate df of id variables
    id_cols <- dplyr::bind_rows(lapply(computed_df_list, function(df){
      unique(df[,c("event_id", "speaker_pair")])
    }))
    align_dimensions <- c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                          "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness", "aff_stress",
                          "aff_surprise", "aff_trust", "aff_valence", "lex_age_acquisition",
                          "lex_letter_count_raw", "lex_morphemecount_raw", "lex_prevalence",
                          "lex_senses_polysemy", "lex_wordfreqlg10_raw", "sem_arousal",
                          "sem_concreteness", "sem_diversity", "sem_neighbors")


    xdimensions <- colnames(computed_df_list[[1]][,colnames(computed_df_list[[1]]) %in% align_dimensions])

    domain_auc_list <- lapply(xdimensions, function(dimension){ #iterate over emotion

      single_doc_auc <- lapply(computed_df_list, function(aligned_ts_df){ #iterate over each document
        domain_ts <- aligned_ts_df %>%
          dplyr::select(exchangecount, contains(dimension)) #select only desired emotion and time count
        # conditionally skip AUC calculation for a dyad if it has fewer exchangecounts than what was sampled to. If resampling is turned off it will set to fill those with less than 3 exchangecounts (done by setting n = 3)
        if (max(domain_ts$exchangecount) < resample_n){
          #create a single row, single column dataframe with one empty value to fill in the AUC
          doc_domain_auc_df <- data.frame(domain_auc = NA)
          doc_domain_auc_df
        }
        else {
          #domain_ts <- data.frame(domain_ts) #make single emotion time series a data frame
          domain_auc <- DescTools::AUC(x = domain_ts[,1], y = domain_ts[,2]) #take AUC of time series
          doc_domain_auc_df <- data.frame(domain_auc) #make data frame of AUC, replicated once
        }
      })
      all_doc_domain_auc_df <- bind_rows(single_doc_auc) #bind all docs AUCs for emotion into one column
      colnames(all_doc_domain_auc_df) <- paste("auc", dimension, sep = "_") # add auc as colname prefix
      all_doc_domain_auc_df

    })

    all_domain_df <- bind_cols(domain_auc_list, id_cols) #bind all columns of AUCs into one data frame
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

    df_list <- split(aligned_ts_df, f = aligned_ts_df$event_id)
    #iterate over each newly split data frame.
    output_df_list <- lapply(df_list, function(df){
      #establish raw speaker name and S1/S2 'key' - going to do this alphabetically for now
      speakervec <- sort(unique(df$speaker_names_raw))
      #substitute speaker names with transient variable to be replaced with real names later
      names(speakervec) <- c("S1", "S2")
      df$speaker_names_raw <- gsub(speakervec[1], names(speakervec)[1], df$speaker_names_raw)
      df$speaker_names_raw <- gsub(speakervec[2], names(speakervec)[2], df$speaker_names_raw)

      df_wide <- df %>%
        group_by(event_id, exchangecount, speaker_names_raw) %>%
        dplyr::summarize(across(contains(align_var), mean),
                         .groups = "drop") %>%
        tidyr::pivot_wider(names_from = c(speaker_names_raw),
                           values_from = align_var) %>%
        dplyr::select(event_id, exchangecount, contains(align_var))
      #dplyr::select(-c(starts_with("m_"), starts_with("analytics_")))

      # wrangle - remove NA rows produced from pivoting and then merge into real wide df (one exc per row)
      speaker1 <- df_wide %>% dplyr::select(c('event_id', 'exchangecount') | c(ends_with("_S1"))) %>% drop_na()
      speaker2 <- df_wide %>% dplyr::select(c('event_id', 'exchangecount') | c(ends_with("_S2"))) %>% drop_na()
      all <- merge(speaker1, speaker2, by=c('event_id', 'exchangecount'))
      all <- all %>% dplyr::arrange(event_id, exchangecount)
      # conditionally pass over scorr calculation and instead fill with NA if time series has 3 or less time points
      if (nrow(all) <= 3) {
        dyad_sc <- all %>%
          pivot_longer(cols = c(ends_with("S1") | ends_with("S2")),
                       names_to = c("dimension", "speaker_names_raw"),
                       names_pattern = "(.*)_([^_]+)$",
                       values_to = "score") %>%
          mutate(score = NA,
                 speaker_names_raw = replace(speaker_names_raw,
                                             which(speaker_names_raw == "S1"), as.character(speakervec)[1]),
                 speaker_names_raw = replace(speaker_names_raw,
                                             which(speaker_names_raw == "S2"),
                                             as.character(speakervec)[2]),
                 speaker_names_raw = as.factor(speaker_names_raw)) %>%
          pivot_wider(names_from = dimension, values_from = score)
        #create two transient variables from the align dimension

        rho_cols <- dyad_sc %>% select(contains(align_var))
        pval_cols <- rho_cols

        rho_cols <- rho_cols %>%
          rename_with(~str_c("S_rho_", .), .cols = everything())
        #mutate(across(contains(align_var), dplyr::na_if(., is.numeric(.)), .names = "S_rho_{.col}")
        pval_cols <- pval_cols %>%
          rename_with(~str_c("S_pval_", .), .cols = everything())
        #mutate(across(contains(align_var), ifelse(is.na(align_var) == F, NA, NA)), .names = "S_pval_{.col}")
        dyad_sc <- dyad_sc %>%
          select(-c(exchangecount, contains(align_var)))
        dyad_sc <- bind_cols(list(dyad_sc, rho_cols, pval_cols)) #bind rho and pval cols together
        dyad_sc
      }
      else {
        #convert all dimension columns to numeric
        all[,colnames(all) %in% align_var] <- as.numeric(all[,colnames(all) %in% align_var])
        x_vars <- all %>% select(c('event_id') | c(ends_with("_S1"))) %>%
          rename_at(vars(matches("_S1")), ~str_replace(., "_S1", ""))
        y_vars <- all %>% select(c('event_id') | c(ends_with("_S2"))) %>%
          rename_at(vars(matches("_S2")), ~str_replace(., "_S2", ""))

        #Separate lapply for spearmans correlation
        dyad_dim_sc_list <- lapply(align_var, function(dim){
          dim_x_vars <- x_vars[,colnames(x_vars) %in% dim]
          dim_y_vars <- y_vars[,colnames(y_vars) %in% dim]
          #run spearman corr and format rho and p value into a data frame
          sc_results <- cor.test(dim_x_vars, dim_y_vars, method = "spearman", exact = F)
          sc_results_df <- data.frame(S_rho = rep(sc_results$estimate, 2), S_pval = rep(sc_results$p.value, 2))
          #add speaker column only if it is the first iteration
          colnames(sc_results_df) <- paste(colnames(sc_results_df), dim, sep = "_")
          if (match(dim, align_var) == 1) {
            #sc_results_df$speaker_names_raw  <- c("S1", "S2")
            sc_results_df$speaker_names_raw <- speakervec
          }
          sc_results_df
        })

        #bind and add event id to spearman correlation df
        dyad_sc <- bind_cols(dyad_dim_sc_list, event_id = unique(df$event_id))
      }
      dyad_sc
    })
    all_dyad_df <- bind_rows(output_df_list)
    return(all_dyad_df)
  }
  #END DEFINE SPEARMAN'S CORRELATION FUNCTION

  #run each constituent summarize function
  main_effect_df <- find_main_effect_dyads(aligned_ts_df = aligned_ts_df,
                                           aggregate_the_data = aggregate_the_data)

  auc_df <- find_auc_dyads(aligned_ts_df = aligned_ts_df,
                           resample_yes_or_no = resample_yes_or_no,
                           resample_n = resample_n)

  scorr_df <- spearmans_corr_dyads(aligned_ts_df = aligned_ts_df)
  #manually left join each summarize data frame together by speaker pair and transcript
  output_df <- dplyr::left_join(main_effect_df, auc_df, by=c("event_id", "speaker_pair"))
  output_df <- dplyr::left_join(output_df, scorr_df, by = c("event_id", "speaker_names_raw"))
  return(output_df)
}


