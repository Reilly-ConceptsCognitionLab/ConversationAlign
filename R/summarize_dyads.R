#' summarize_dyads
#'
#' appends AUC and Spearman Rank Correlation indices to each dyad (event_id) using a resampling algoirthm that defaults to the minimum number of exchanges across all documents entered
#'
#' @name summarize_dyads
#' @param dataframe produced in the align_dyads function
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
    dplyr::group_by(event_id) %>%
    dplyr::summarize(p_count = length(unique(Participant_ID)),
                     .groups = "drop") #generate dataframe with number of interlocutors in each transcript

  err_ts_vec <- check_pnum$event_id[which(check_pnum$p_count != 2)]
  #check if any transcripts were identified and throw an error with the specific transcripts
  if (length(err_ts_vec) != 0) {
    stop("summarize_dyads requires that all transcripts have exactly two interlocutors.\ntranscripts with less than or greater than 2 interlocutors:\n",
         paste(err_ts_vec, collapse = "\n"))
  }

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

    #identify the variables from the psycholiguistic database that users have aligned on
    align_var <- colnames(aligned_ts_df)[which(colnames(aligned_ts_df) %in% align_dimensions)]

    #mutates new mean columns for each aligned variable, adds a interlocutor pair variable, and preserves all columns
    df_averaged_long <- aligned_ts_df %>%
      dplyr::group_by(event_id) %>% #add a column of concatenated interlocutor names by transcript
      dplyr::mutate(participant_pair = paste(sort(unique(Participant_ID)), collapse = "---")) %>%
      dplyr::group_by(turncount, .add = TRUE) %>% #group by interlocutor and dyad
      dplyr::mutate(dplyr::across(tidyselect::starts_with(align_var), mean, .names = "mean_{col}")) %>%
      dplyr::ungroup()
    return(df_averaged_long)

  }
  #END DEFINE MAIN EFFECT FUNCTION

  #DEFINE DIFFERENCE TIME SERIES FUNCTION
  find_diff_ts <- function(aligned_df = aligned_ts_df){
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
      svec <- unique(as.character(df$Participant_ID))
      df <- df %>%
        dplyr::mutate(participant_var = ifelse(as.character(Participant_ID) == svec[1], "S1", "S2"),
                      participant_pair = paste(sort(svec), collapse = "---"))
    })

    df_speakvar <- dplyr::bind_rows(df_list_speakvar)

    df_wide <- df_speakvar %>%
      dplyr::group_by(event_id, exchangecount, Participant_ID) %>%
      dplyr::summarise(dplyr::across(contains(align_var), first),
                       participant_var = first(participant_var),
                       participant_pair = first(participant_pair),
                       .groups = "drop") %>%
      tidyr::pivot_wider(names_from = c("participant_var"), values_from = any_of(align_dimensions))

    # wrangle into two seperate dataframes by interlocutor
    participant1 <- df_wide %>% dplyr::select(c("event_id", "exchangecount", "participant_pair") | c(ends_with("_S1"))) %>% drop_na()
    participant2 <- df_wide %>% dplyr::select(c("event_id", "exchangecount") | c(ends_with("_S2"))) %>% drop_na()
    widedf <- merge(participant1, participant2, by=c("event_id", "exchangecount")) %>%
      dplyr::arrange(event_id, exchangecount)

    #iterate over each aligned dimension, selecting only the scores for that dimension and pulling a difference value and subbing it in for the actual values
    for (dimension in align_var){
      both_participant_cols <- widedf %>% select(starts_with(dimension))
      absdiffcol <- data.frame(dimension = abs(both_participant_cols[,1] - both_participant_cols[,2]))
      widedf[which(colnames(widedf) %in% paste(dimension, c("S1", "S2"), sep = "_"))] <- absdiffcol
    }

    longdf <- widedf %>%
      tidyr::pivot_longer(cols = c(ends_with("_S1") | ends_with("_S2")), names_to = c("dimension", "Participant_ID"), names_pattern = "(.*)_([^_]+)$", values_to = "score") %>%
      tidyr::pivot_wider(names_from = dimension, values_from = score) %>%
      dplyr::filter(Participant_ID == "S1") %>%
      dplyr::select(-Participant_ID)
    return(longdf)
  }
  #END DEFINE ABOSOLUTE DIFFERENE TIME SERIES FUNCTION

  #DEFINE time SERIES RESCALER
  resample_time_series <- function(df_list, threshold) {

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
          dplyr::mutate(event_id = unique(x$event_id),
                        participant_pair = unique(x$participant_pair))

        final_ts[,colnames(final_ts) %in% align_dimensions] <- NA
        final_ts
      }
      else{
        #computes how many points must be calculated from each currently present point
        scale_ratio <- threshold / max(ts_select$exchangecount)

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
            dplyr::select(!exchangecount) #bind measured values to the sequence

          interpol_integer[,1:ncol(interpol_integer)] <- lapply(interpol_integer[,1:ncol(interpol_integer)], function(col_select){
            col_select <- zoo::na.approx(col_select)
          })

          if (nrow(interpol_integer) == threshold +1) {
            rescale_final <- interpol_integer
          }
          else {
            #compute a decimal to select n number of points to fill
            point_diff <- threshold - (nrow(interpol_integer) - 1)
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
            interpol_added <- dplyr::bind_rows(interpol_list_added) #binds the data frame list together

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
            dplyr::mutate(Grouper = grouper_counter) %>%
            dplyr::group_by(Grouper) %>% #create and group by the integer needed to downscale
            dplyr::summarise(dplyr::across(contains(colnames(x)), mean), #aggregate measurements by group
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
          dplyr::mutate(exchangecount = seq(from = 0, to = threshold), #create exchange count column
                        event_id = unique(x$event_id),
                        participant_pair = unique(x$participant_pair))
        #bind all new columns together into one data frame with an exchange count
        final_ts <- data.frame(final_ts)
      }
    })
    #returns a list of scaled and smoothed data frames
    return(dflistscaled)
  }
  #END DEFINE time SERIES RESCALER

  #DEFINE FIND DYAD AREA UNDER THE CURVE FUNCTION
  find_auc_dyads <- function(aligned_ts_df, resample = TRUE) {
    #run the difference time series function on the aligned dataframe
    computed_df <- find_diff_ts(aligned_df = aligned_ts_df) #convert to difference time series
    computed_df <- computed_df %>% droplevels()
    computed_df_list <- split(computed_df, f = computed_df$event_id)
    #conditionally resample every time series based on the given argument
    if (is.logical(resample) == TRUE) {

      #mutate the max exchangecount for each dyad
      min_exc_max <- computed_df %>%
        dplyr::group_by(event_id) %>%
        dplyr::mutate(exc_max = max(exchangecount)) %>%
        dplyr::summarise(exc_max = first(exc_max),
                         .groups = "drop")
      #take the minimum of the max exchange count by dyad variable after converting to a vector
      min_exc <- min(min_exc_max$exc_max)
      #set this new min exchangecount to resample n
      threshold <- min_exc

      if (resample == TRUE){
        if (threshold < 20) {
          warning(writeLines("the threshold for resampling is below 20 exchanges.\narea under the curve and spearman's rank correlation become less valid measures of alignment below 20 exchanges"))
        }


        computed_df_list <- resample_time_series(df_list = computed_df_list, threshold = threshold)
      }
      else {
        small_dyads <- unique(min_exc_max$event_id[which(min_exc_max$exc_max < 20)])
        warning(writeLines(paste("The following dyads are shorter than 20 exchange counts.\nMeasures of area under the curve and spearman's rank correlation become less valid below 20 exchanges", paste(small_dyads, collapse = "\n"), sep = "\n")))

        threshold <- 3
      }
    }
    else {
      stop("Argument resample must be logical")
    }

    #create a separate df of id variables
    id_cols <- dplyr::bind_rows(lapply(computed_df_list, function(df){
      unique(df[,c("event_id", "participant_pair")])
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
        if (max(domain_ts$exchangecount) < threshold){
          #create a single row, single column dataframe with one empty value to fill in the AUC
          doc_domain_auc_df <- data.frame(domain_auc = NA)
          doc_domain_auc_df
        }
        else {
          domain_ts <- data.frame(domain_ts) #make single emotion time series a data frame
          domain_auc <- DescTools::AUC(x = domain_ts[,1], y = domain_ts[,2]) #take AUC of time series
          doc_domain_auc_df <- data.frame(domain_auc) #make data frame of AUC, replicated once
        }
      })
      all_doc_domain_auc_df <- dplyr::bind_rows(single_doc_auc) #bind all docs AUCs for emotion into one column
      colnames(all_doc_domain_auc_df) <- paste("auc", dimension, sep = "_") # add auc as colname prefix
      all_doc_domain_auc_df

    })

    all_domain_df <- dplyr::bind_cols(domain_auc_list, id_cols) #bind all columns of AUCs into one data frame
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
      #establish raw participant name and S1/S2 'key' - going to do this alphabetically for now
      participantvec <- sort(unique(df$Participant_ID))
      #substitute participant names with transient variable to be replaced with real names later
      names(participantvec) <- c("S1", "S2")
      df$Participant_ID <- gsub(participantvec[1], names(participantvec)[1], df$Participant_ID)
      df$Participant_ID <- gsub(participantvec[2], names(participantvec)[2], df$Participant_ID)

      #create a wide data frame with each row containing both participants' turn aggregated scores
      df_wide <- df %>%
        dplyr::group_by(event_id, exchangecount, Participant_ID, .add = FALSE) %>%
        dplyr::summarise(dplyr::across(contains(align_var), mean),
                         .group = "drop") %>%
        tidyr::pivot_wider(names_from = tidyselect::contains("Participant_ID"),
                           values_from = align_var) %>%
        dplyr::select(event_id, exchangecount, contains(align_var))

      # wrangle - remove NA rows produced from pivoting and then merge into real wide df (one exc per row)
      participant1 <- df_wide %>% dplyr::select(c('event_id', 'exchangecount') | c(ends_with("_S1"))) %>% tidyr::drop_na()
      participant2 <- df_wide %>% dplyr::select(c('event_id', 'exchangecount') | c(ends_with("_S2"))) %>% tidyr::drop_na()
      all <- merge(participant1, participant2, by=c('event_id', 'exchangecount'))
      all <- all %>% dplyr::arrange(event_id, exchangecount)
      # conditionally pass over scorr calculation and instead fill with NA if time series has 3 or less time points
      if (nrow(all) <= 3) {
        dyad_sc <- all %>%
          tidyr::pivot_longer(cols = c(ends_with("S1") | ends_with("S2")),
                              names_to = c("dimension", "Participant_ID"),
                              names_pattern = "(.*)_([^_]+)$",
                              values_to = "score") %>%
          dplyr::mutate(score = NA,
                        Participant_ID = replace(Participant_ID,
                                                 which(Participant_ID == "S1"), as.character(participantvec)[1]),
                        Participant_ID = replace(Participant_ID,
                                                 which(Participant_ID == "S2"),
                                                 as.character(participantvec)[2]),
                        Participant_ID = as.factor(Participant_ID)) %>%
          tidyr::pivot_wider(names_from = dimension, values_from = score)
        #create two transient variables from the align dimension

        rho_cols <- dyad_sc %>% dplyr::select(contains(align_var))
        #add column prefixes to the rho columns
        rho_cols <- rho_cols %>%
          dplyr::rename_with(~stringr::str_c("S_rho_", .), .cols = tidyselect::everything())
        #bind the empty rho columns to dyad identifiers so that it can be bound to to total data frame.
        dyad_sc <- dyad_sc %>%
          select(-c(exchangecount, contains(align_var)))
        dyad_sc <- dplyr::bind_cols(list(dyad_sc, rho_cols)) #bind rho and pval cols together
        dyad_sc
      }
      else {
        #convert all dimension columns to numeric and remove speaker suffixes
        all[,colnames(all) %in% align_var] <- as.numeric(all[,colnames(all) %in% align_var])
        x_vars <- all %>% dplyr::select(c('event_id') | c(ends_with("_S1"))) %>%
          dplyr::rename_at(dplyr::vars(matches("_S1")), ~stringr::str_replace(., "_S1", ""))
        y_vars <- all %>% select(c('event_id') | c(ends_with("_S2"))) %>%
          dplyr::rename_at(dplyr::vars(matches("_S2")), ~stringr::str_replace(., "_S2", ""))

        #Separate lapply for spearmans correlation
        dyad_dim_sc_list <- lapply(align_var, function(dim){
          dim_x_vars <- x_vars[,colnames(x_vars) %in% dim]
          dim_y_vars <- y_vars[,colnames(y_vars) %in% dim]
          #run spearman corr and format rho and p value into a data frame
          sc_results <- cor.test(dim_x_vars, dim_y_vars, method = "spearman", exact = F)
          sc_results_df <- data.frame(S_rho = rep(sc_results$estimate, 2))
          #S_pval = rep(sc_results$p.value, 2)) - removing this
          #add participant column only if it is the first iteration
          colnames(sc_results_df) <- paste(colnames(sc_results_df), dim, sep = "_")
          if (match(dim, align_var) == 1) {
            sc_results_df$Participant_ID <- participantvec
          }
          sc_results_df
        })

        #bind and add event id to spearman correlation df
        dyad_sc <- dplyr::bind_cols(dyad_dim_sc_list, event_id = unique(df$event_id))
      }
      dyad_sc
    })
    all_dyad_df <- dplyr::bind_rows(output_df_list)
    return(all_dyad_df)
  }
  #END DEFINE SPEARMAN'S CORRELATION FUNCTION

  #run each constituent summarize function
  main_effect_df <- find_main_effect_dyads(aligned_ts_df = aligned_ts_df,
                                           aggregate_the_data = aggregate_the_data)
  auc_df <- find_auc_dyads(aligned_ts_df = aligned_ts_df,
                           resample = resample)

  scorr_df <- spearmans_corr_dyads(aligned_ts_df = aligned_ts_df)
  #manually left join each summarize data frame together by participant pair and transcript
  output_df <- dplyr::left_join(main_effect_df, auc_df, by=c("event_id", "participant_pair"))
  output_df <- dplyr::left_join(output_df, scorr_df, by = c("event_id", "Participant_ID"))
  return(output_df)
}



