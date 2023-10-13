#' align_dyads
#'
#' Yokes user-specified semantic, affective, and phonological values to each word in a cleaned language transcript. Prepares a dataframe aligned by exchange and turn across speakers.
#'
#' @name align_dyads
#' @param clean_ts_df cleaned and formatted dataframe ported from the clean_dyads() step
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom tidytable consecutive_id
#' @importFrom dplyr bind_rows
#' @importFrom tidyverse group_by
#' @importFrom tidyverse summarise
#' @importFrom tidyverse summarise(across())
#' @importFrom tidyselect any_of
#' @importFrom stringr str_split_1
#' @importFrom stringr str_squish
#' @export align_dyads

align_dyads <- function(clean_ts_df) {
  print("this is a package update test")
  #allow the user to select what variables they want to align, or provide their own database(s) and subset them
  myvars <- select.list(c("aff_anger", "aff_anxiety", "aff_boredom",  "aff_closeness",
                          "aff_confusion", "aff_dominance", "aff_doubt", "aff_empathy",
                          "aff_encouragement", "aff_excitement", "aff_guilt", "aff_happiness",
                          "aff_hope", "aff_hostility", "aff_politeness", "aff_sadness",
                          "aff_stress", "aff_surprise", "aff_trust", "aff_valence",
                          "lex_age_acquisition", "lex_letter_count_raw",
                          "lex_morphemecount_raw", "lex_prevalence", "lex_senses_polysemy",
                          "lex_wordfreqlg10_raw", "sem_arousal", "sem_concreteness",
                          "sem_diversity", "sem_neighbors"), preselect = NULL, multiple = TRUE,
                        title = writeLines("Select the variables you would like to align your conversation transcripts on.\nPlease do not select more than three variables."),
                        graphics = FALSE)
  var_selected <- lookup_db %>% #select desired columns from lookup_db
    select(matches("^word$"), contains(myvars))
  #create variable containing the column names of each variable to be aligned
  var_aligners <- colnames(var_selected)[-grep("^word$", colnames(lookup_db), ignore.case = TRUE)]

  ts_list <- split(clean_ts_df, f = clean_ts_df$event_id) #split transcript df into list by event_id
  ts_aligned_list <- lapply(ts_list, function(ts_select){
    #join measures of each variable to each word in each transcript

    df_aligned <- dplyr::left_join(ts_select, var_selected, by = c("cleantext" = "word"), multiple = "first")

    df_aligned <- data.frame(df_aligned)
    df_aligned <- df_aligned[complete.cases(df_aligned[, c(which(colnames(df_aligned) %in% myvars))]),]     # remove rows with words that couldn't be aligned


    df_aligned_an <- df_aligned %>%
      group_by(event_id, speaker_names_raw) %>%
      mutate(an_wordcount_align = length(str_squish(str_split_1(paste(cleantext, collapse = " "), " "))),
             an_mean_word_length_align = mean(nchar(str_squish(str_split_1(paste(cleantext, collapse = " "), " ")))),
             an_word_removed_align = an_wordcount_clean - an_wordcount_align) %>%
      ungroup()

    #group on event id and add a turn count column that sequences each uninterrupted utterance
    df_aligned_turn <- df_aligned_an %>%
      dplyr::group_by(event_id) %>%
      dplyr::mutate(turncount = dplyr::consecutive_id(speaker_names_raw), .before = 1)

    #add an exchange count variable, which is one turn from each speaker
    df_aligned_turn$exchangecount <- ceiling(df_aligned_turn$turncount / 2)
    #rearrange the columns to be more readable
    df_aligned_ec <- df_aligned_turn %>%
      select(tidyselect::any_of(c("event_id", "speaker_names_raw", "exchangecount", "turncount", "cleantext", "time")),
             contains(var_aligners), everything())
    df_aligned_ec
  })
  ts_aligned_df_total <- bind_rows(ts_aligned_list)

  #DEFINE THE METADATA ALIGN FUNCTION
  align_metadata <- function(aligned_ts_df) {
    #allow user to input the file path to demographic data, randomly assign groups, or not align groups
    ask_meta_filepath <- readline(writeLines("If you would like to align metadata by speaker and event ID, input the file path to the metadata csv file.\nIf you do not wish to align metadata do not enter anything.\nEnter 'random' to randomly assign a variable to each speaker in each dyad."))
    #if user inputs 'random', randomly assigns groups across transcripts
    if (str_to_lower(ask_meta_filepath) == "random") {
      randomly <- lapply(split(aligned_ts_df, aligned_ts_df$event_id), function(x){ #iterates over each doc
        x <- data.frame(x)
        #creates vector of each speaker with random indexes and assigns a alphanumeric sequence name
        speakervec <- sample(unique(x[,grep("speaker_names_raw", colnames(x), ignore.case = T)]))
        names(speakervec) <- paste("S", 1:length(speakervec), sep = "")
        #creates a data frame with just speaker names and assigned code
        coloutput <- data.frame(speaker_names_raw = speakervec,
                                speaker_group_var_random = sapply(speakervec, function(y) {
                                  names(speakervec)[match(y, speakervec)]}))
        x <- x %>% left_join(coloutput, by=c("speaker_names_raw")) #binds code to aligned data frame
      })
      randomly <- bind_rows(randomly) #binds all the doc data frame into one
      return(randomly)
    }
    #if input is empty, returns the aligned data frame with no demographics
    else if (ask_meta_filepath == "") {
      return(aligned_ts_df)
    }
    #if file path is entered:
    else {
      #reads in a csv file of demographic information associated with participant IDs.
      metadata <- data.frame(read.csv(ask_meta_filepath))
      #allows the user to specify which columns they want to subset
      subset_metadata <- select.list(c(colnames(metadata), "Select all columns"),
                                     preselect = NULL, multiple = TRUE,
                                     title = "Select the columns you would like to subset. The participant ID and event/transcript ID column must be included.",
                                     graphics = FALSE)
      #if the select all option is chosen, selects every column
      if (any(grepl("Select all columns", subset_metadata)) == TRUE) {
        subset_metadata <- colnames(metadata)
      }
      metadata_selected <- metadata[,colnames(metadata) %in% subset_metadata] #select specified columns
      colnames(metadata_selected) <- tolower(colnames(metadata_selected))
      #select dimensions that aren't used to align on
      meta_dims <- which(!colnames(metadata_selected) %in% c("event_id", "speaker"))
      #make all dimensions that aren't alingers factors
      metadata_selected[,meta_dims] <- lapply(metadata_selected[,meta_dims], factor)
      colnames(metadata_selected)[meta_dims] <- paste("m", tolower(colnames(metadata_selected)[meta_dims]), sep = "_")

      #join metadata to aligned data frame by event id and PID
      metadata_aligned_df <- dplyr::left_join(aligned_ts_df, metadata_selected,
                                              by=c("event_id", "speaker_names_raw" = "speaker"))
      return(metadata_aligned_df)
    }
  }
  #END DEFINING METADATA ALIGN FUNCTION
  output <- align_metadata(aligned_ts_df = ts_aligned_df_total) #run demoraphic aligner on aligned data frame
  if (is.factor(output$speaker_names_raw) == FALSE & is.factor(output$event_id) == FALSE) {
    output$speaker_names_raw <- as.factor(output$speaker_names_raw)
    output$event_id <- as.factor(output$event_id)
  }
  return(output)
}
