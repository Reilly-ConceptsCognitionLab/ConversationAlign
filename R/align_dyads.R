#' align_dyads
#'
#' Yokes user-specified semantic, affective, and phonological values to each word in a cleaned language transcript. Prepares a dataframe aligned by exchange and turn across Participant_IDs.
#'
#' @name align_dyads
#' @param clean_ts_df a dataframe cleaned and formatted during the read_dyads() function
#' @return a dataframe one-word-per-row format with variables of interest appended
#' @importFrom magrittr %>%
#' @importFrom tidyselect any_of
#' @importFrom tidyselect contains
#' @importFrom tidyselect everything
#' @importFrom dplyr group_by
#' @importFrom dplyr  select
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr  mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr ungroup
#' @importFrom dplyr consecutive_id
#' @importFrom stringr str_split_1
#' @importFrom stringr str_squish
#' @export align_dyads

align_dyads <- function(clean_ts_df) {
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
                        title = writeLines("Select any number of variables you would like to align your conversation transcripts on."),
                        graphics = FALSE)
  var_selected <- lookup_db %>% #select desired columns from lookup_db
    dplyr::select(matches("^word$"), contains(myvars))
  #create variable containing the column names of each variable to be aligned
  var_aligners <- colnames(var_selected)[-grep("^word$", colnames(lookup_db), ignore.case = TRUE)]

  ts_list <- split(clean_ts_df, f = clean_ts_df$event_id) #split transcript df into list by event_id
  ts_aligned_list <- lapply(ts_list, function(ts_select){
    #join measures of each variable to each word in each transcript

    df_aligned <- dplyr::left_join(ts_select, var_selected, by = c("cleantext" = "word"), multiple = "first")

    df_aligned <- data.frame(df_aligned)
    df_aligned <- df_aligned[complete.cases(df_aligned[, c(which(colnames(df_aligned) %in% myvars))]),]     # remove rows with words that couldn't be aligned


    df_aligned_an <- df_aligned %>%
      dplyr::group_by(event_id, Participant_ID) %>%
      dplyr::mutate(wordcount_align = length(stringr::str_squish(stringr::str_split_1(paste(cleantext, collapse = " "), " "))),
                    mean_word_length_align = mean(nchar(stringr::str_squish(stringr::str_split_1(paste(cleantext, collapse = " "), " ")))),
                    word_removed_align = wordcount_clean - wordcount_align) %>%
      dplyr::ungroup()

    #group on event id and add a turn count column that sequences each uninterrupted utterance
    df_aligned_turn <- df_aligned_an %>%
      dplyr::group_by(event_id) %>%
      dplyr::mutate(turncount = dplyr::consecutive_id(Participant_ID), .before = 1)

    #add an exchange count variable, which is one turn from each interlocutor
    df_aligned_turn$exchangecount <- ceiling(df_aligned_turn$turncount / 2)
    #rearrange the columns to be more readable
    df_aligned_ec <- df_aligned_turn %>%
      dplyr::select(tidyselect::any_of(c("event_id", "Participant_ID", "exchangecount", "turncount", "cleantext", "time")),
                    tidyselect::contains(var_aligners), tidyselect::everything())
    df_aligned_ec
  })
  ts_aligned_df_total <- dplyr::bind_rows(ts_aligned_list)

  #DEFINE THE METADATA ALIGN FUNCTION
  align_metadata <- function(aligned_ts_df) {
    #allow user to input the file path to demographic data, randomly assign groups, or not align groups
    ask_meta_filepath <- readline(writeLines("If you would like to align metadata by interlocutor and event ID, input the absolute or relative file path to the metadata csv file.\nThe file path should not be in quotes (e.g. my_data/metadata.csv)\nThe csv file must contain column names 'Participant_ID' and 'event_id' or it will not align\nIf you do not wish to align metadata press enter."))
    #if input is empty, returns the aligned data frame with no demographics
    if (ask_meta_filepath == "") {
      return(aligned_ts_df)
    }
    #if file path is entered:
    else {
      #reads in a csv file of demographic information associated with participant IDs.
      metadata <- data.frame(read.csv(ask_meta_filepath))

      #check for event and participant column names and replace with correct cases if incorrect
      colnames(metadata)[grep("^event_id$", colnames(metadata), ignore.case = T)] <- "event_id"
      colnames(metadata)[grep("^Participant_ID$", colnames(metadata), ignore.case = T)] <- "Participant_ID"

      #check that there are correctly named participant and event id column headers, and throw an error if not
      if (any(any(grepl("^event_id$", colnames(metadata), ignore.case = F)),
              any(grepl("^Participant_ID$", colnames(metadata), ignore.case = F))) == FALSE) {
        stop("cannot find column header 'Participant_ID' and or 'event_id' in metadata file; the file must contain both")
      }

      #allows the user to specify which columns they want to subset - preselecting event and participant id
      subset_metadata <- select.list(c(colnames(metadata), "Select all columns"),
                                     preselect = c("event_id", "Participant_ID"), multiple = TRUE,
                                     title = "Select the columns you would like to subset. 'Participant_ID' and 'event_id' columns are preselected.",
                                     graphics = FALSE)
      #if the select all option is chosen, selects every column
      if (any(grepl("Select all columns", subset_metadata)) == TRUE) {
        subset_metadata <- colnames(metadata)
      }
      metadata_selected <- metadata[,colnames(metadata) %in% subset_metadata] #select specified columns

      #select dimensions that aren't used to align on
      meta_dims <- which(!colnames(metadata_selected) %in% c("event_id", "Participant_ID"))

      #join metadata to aligned data frame by event id and PID
      metadata_aligned_df <- dplyr::left_join(aligned_ts_df, metadata_selected,
                                              by=c("event_id", "Participant_ID"))

      return(metadata_aligned_df)
    }
  }
  #END DEFINING METADATA ALIGN FUNCTION
  output <- align_metadata(aligned_ts_df = ts_aligned_df_total) #run demoraphic aligner on aligned data frame
  if (is.factor(output$Participant_ID) == FALSE & is.factor(output$event_id) == FALSE) {
    output$Participant_ID <- as.factor(output$Participant_ID)
    output$event_id <- as.factor(output$event_id)
  }
  return(output)
}
