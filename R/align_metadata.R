#' align_metadata
#'
#' Prompts the user for a file path to a csv file. Joins metadata in the file to the output of align_dyads by 'event_id' and 'Participant_ID'
#'
#' @name align_metadata
#' @param aligned_ts_df a data frame with yoked psycholingustic variables from the align_dyads function
#' @return a data frame with user provided and selected metadata joined by 'event_id' and 'Participant_ID'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr  select
#' @importFrom dplyr left_join
#' @importFrom utils read.csv
#' @importFrom utils select.list
#' @export align_metadata

align_metadata <- function(aligned_ts_df) {
  #allow user to input the file path to demographic data, randomly assign groups, or not align groups
  ask_meta_filepath <- readline(writeLines("If you would like to align metadata by interlocutor and event ID, input the absolute or relative file path to the metadata csv file.\nThe file path should not be in quotes (e.g. my_data/metadata.csv)\nThe csv file must contain column names 'Participant_ID' and 'event_id' or it will not align."))
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

  #join metadata to aligned data frame by event id and PID
  metadata_aligned_df <- dplyr::left_join(aligned_ts_df, metadata_selected,
                                          by=c("event_id", "Participant_ID"))

  return(metadata_aligned_df)
}
