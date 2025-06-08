#' read_1file
#'
#' Reads pre-formatted dyadic (2 interlocutor) conversation transcript already imported into your R environment.
#'
#' @name read_1file
#' @param my_dat conversation transcript in csv or txt format
#' @return a dataframe formatted with 'Event_ID', "Participant_ID", "RawText" -- ready for clean_dyads()
#' @export

read_1file <- function(my_dat) {
  #returns name not contents of mydat
  object_name <- deparse(substitute(my_dat))

  # Convert to data frame if not already
  if (!is.data.frame(my_dat)) {
    my_dat <- as.data.frame(my_dat)
  }

  # Store original column names for reference
  original_cols <- colnames(my_dat)

  # Standardize column names (case-insensitive)
  colnames(my_dat) <- tolower(colnames(my_dat))

  # Initialize standardized columns
  standardized_cols <- colnames(my_dat)

  # Participant ID detection and standardization
  participant_pattern <- "speaker|speaker_names_raw|participant|interlocutor|patient|person|partner|source|pid|talker"
  participant_idx <- grepl(participant_pattern, colnames(my_dat))
  if (sum(participant_idx) > 0) {
    standardized_cols[participant_idx] <- "Participant_ID"
  }

  # RawText detection and standardization
  text_pattern <- "text|turn|talker|mytext|utterance|my_text"
  text_idx <- grepl(text_pattern, colnames(my_dat))
  if (sum(text_idx) > 0) {
    standardized_cols[text_idx] <- "RawText"
  }

  # Apply standardized names
  colnames(my_dat) <- standardized_cols

  # Check required columns exist
  required_cols <- c("Participant_ID", "RawText")
  missing_cols <- setdiff(required_cols, colnames(my_dat))

  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:",
               paste(missing_cols, collapse = ", "),
               "\nAvailable columns:",
               paste(original_cols, collapse = ", "),
               "\nExpected participant columns should match:", participant_pattern,
               "\nExpected text columns should match:", text_pattern),
         call. = FALSE)
  }

  # Add Event_ID using the object's name
  my_dat$Event_ID <- object_name

  # Convert ID columns to factors
  id_cols <- c("Event_ID", "Participant_ID")
  for (col in id_cols) {
    if (col %in% colnames(my_dat)) {
      my_dat[[col]] <- as.factor(my_dat[[col]])
    }
  }

  # Reorder columns to put standard ones first
  standard_cols <- c("Event_ID", "Participant_ID", "RawText")
  other_cols <- setdiff(colnames(my_dat), standard_cols)
  my_dat <- my_dat[, c(standard_cols, other_cols)]

  return(my_dat)
}
