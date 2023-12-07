#' read_dyads
#'
#' Reads pre-formatted dyadic (2 interlocutor) conversation transcripts from your machine. Transcripts must be either csv or txt format. IF you are supplying a txt file, your transcript must be formatted as an otter.ai txt file export. Your options for using csv files are more flexible. ConversationAlign minimally requires a csv file with two columns, denoting interlocutor and text. Each separate conversation transcript should be saved as a separate file. ConversationAlign will use the file names as a document ID. Within the read dyads function, set the folder_name argument as the directory path to the local folder containing your transcripts on your machine (e.g., "my_transcripts"). Please see our github page for examples of properly formatted transcripts: https://github.com/Reilly-ConceptsCognitionLab/ConversationAlign
#'
#' @name read_dyads
#' @param folder_name folder of conversation transcripts in csv or txt format
#' @return a concatenated dataframe with each language transcript saved as a separate 'event_id'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @export read_dyads


read_dyads <- function(folder_name = "my_transcripts") {
  #defines three functions - the two that select and format txt and csv files, and the function that actually reads in the otter transcript txt file.

  read_otter_transcript <- function(file_path) {
    lines <- readLines(file_path) #read otter ai file
    #removes otter ai watermark if it is present
    if (any(grepl("otter.ai", lines)) == TRUE) {
      lines <- as.character(lines[-(grep("otter.ai", lines))])}
    num_lines <- length(lines) #create a var for number of lines
    speaker <- character()
    time <- character()
    text <- character()

    #process lines of dialogue
    current_line <- 1
    while (current_line <= num_lines) {

      first_line_vec <- strsplit(lines[current_line], " ")
      speaker_time <- first_line_vec[[1]]

      speaker <- c(speaker, speaker_time[1]) #select speaker
      #checks for a colon to differentiate between time and speaker
      timeadd <- tryCatch({speaker_time[max(grep(":", speaker_time))]}, #attempts to identify a colon
                          warning = function(w){return(NA)}) #if no colon, continues without time
      time <- c(time, timeadd)

      #select lines of speech
      speech_lines <- character()
      line_counter <- current_line + 1
      while (line_counter <= num_lines && lines[line_counter] != "") { #if not max line/empty line
        speech_lines <- c(speech_lines, lines[line_counter]) #add text on line to speech text vector
        line_counter <- line_counter + 1
      }
      text <- append(text, paste(speech_lines, collapse = " "))
      current_line <- line_counter + 1 #move to next speaker
    }
    #create df
    transcript_df <- data.frame(Participant_ID = speaker,
                                time = time,
                                rawtext = text,
                                stringsAsFactors = FALSE)
    return(transcript_df)
  }
  #END DEFINE OTTER READ TRANSCRIPT .TXT FILE FUNCTION

  #DEFINE READ ME TXT FILE FUNCITON
  read_dyads_txt <- function(folder_name){
    if (any(grepl("*.txt$", list.files(path = folder_name, pattern = ".", full.names = TRUE, recursive = TRUE))) == TRUE) {
      file_list_txt <- list.files(path = folder_name, pattern = "*.txt$", full.names = TRUE, recursive = TRUE) #list files with .txt ending
      file_names_txt <- list.files(path = folder_name, pattern = ".txt$", full.names = FALSE, recursive = TRUE)
      file_names_txt <- gsub('.*/ ?(\\w+)', '\\1', file_names_txt)
      file_names_txt <- gsub(".txt$", "", file_names_txt)

      txtdata <- lapply(file_list_txt, function(x) {
        #runs txt files names through otter reading function
        xorf <- read_otter_transcript(x)
        #selects those that were properly transcribed by otter, still need to add other parameters
        if (ncol(xorf) == 3) {
          x <- xorf
        }
        else {
          # Other function for reading in non-otter txt transcripts can go here later
          stop(paste("Unable to read transcript ", as.character(match(x, file_list_txt)), " as an otter transcript. Please refer to the ConversationAlign GitHub page for examples of properly formatted transcripts.", sep = ""), call. = FALSE)
        }
      })

      #adds a doc id column to each transcript based on its name attribute
      txtdata <- lapply(file_names_txt, function(x){
        txtdata[[match(x, file_names_txt)]] <- cbind(event_id = rep(x, nrow(txtdata[[match(x, file_names_txt)]])), txtdata[[match(x, file_names_txt)]])})
      return(txtdata)
    }}
  #END DEFINE READ ME TXT FILE FUNCITON

  #DEFINE READ ME CSV FILE FUNCTION
  read_dyads_csv <- function(folder_name) {
    if (any(grepl("*.csv$", list.files(path = folder_name, pattern = ".", full.names = TRUE, recursive = TRUE))) == TRUE) {
      file_list_csv <- list.files(path = folder_name, pattern = "*.csv$", full.names = TRUE, recursive = TRUE)
      file_names_csv <- list.files(path = folder_name, pattern = ".csv$", full.names = FALSE, recursive = TRUE)
      file_names_csv<- gsub('.*/ ?(\\w+)', '\\1', file_names_csv)
      file_names_csv <- gsub(".csv$", "", file_names_csv)

      #creates a list of read in csv data frames
      csvdata <- lapply(file_list_csv, function(x){
        x_read_csv <- read.csv(x, header = TRUE)
        #identify and remove all columns that are entirely NA or empty strings
        sum_nas_es <- apply(x_read_csv, 2, function(y){sum(is.na(y) | y == "")})
        x_read_csv <- x_read_csv[sum_nas_es < nrow(x_read_csv)]
        x_read_csv <- data.frame(x_read_csv)

        #check that the column names are correct
        if ((any(grepl("^speaker$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^speaker_names_raw$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Participant$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Interlocutor$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Patient$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Person$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Partner$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Source$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^PID$", colnames(x_read_csv), ignore.case = T)) == TRUE) &
            (any(grepl("^Text$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Turn$", colnames(x_read_csv), ignore.case = T)) == TRUE |
             any(grepl("^Utterance$", colnames(x_read_csv), ignore.case = T)) == TRUE)) {

          #tests for column named time - if not present it adds a time column filled with NAs
          if (any(grepl("^time*", colnames(x_read_csv), ignore.case = TRUE)) |
              any(grepl("^start*", colnames(x_read_csv), ignore.case = TRUE))) {
            colnames(x_read_csv)[which(grepl("^time*", colnames(x_read_csv), ignore.case = T) |
                                         grepl("^start*", colnames(x_read_csv), ignore.case = T))] <- "time"
          }
          else {
            x_read_csv$time <- rep(NA, nrow(x_read_csv)) #if no time col fill with NA
          }
          #correct the speaker and text names to our conventions
          colnames(x_read_csv)[which(grepl("speaker", colnames(x_read_csv), ignore.case = T) |
                                       grepl("PID", colnames(x_read_csv), ignore.case = T) |
                                       grepl("interlocutor", colnames(x_read_csv), ignore.case = T) |
                                       grepl("patient", colnames(x_read_csv), ignore.case = T) |
                                       grepl("person", colnames(x_read_csv), ignore.case = T) |
                                       grepl("partner", colnames(x_read_csv), ignore.case = T) |
                                       grepl("source", colnames(x_read_csv), ignore.case = T) |
                                       grepl("participant", colnames(x_read_csv), ignore.case = T))] <- "Participant_ID"

          colnames(x_read_csv)[which(grepl("Text", colnames(x_read_csv), ignore.case = T) |
                                       grepl("utterance", colnames(x_read_csv), ignore.case = T))] <- "rawtext"

          x_read_csv <- data.frame(x_read_csv)
          x_final <- x_read_csv
        }

        col_check <- x_read_csv[, colnames(x_read_csv) %in%
                                  c("Participant_ID", "rawtext", "time")]
        if (ncol(col_check) != 3) { #if there are less than three columns
          stop(paste("Function is unable to process csv transcript ", #error stating missing column
                     as.character(match(x, file_list_csv)), #also states the transcript
                     " correctly. Make sure that each transcript includes a column marking who is producing text in each row.
                     When you are preparing your language transcripts, please make sure this column is named one of the following: 'interlocutor', 'person', 'partner', 'source', 'speaker', 'participant', 'PID', or 'speaker_names_raw'.
                     Please make sure your a column containing raw language transcriptions is named 'utterance', 'turn', or 'text'.
                     If you wish to include a time column please title it 'time' or 'start'.",
                     sep = ""), call. = FALSE)

        }
        x_final
      })
      csvdata <- lapply(csvdata, data.frame)
      csvdata <- lapply(file_names_csv, function(x){
        csvdata[[match(x, file_names_csv)]] <- cbind(event_id = rep(x, nrow(csvdata[[match(x, file_names_csv)]])), csvdata[[match(x, file_names_csv)]])})
      csvdata
    }}
  #END DEFINE READ ME CSV FILE FUNCTION
  #calls two functions to read in txt and csv file transcripts, returning a list.
  txtlist <- read_dyads_txt(folder_name)
  csvlist <- read_dyads_csv(folder_name)
  all_list <- append(txtlist, csvlist) #append the two lists into one list

  #throws an error if no files are found
  if (length(all_list) == 0) {
    stop("No files found. Please make sure you are providing the local or absolute file path to the desired folder as a character vector. At least one .csv or .txt file must be present.")
  }

  alldf <- dplyr::bind_rows(all_list) #binds the rows  of each list into one data frame
  alldf$event_id <- as.factor(alldf$event_id)
  alldf$Participant_ID <- as.factor(alldf$Participant_ID)
  return(alldf)
  #outputs a data frame containing every dyad with columns: event_id, Participant_ID, time, and rawtext
}
