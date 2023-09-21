#' read_dyads
#'
#' Reads pre-formatted conversation transcripts from txt or csv on user's machine
#'
#' @name read_dyads
#' @param folder_name user can specify a folder name and directory for where their language transcripts will be read from, default is 'my-transcripts' in root
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom data.table setattr
#' @export read_dyads


read_dyads <- function(folder_name = "my_transcripts") {
  #defines three functions - the two that select and format txt and csv files, and the function that actually reads in the otter transcript txt file.

  read_otter_transcript <- function(file_path) {
    lines <- readLines(file_path) #read otter ai file
    #Ben added - removes otter ai watermark if it is present
    if (any(grepl("otter.ai", lines)) == TRUE) {
      lines <- as.character(lines[-(grep("otter.ai", lines))])}
    num_lines <- length(lines) #create a var for number of lines
    speaker <- character()
    time <- character()
    text <- character()

    #process lines of dialogue
    current_line <- 1
    while (current_line <= num_lines) {
      speaker_time <- strsplit(lines[current_line], " ")[[1]]
      speaker <- c(speaker, speaker_time[1]) #select speaker
      #Ben added - allows for last names and also timeless transcripts
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
      text <- c(text, paste(speech_lines, collapse = " ")) #append speech on line to vector as string
      current_line <- line_counter + 1 #move to next speaker
    }
    #create df
    transcript_df <- data.frame(speaker_names_raw = speaker,
                                time = time,
                                rawtext = text,
                                stringsAsFactors = FALSE)
    return(transcript_df)
  }
  #END DEFINE OTTER READ TRANSCRIPT .TXT FILE FUNCTION


  #DEFINE READ OTTER STYLECSV TRANSCRIPTS FUNCITON
  read_otter_format_csv <- function(file_path) {
    lines <- readLines(file_path) #read otter ai file
    #removes otter.ai watermark if it is present
    if (any(grepl("otter.ai", lines)) == TRUE) { #remove otter watermark if present
      lines <- as.character(lines[-grep("otter.ai", lines)])
    }
    if (any(grepl("column\\d", lines, ignore.case = T)) == TRUE) { #remove any column titles if present
      lines <- as.character(lines[-grep("column\\d", lines, ignore.case = T)])
    }
    lines <- gsub(",", "", lines)
    #from here on, this is the same as the function for reading otter files
    num_lines <- length(lines) #create a var for number of lines
    speaker <- character() #create speaker, time, and text col
    time <- character()
    text <- character()

    #process lines of dialogue
    current_line <- 1
    while (current_line <= num_lines) {
      speaker_time <- strsplit(lines[current_line], " ")[[1]]
      speaker <- c(speaker, speaker_time[1]) #select speaker
      #Ben added - allows for last names and also timeless transcripts
      timeadd <- tryCatch({speaker_time[max(grep(":", speaker_time))]}, #attempts to identify a colon
                          warning = function(w){return(NA)}) #if no colon, continues without gathering time
      time <- c(time, timeadd)

      #select lines of speech
      speech_lines <- character()
      line_counter <- current_line + 1
      while (line_counter <= num_lines && lines[line_counter] != "") { #if not max line not/empty line
        speech_lines <- c(speech_lines, lines[line_counter]) #add text on line to text vector
        line_counter <- line_counter + 1
      }
      text <- c(text, paste(speech_lines, collapse = " ")) #append speech on line to vector as a string
      current_line <- line_counter + 1 #move to next speaker
    }
    #create df
    transcript_df <- data.frame(speaker_names_raw = speaker,
                                time = time,
                                rawtext = text,
                                stringsAsFactors = FALSE)
    return(transcript_df)
  }
  #END DEFINE READ OTTER STYLECSV TRANSCRIPTS FUNCITON

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

        #identifies and removes the first column if it sequences by one each row
        if (is.numeric(x_read_csv[,1]) == TRUE) {
          if (all(x_read_csv[,1] == seq(x_read_csv[1,1]:x_read_csv[nrow(x_read_csv),1])) == TRUE){
            x_read_csv <- x_read_csv[,-1]
          }
        }


        # If the transcript has 2/3 cols or already formatted with at least speaker and text column
        if (ncol(x_read_csv) == 3 |
            ncol(x_read_csv) == 2 |
            ((any(grepl("^speaker$", colnames(x_read_csv), ignore.case = T)) == TRUE |
              any(grepl("^speaker_names_raw$", colnames(x_read_csv), ignore.case = T)) == TRUE |
              any(grepl("^Participant$", colnames(x_read_csv), ignore.case = T)) == TRUE |
              any(grepl("^PID$", colnames(x_read_csv), ignore.case = T)) == TRUE) &
             (any(grepl("^Text$", colnames(x_read_csv), ignore.case = T)) == TRUE |
              any(grepl("^Utterance$", colnames(x_read_csv), ignore.case = T)) == TRUE))) {

          #exclusively check if the column names are correct
          if ((any(grepl("^speaker$", colnames(x_read_csv), ignore.case = T)) == TRUE |
               any(grepl("^speaker_names_raw$", colnames(x_read_csv), ignore.case = T)) == TRUE |
               any(grepl("^Participant$", colnames(x_read_csv), ignore.case = T)) == TRUE |
               any(grepl("^PID$", colnames(x_read_csv), ignore.case = T)) == TRUE) &
              (any(grepl("^Text$", colnames(x_read_csv), ignore.case = T)) == TRUE |
               any(grepl("^Utterance$", colnames(x_read_csv), ignore.case = T)) == TRUE)) {

            #tests for column named time - if not present it adds a time column filled with NAs
            if (any(grepl("time", colnames(x_read_csv), ignore.case = TRUE))) {
              colnames(x_read_csv)[which(grepl("time", colnames(x_read_csv), ignore.case = T) |
                                           grepl("Start", colnames(x_read_csv), ignore.case = T))] <- "time"
            }
            else {
              x_read_csv$time <- rep(NA, nrow(x_read_csv)) #if no time col fill with NA
            }
            #correct the speaker and text names to our conventions
            colnames(x_read_csv)[which(grepl("speaker", colnames(x_read_csv), ignore.case = T) |
                                         grepl("PID", colnames(x_read_csv), ignore.case = T) |
                                         grepl("Participant", colnames(x_read_csv), ignore.case = T))] <- "speaker_names_raw"

            colnames(x_read_csv)[which(grepl("Text", colnames(x_read_csv), ignore.case = T) |
                                         grepl("Utterance", colnames(x_read_csv), ignore.case = T))] <- "rawtext"

            x_read_csv <- data.frame(x_read_csv)
            #remove all columns that not identified as speaker, time, or Text
            x_read_csv <- x_read_csv[, colnames(x_read_csv) %in%
                                       c("speaker_names_raw", "time", "rawtext")]
            x_read_csv
          } #end check for columns named speaker and text

          else {
            #iterate over columns, run parameters to identify which is time, speaker, and Text
            colnames(x_read_csv) <- sapply(seq(ncol(x_read_csv)), function(col_num){
              col_select <- x_read_csv[,col_num]
              if (sum(grepl("\\d:\\d\\d", col_select)) > (length(col_select)/2)) {
                "time"
              }
              else if (length(unique(col_select)) < (length(col_select)/2)) {
                "speaker_names_raw" #if most of the column is the same, makes it the speaker column
              }
              else if (length(unique(col_select) > (length(col_select)/2))) {
                "rawtext" #if most of the column is different, assigns in to text column name
              }})
            if (any(grepl("time", colnames(x_read_csv))) == FALSE) {
              x_read_csv$time <- rep(NA, nrow(x_read_csv)) #if no time col fill with NA
            }
            x_read_csv
          }
          x_final <- x_read_csv
        }
        else { #if the transcript is not formatted at all - function to process it
          x_otter_read <- read_otter_format_csv(file_path = x)
          x_final <- x_otter_read
        }
        if (any(duplicated(colnames(x_final))) == TRUE) { #tests for any repeated column names
          error_col_name <- colnames(x_final)[duplicated(colnames(x_final))]
          stop(paste("Duplicated column name ", #throws an error, stating column misinterpretation
                     paste0(error_col_name, "."),
                     " Check column names or formatting of csv transcript ",
                     as.character(match(x, file_list_csv)), sep = ""), call. = FALSE)
        }
        if (ncol(x_final) != 3) { #if there are less than three columns
          missing_col <- setdiff(tolower(sort(c("Rawspeaker", "rawtext", "time"))),
                                 tolower(sort(colnames(x_final)))) #finds missing column name(s)
          stop(paste("Function is unable to process csv transcript ", #error stating missing column
                     as.character(match(x, file_list_csv)), #also states the transcript
                     " correctly. Column ", missing_col, " is missing.",
                     sep = ""), call. = FALSE)
        }
        x_final
      })
      csvdata <- lapply(csvdata, data.frame)
      csvdata <- lapply(file_names_csv, function(x){
        csvdata[[match(x, file_names_csv)]] <- cbind(event_id = rep(x, nrow(csvdata[[match(x, file_names_csv)]])), csvdata[[match(x, file_names_csv)]])})
      return(csvdata)
    }}
  #END DEFINE READ ME CSV FILE FUNCTION
  #calls two functions to read in txt and csv file transcripts, returning a list.
  txtlist <- read_dyads_txt(folder_name)
  csvlist <- read_dyads_csv(folder_name)
  all_list <- append(txtlist, csvlist) #append the two lists into one list

  #throws an error if no files are found
  if (length(all_list) == 0) {
    stop("No files found. Please make sure you are providing the local path to the desired folder as a character vector. At least one .csv or .txt file must be present.")
  }

  alldf <- bind_rows(all_list) #binds the rows  of each list into one data frame
  alldf$event_id <- as.factor(alldf$event_id)
  alldf$speaker_names_raw <- as.factor(alldf$speaker_names_raw)
  return(alldf)
  #outputs a data frame containing every dyad with columns: event_id, speaker_names_raw, time, and rawtext
}
