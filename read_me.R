#' Read language transcripts
#'
#' Reads pre-formatted conversation transcripts from txt or csv on user's machine
#'
#' @name read_me
#' @param folder_name user can specify a folder name and directory for where their language transcripts will be read from, default is 'my-transcripts' in root
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom data.table setattr
#' @export read_me



read_me <- function(folder_name = "my_transcripts") {
  #defines three functions - the two that select and format txt and csv files, and the function that actually reads in the otter transcript txt file.

  read_otter_transcript <- function(file_path) {
    lines <- readLines(file_path) #read otter ai file
    #Ben added - removes otter ai watermark if it is present
    if (any(grepl("otter.ai", lines)) == TRUE) {
      lines <- as.character(lines[-(grep("otter.ai", lines))])}
    num_lines <- length(lines) #create a var for number of lines
    speaker <- character() #create speaker col
    time <- character() #create time col
    text <- character() #create text col

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
      while (line_counter <= num_lines && lines[line_counter] != "") { #if not max line not on empty line
        speech_lines <- c(speech_lines, lines[line_counter]) #add text on line to speech line text vector
        line_counter <- line_counter + 1
      }
      text <- c(text, paste(speech_lines, collapse = " ")) #append speech on line to vector as one string
      current_line <- line_counter + 1 #move to next speaker
    }
    #create df
    transcript_df <- data.frame(Speaker_names_raw = speaker,
                                Time = time,
                                RawText = text,
                                stringsAsFactors = FALSE)
    return(transcript_df)
  }
  #END DEFINE OTTER READ TRANSCRIPT .TXT FILE FUNCTION

  read_me_txt <- function(folder_name){
    if (any(grepl("*.txt$", list.files(path = folder_name, pattern = ".", full.names = TRUE, recursive = TRUE))) == TRUE) {
      file_list_txt <- list.files(path = folder_name, pattern = "*.txt$", full.names = TRUE, recursive = TRUE) #list files with .txt ending
      txtdata <- lapply(file_list_txt, function(x) {
        #runs txt files names through otter reading function
        xorf <- read_otter_transcript(x)
        #selects those that were properly transcribed by otter, still need to add other parameters
        if (ncol(xorf) == 3) {
          x <- xorf
        }
        else {
          # --- WIP --- Other function for reading in non-otter txt transcripts
        }
      })

      data.table::setattr(txtdata, "names", file_list_txt) #add names attribute to each list element
      #adds a doc id column to each transcript based on its name attribute
      #    txtdata <- lapply(names(txtdata), function(x){
      #      txtdata[[match(x, names(txtdata))]] <- cbind(Doc_id = rep(x, nrow(txtdata[[match(x, names(txtdata))]])), txtdata[[match(x, names(txtdata))]])})
      #returns the list of each data frame with doc IDs.
      return(txtdata)
    }} #end of the read_me txt function

  read_me_csv <- function(folder_name) {
    if (any(grepl("*.csv$", list.files(path = folder_name, pattern = ".", full.names = TRUE, recursive = TRUE))) == TRUE) {
      file_list_csv <- list.files(path = folder_name, pattern = "*.csv$", full.names = TRUE, recursive = TRUE) #list files with .csv ending
      #creates a list of read in csv dataframes
      csvdata <- lapply(full_file_list, function(x){
        x <- read.csv(x)
        if (ncol(x) == 3) {
          if (sort(tolower(colnames(x))) != sort("speaker", "text", "time")) {
            # --- WIP --- test for columns somehow...
          }
        }
        else {
          # --- WIP --- tests for the three columns we need? Maybe it throws an error?
        }
      })

      data.table::setattr(csvdata, "names", file_list_csv) #add names attribute to textdata
      #adds a doc id column to each transcript based on its name attribute
      #      csvdata <- lapply(names(csvdata), function(x){
      #        csvdata[[match(x, names(csvdata))]] <- cbind(Doc_id = rep(x, nrow(csvdata[[match(x, names(csvdata))]])), csvdata[[match(x, names(csvdata))]])})
      return(csvdata)
    }}
  #END OF THE READ_ME__CSV FUNCTION

  #calls two functions to read in txt and csv file transcripts, returning a list.
  txtlist <- read_me_txt(folder_name)
  csvlist <- read_me_csv(folder_name)
  all_list <- append(txtlist, csvlist) #append the two lists into one list

  all_list_num <- lapply(seq(length(all_list)), function(doc_num){ #iterate over each transcript
    all_list[[doc_num]] <- cbind(Doc_id = rep(doc_num, nrow(all_list[[doc_num]])), all_list[[doc_num]])}) #bind the index of the transcript as a column to the data frame
  alldf <- bind_rows(all_list_num) #binds the rows  of each list into one data frame
  return(alldf)
  #outputs a data frame containing every dyad with columns: Doc_id, Speaker_names_raw, Time, and RawText
}
