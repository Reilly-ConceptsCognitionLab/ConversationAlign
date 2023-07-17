readme <- function(folder_name = "my_transcripts") {
  #calls three functions to read in txt, csv, and word file transcripts and returning a list.
  txtlist <- readmetxt(folder_name)
  csvlist <- readmecsv(folder_name)
  #binds the rows  of each list into one data frame
  alldf <- bind_rows(txtlist, csvlist)
  return(alldf)
}

readmetxt <- function(folder_name){
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
        #Other function for reading in non-otter txt transcripts
      }
    })
    
    data.table::setattr(txtdata, "names", file_list_txt) #add names attribute to each list element
    #adds a doc id column to each transcript based on its name attribute
    txtdata <- lapply(names(txtdata), function(x){
      txtdata[[match(x, names(txtdata))]] <- cbind(Doc_id = rep(x, nrow(txtdata[[match(x, names(txtdata))]])), txtdata[[match(x, names(txtdata))]])})
    #returns the list of each data frame with doc IDs.
    return(txtdata)
  }
}

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
    timeadd <- tryCatch({speaker_time[max(grep(":", speaker_time))]},
                        warning = function(w){return(NA)})
    time <- c(time, timeadd)
    
    #select lines of speech
    speech_lines <- character()
    line_counter <- current_line + 1
    while (line_counter <= num_lines && lines[line_counter] != "") {
      speech_lines <- c(speech_lines, lines[line_counter])
      line_counter <- line_counter + 1
    }
    text <- c(text, paste(speech_lines, collapse = " "))
    current_line <- line_counter + 1 #move to next speaker
  }
  #create df
  transcript_df <- data.frame(RawSpeaker = speaker,
                              Time = time,
                              RawText = text,
                              stringsAsFactors = FALSE)
  return(transcript_df)
}

readmecsv <- function(folder_name) {
  if (any(grepl("*.csv$", list.files(path = folder_name, pattern = ".", full.names = TRUE, recursive = TRUE))) == TRUE) {
    file_list_csv <- list.files(path = folder_name, pattern = "*.csv$", full.names = TRUE, recursive = TRUE) #list files with .csv ending
    #creates a list of read in csv dataframes
    csvdata <- lapply(full_file_list, function(x){
      x <- read.csv(x)
      if (ncol(x) == 3) {
        if (sort(tolower(colnames(x))) != sort("speaker", "text", "time")) {
          #test for columns somehow...
        }
      }
      else {
        #tests for the three columns we need? Maybe it throws an error? 
      }
    })
    
    #Check this! It will definitely have to change!
    csvdata <- lapply(csvdata, function(x) {
      if (ncol(i) != 3) {
        #col number issue csv file func
      }
      else if (colnames(i) != c("Speaker", "Time", "Text")) {
        #col name/content issue csv file func
      }
    })
    
    data.table::setattr(csvdata, "names", file_list_csv) #add names attribute to textdata
    #adds a doc id column to each transcript based on its name attribute
    csvdata <- lapply(names(csvdata), function(x){
      csvdata[[match(x, names(csvdata))]] <- cbind(Doc_id = rep(x, nrow(csvdata[[match(x, names(csvdata))]])), csvdata[[match(x, names(csvdata))]])})
    return(csvdata)
  }
}
