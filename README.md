
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ConversationAlign

<!-- badges: start -->
<!-- badges: end -->

## Overview

An R package for analyzing semantic and affective alignment between
speakers in natural language transcripts. One of the primary
applications of this package will be analyzing attributes of
intergenerational communication.

<figure>
<img src="man/figures/agepos1.jpg"
alt="2 women having a conversation" />
<figcaption aria-hidden="true">2 women having a
conversation</figcaption>
</figure>

## Installation

Install the development version of ConversationAlign from
[GitHub](https://github.com/) by typing the following in your console or
script (make sure you have devtools installed):

``` r
install.packages("devtools")
devtools::install_github("Reilly-ConceptsCognitionLab/ConversationAlign")
```

## Prep your language transcripts

Move your Otter.ai text files or CSV files into a folder. The default
folder name is ‘my_transcripts’. If you want to specify your own folder
name in the root directory that’s fine too. You will call that path as
an argument to the first function called read_dyads()

## Read your transcripts into R

### read_dyads()

This function will read all your files and concatenate them into a
single dataframe, appending document IDs

``` r
myrawtranscripts <- read_dyads()
myrawtranscripts <- read_dyads(/my_customfolder)  #if specifying a custom folder path
read_dyads <- function(folder_name = "my_transcripts") {
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
    transcript_df <- data.frame(Speaker_names_raw = speaker,Time = time, RawText = text,
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
```

## Clean your transcripts

This step uses regular expressions to clean and format your data,
eliminating stopwords, changing the case to lower, omitting whitespaces
and non-alphabetic characters, etc.

``` r
#takes object from the read_dyads step
mycleantranscripts <- clean_dyads(myrawtranscripts)
clean_dyads <- function(read_ts_df) {
  read_data_frame <- read_ts_df %>%
    filter(Speaker_names_raw != "Unknown") %>% #filter out common unwanted speaker names
    filter(Speaker_names_raw != "unknown") %>%
    filter(Speaker_names_raw != "Speaker") %>%
    filter(Speaker_names_raw != "speaker") %>%
    filter(Speaker_names_raw != "Other") %>%
    filter(Speaker_names_raw != "other") %>%
    filter(Speaker_names_raw != "E") %>%
    filter(Speaker_names_raw != "e") %>%
    filter(Speaker_names_raw != "Experimenter") %>%
    filter(Speaker_names_raw != "experimenter") %>%
    filter(Speaker_names_raw != "Assistant") %>%
    filter(Speaker_names_raw != "assistant")
  read_data_frame$Speaker_names_raw <- as.factor(read_data_frame$Speaker_names_raw) #convert variables to factor
  read_data_frame$Doc_id <- as.factor(read_data_frame$Doc_id)

  #convert time from hh:mm:ss or mm:ss to milliseconds
  read_data_frame$Time <- sapply(read_data_frame$Time, function(x){
    if (any(grepl(":", x)) == TRUE) {  #checks for colons, indicative of mm:ss
      x <- as.numeric(unlist(str_split(x, ":"))) #breaks string into vector by colon placement
      if (length(x) == 2) { #shows just mm, ss
        sum((x[1]*60000), (x[2]*1000))      }
      else if ( length(xvec) == 3) { #shows hh, mm, ss
        sum((x[1]*3600000), (x[2]*60000), (x[3]*1000))}}
    else if (is.na(x) == TRUE) { #keeps NA time values as NA - may be a better way to do this?
      NA}})

  load("data/omissions_dyads23.rda") #load in omissions database

  clean <- function(x) {
    x <- tolower(x) #to lower
    x <- gsub("\"", " ", x)
    x <- gsub("\n", " ", x)
    x <- gsub("`", "'", x)  # replaces tick marks with apostrophe for contractions
    x <- gsub("can't", "can not", x)
    x <- gsub("won't", "will not", x)
    x <- gsub("n't", " not", x) #replace contraction with full word not
    x <- textclean::replace_contraction(x) #replace contractions
    x <- gsub("-", " ", x) #replace all hyphens with spaces
    x <- tm::removeWords(x, omissions_dyads23$.)
    x <- gsub("\\d+(st|nd|rd|th)", " ", x) #omits 6th, 23rd, ordinal numbers
    x <- gsub("[^a-zA-Z]", " ", x) #omit non-alphabetic characters
    x <- gsub("\\b[a]\\b{1}", " ", x)
    x <- tm::stripWhitespace(x)
    x <- stringr::str_squish(x)
    x <- textstem::lemmatize_words(x)
  }

  read_data_frame$RawText <- str_squish(read_data_frame$RawText) #remove unneeded white space from text

  df_with_word_count <- read_data_frame %>%
    rowwise() %>% #group by individual row
    mutate(Analytics_wordcount_raw = length(stri_remove_empty(str_split_1(paste(RawText, collapse = " "), " "))), #create new column of word count by row
           Analytics_mean_word_length_raw = mean(nchar(stri_remove_empty(str_split_1(paste(RawText, collapse = " "), pattern = " "))))) %>% #create new column of average word length by row
    ungroup()

  dfclean <- df_with_word_count %>%
    mutate(CleanText = clean(RawText)) %>%  #run clean function on raw text, making a new column
    rowwise() %>% #group by individual row
    mutate(Analytics_wordcount_clean = length(stri_remove_empty(str_split_1(paste(CleanText, collapse = " "), " "))), # create word count column for cleaned text
           Analytics_mean_word_length_clean = mean(nchar(stri_remove_empty(str_split_1(paste(CleanText, collapse = " "), pattern = " "))))) %>% #create mean word length column for clean text
    ungroup() %>%
    select(!RawText)# remove old raw text and grouping column

  dfclean_sep <- tidyr::separate_rows(dfclean, CleanText) # create row for each word in clean text

  dfclean_filtered <- dfclean_sep %>%
    filter(CleanText != "")#remove rows where text is an empty string

  #calculate words removed from the difference between the raw word count and clean word count
  dfclean_filtered$Analytics_words_removed <- dfclean_filtered$Analytics_wordcount_raw - dfclean_filtered$Analytics_wordcount_clean

  return(dfclean_filtered)
}
```

## Align your transcripts

Prompts user to specify one or more variables to align on from a lookup
database (lookup_db) reflecting published word norms from numermous
sources (e.g., afffectvec, Kuperman norms, Brysbaert norms, etc.). Yokes
data to each word then structures a dataframe by speaker and exchange
across each dyad.

``` r
#takes cleaned dataframe from clean_dyads() step
align_dyads <- function(clean_ts_df) {
  load("data/lookup_db.rda") #load lookup database
  #allow the user to select what variables they want to align, or provide their own database(s) and subset them
  myvars <- select.list(c("admiration", "anger", "animosity", "anticipation", "anxiety", "aoa", "awe", "boredom", "calmness",  "closeness", "comfort", "compatibility", "concreteness", "confusion", "contempt", "disgust", "distance", "dominance", "doubt", "empathy", "encouragement", "excitement", "fear", "friendliness", "gratitude", "happiness", "hostility", "interest", "joy", "lg10wf", "love", "n_letters", "relieved", "sadness", "satisfaction", "stress", "surprise", "tension", "trust", "valence", "add my own database as well"),
                        preselect = NULL, multiple = TRUE,
                        title = "Select the variables you would like to align your conversation transcripts on",
                        graphics = FALSE)

  if (length(myvars) == 0) { #if no variables are selected, defaults are automatically added
    myvars <- c("happiness", "hostility", "empathy", "excitement")
  }

  var_selected <- lookup_db %>% #select desired columns from lookup_db
    select(matches("^word$"), contains(myvars))

  if (any(grepl("add my own database as well", myvars)) == TRUE) {
    #take use input for the full file path to the data base they want to use
    database_path <- readline("Input the file path to the database you would like to add.")
    user_added_db <- read.csv(database_path) #IS IT OK TO ASSUME THAT DATABASE WILL BE .CSV???
    user_added_db <- data.frame(user_added_db)
    #display the column names of user added database and allow them to choose the columns they want
    subset_user_db <- select.list(c(colnames(user_added_db), "Select all columns"),
                                  preselect = NULL, multiple = TRUE,
                                  title = "Select the columns you would like to subset. The word column must be included.",
                                  graphics = FALSE)
    #allows user to select one option to select every column in their added database
    if (any(grepl("Select all columns", subset_user_db)) == TRUE) {
      subset_user_db <- colnames(user_added_db)
    }
    user_added_db <- user_added_db %>% select(contains(subset_user_db)) #select the columns specified from the database
    #alter the word column on the added database to match the column name of the built in databse
    colnames(user_added_db)[grep("^word$", colnames(user_added_db), ignore.case = TRUE)] <- "word"
    #if user added their own database and subsetted from built in - binds both together.
    if (length(myvars[-grep("add my own database as well", myvars)]) > 0) {
      var_selected <- full_join(x = var_selected, y = user_added_db, by="word")
    }
  }
  #create variable containing the column names of each variable to be aligned
  var_aligners <- colnames(var_selected)[-grep("^word$", colnames(lookup_db), ignore.case = TRUE)]

  var_selected <- var_selected %>% distinct(word, .keep_all = TRUE)

  ts_list <- split(clean_ts_df, f = clean_ts_df$Doc_id) #split the transcript data frame into a list by Doc_id
  ts_aligned_list <- lapply(ts_list, function(ts_select){
    #join measures of each variable to each word in each transcript
    df_aligned <- left_join(ts_select, var_selected, by = c("CleanText" = "word"), multiple = "first")
    df_aligned <- df_aligned[complete.cases(df_aligned), ] # remove any words that couldn't be aligned
    df_aligned <- data.frame(df_aligned)

    df_aligned_agg <- df_aligned %>%
      mutate(TurnCount = consecutive_id(Speaker_names_raw), .before = 1) %>% # add a turn column
      select(Doc_id, Speaker_names_raw, TurnCount, Time, contains(var_aligners), starts_with("Analytics")) %>%
      # select variables, speaker and dyad information, and word analytics
      group_by(Doc_id, TurnCount, Speaker_names_raw) %>% #group by doc id, turn, and speaker
      summarise(Time = min(Time), #make time the minimum for each turn
                across(contains(var_aligners), mean), #average each variable by turn
                across(starts_with("Analytics_wordcount"), sum), #sum word counts
                across(starts_with("Analytics_words_removed"), sum), #sum removed word counts
                across(starts_with("Analytics_mean_word_length"), mean),
                .groups = "drop") %>%
      ungroup() #reformat data frame back to chronological order
    # identifies if there are an odd number of rows (one speaker spoke but other did not respond)
    if ((nrow(df_aligned_agg)%%2) == 1 ) {
      temprow <- data.frame(matrix(NA, nrow = 1, ncol = ncol(df_aligned_agg))) #creates a new adder row
      colnames(temprow) <- c(colnames(df_aligned_agg))
      df_aligned_agg <- rbind(df_aligned_agg, temprow) #adds row full of NA to end of the data frame
    }
    ExchangeCount <- rep(seq(1:(length(df_aligned_agg$TurnCount)/2)), each=2) #creates Exchange Count
    df_aligned_EC <- cbind(ExchangeCount, df_aligned_agg) #binds ExC to the data frame
    df_aligned_EC <- na.omit(df_aligned_EC) #removes added NA row
    df_aligned_EC <- df_aligned_EC %>%
      select(!TurnCount) #removes turn count column

    df_aligned_EC #output the transcript exchange count organized aligned data frame to a list
  })
  ts_aligned_df_total <- bind_rows(ts_aligned_list)

  #DEFINE THE DEMOGRAPHIC_ALIGN FUNCTION
  demographic_align <- function(aligned_ts_df) {
    #allow user to input the file path to demographic data, randomly assign groups, or not align groups
    ask_demo_filepath <- readline("If you would like to align demographics to speakers, input the file path to the demographic csv file.")
    #if user inputs 'random', randomly assigns groups across transcripts
    if (str_to_lower(ask_demo_filepath) == "random") {
      randomly <- lapply(split(aligned_ts_df, aligned_ts_df$Doc_id), function(x){ #iterates over each doc
        x <- data.frame(x)
        #creates a vector of each speaker with random indexes and assigns a alphanumeric sequence name
        speakervec <- sample(unique(x[,grep("Speaker_names_raw", colnames(x), ignore.case = T)]))
        names(speakervec) <- paste("S", 1:length(speakervec), sep = "")
        #creates a data frame with just speaker names and assigned code
        coloutput <- data.frame(Speaker_names_raw = speakervec,
                                Speaker_Code_Random = sapply(speakervec, function(y) {
                                  names(speakervec)[match(y, speakervec)]}))
        x <- x %>% left_join(coloutput, by=c("Speaker_names_raw")) #binds code to the aligned data frame
      })
      randomly <- bind_rows(randomly) #binds all the doc data frame into one
      return(randomly)
    }
    #if input is empty, returns the aligned data frame with no demographics
    else if (ask_demo_filepath == "") {
      return(aligned_ts_df)
    }
    #if file path is entered:
    else {
      #reads in a csv file of demographic information associated with participant IDs.
      demoinfo <- data.frame(read.csv(ask_demo_filepath))
      #allows the user to specify which columns they want to subset
      subset_demo_data <- select.list(c(colnames(demoinfo), "Select all columns"),
                                      preselect = NULL, multiple = TRUE,
                                      title = "Select the columns you would like to subset. The participant ID column must be included.",
                                      graphics = FALSE)
      #if the select all option is chosen, selects every column
      if (any(grepl("Select all columns", subset_demo_data)) == TRUE) {
        subset_demo_data <- colnames(demoinfo)
      }
      demos_selected <- demoinfo %>%
        select(contains(subset_demo_data)) #selects only specified columns from the demographics

      demos <- demos_selected %>%
        select(!contains("PID")) %>%
        select(!contains("Participant")) #selects only columns that aren't participant ID

      partid <- demos_selected %>%
        select(contains(setdiff(colnames(demos_selected), colnames(demos))))
      #creates a new data frame that just includes specified demo domains and combines them into to one string, which will be a total combination of demographics
      domaincode <- data.frame(sapply(colnames(demos), function(x) {
        domainlvl <- sort(unique(demos[, match(x, colnames(demos))]))  #creates a vector of unique domain info
        names(domainlvl) <- paste("S", 1:length(domainlvl), sep = "")  #alphabetically assigns a code to each
        coloutput <- sapply(demos[match(x, colnames(demos))], function(y) {
          names(domainlvl)[match(y, domainlvl)]
        })
        coloutput
      }))
      colnames(domaincode) <- paste("Speaker_group_var_code", tolower(colnames(demos)), sep = "_")
      colnames(demos) <- paste("Speaker_group_var", tolower(colnames(demos)), sep = "_")
      demos <- cbind(demos, domaincode) #bind the assigned codes to the original groups
      demos[] <- lapply(demos[], factor) #make each grouping variable a factor
      demos <- cbind(PID = partid, demos) #bind participant ID to the demographic groups

      demo_aligned_df <- aligned_ts_df %>%
        left_join(demos, by=c("Speaker_names_raw" = "PID")) #align demographic groups by participant ID

      return(demo_aligned_df)
    }}
  #END DEFINING DEMOGRAPHIC_ALIGN FUNCTION
  demographic_align(aligned_ts_df = ts_aligned_df_total) #run demoraphic aligner on aligned data frame
}
```

## Inspect your transcripts

``` r
#TBD
```

## Analyze your transcripts

``` r
#TBD
```

## Get in touch!

Contact <jamie_reilly@temple.edu> for feedback and assistance.
