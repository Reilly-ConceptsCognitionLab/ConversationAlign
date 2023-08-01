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
#' @importFrom tidyverse mutate
#' @importFrom tidytable consecutive_id
#' @importFrom dplyr bind_rows
#' @importFrom tidyverse group_by
#' @importFrom tidyverse summarise
#' @importFrom tidyverse summarise(across())
#' @export align_dyads

align_dyads <- function(clean_ts_df) {
  load("data/lookup_db.rda") #load lookup database
  #allow the user to select what variables they want to align, or provide their own database(s) and subset them
  myvars <- select.list(c("admiration", "anger", "animosity", "anticipation", "anxiety", "aoa", "arousal", "awe", "boredom", "calmness",  "closeness", "comfort", "compatibility", "concreteness", "confusion", "contempt", "disgust", "distance", "dominance", "doubt", "empathy", "encouragement", "excitement", "fear", "friendliness", "gratitude", "happiness", "hostility", "interest", "joy", "wfreq", "love", "n_letters", "relieved", "sadness", "satisfaction", "stress", "surprise", "tension", "trust", "valence"),
                        preselect = NULL, multiple = TRUE,
                        title = "Select the variables you would like to align your conversation transcripts on. Please do not select more than three variables.",
                        graphics = FALSE)

  if (length(myvars) == 0) { #if no variables are selected, defaults are automatically added
    myvars <- c("happiness", "hostility", "empathy", "excitement")
  }
  var_selected <- lookup_db %>% #select desired columns from lookup_db
    select(matches("^word$"), contains(myvars))
  #create variable containing the column names of each variable to be aligned
  var_aligners <- colnames(var_selected)[-grep("^word$", colnames(lookup_db), ignore.case = TRUE)]

  ts_list <- split(clean_ts_df, f = clean_ts_df$Event_id) #split transcript df into list by Event_id
  ts_aligned_list <- lapply(ts_list, function(ts_select){
    #join measures of each variable to each word in each transcript
    df_aligned <- left_join(ts_select, var_selected, by = c("CleanText" = "word"), multiple = "first")
    df_aligned <- df_aligned[complete.cases(df_aligned), ] # remove any words that couldn't be aligned
    df_aligned <- data.frame(df_aligned)

    df_aligned_agg <- df_aligned %>%
      mutate(TurnCount = consecutive_id(Speaker_names_raw), .before = 1) %>% # add a turn column
      select(Event_id, Speaker_names_raw, TurnCount, Time, contains(var_aligners), starts_with("Analytics")) %>%
      # select variables, speaker and dyad information, and word analytics
      group_by(Event_id, TurnCount, Speaker_names_raw) %>% #group by doc id, turn, and speaker
      summarise(Time = min(Time), #make time the minimum for each turn
                across(starts_with(var_aligners) & ends_with(var_aligners), mean), #average each variable by turn
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
      randomly <- lapply(split(aligned_ts_df, aligned_ts_df$Event_id), function(x){ #iterates over each doc
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
