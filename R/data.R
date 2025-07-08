#' Sample Dyadic Interview Transcript: Marc Maron and Terry Gross Radio Interview 2013
#'
#' Text and talker information delineated, raw transcript, multiple lines per talker
#'
#' @format ## "MaronGross_2013"
#' A data.frame with 546 obs, 2 vars:
#' \describe{
#'   \item{text}{text from interview}
#'    \item{speaker}{speaker identity}
#'   ...
#' }
"MaronGross_2013"


' Sample Conversation Transcript: Nursery Rhymes
#'
#' Text and talker information delineated, 3 separate nursery rhymes, good for computing analytics and word counts
#'
#' @format ## "NurseryRhymes"
#' A data.frame with 100 observations, 2 vars:
#' \describe{
#'   \item{Event_ID}{factor 3 different simulated conversations}
#'    \item{Participant_ID}{fictional speaker names, 2 each conversation}
#'    \item{Text_Raw}{simulated language production, actually looped phrases from nursery rhymes}
#'   ...
#' }
"NurseryRhymes"


' Sample Conversation Transcript: Nursery Rhymes Prepped
#'
#' Text and talker information delineated, 3 separate nursery rhymes, good for computing analytics and word counts
#'
#' @format ## "NurseryRhymes_Prepped"
#' A data.frame with 1507 x 7 observations, 5 vars:
#' \describe{
#'   \item{Event_ID}{factor 3 different simulated conversations}
#'    \item{Participant_ID}{fictional speaker names, 2 each conversation}
#'    \item{Exchange_Count}{sequential numbering of exchanges by conversation, 1 exchange = 2 turns}
#'    \item{Turn_Count}{sequential numbering of turns by conversation}
#'    \item{Text_Clean}{content words}
#'     \item{emo_anger}{raw value of anger salience yoked to each word}
#'   ...
#' }
"NurseryRhymes_Prepped"

