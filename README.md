
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ConversationAlign

<!-- badges: start -->
<!-- badges: end -->

## Overview

ConversationAlign is an R package for analyzing alignment between
interlocutors (conversation partners) in natural language transcripts.
The inspiration for this project stemmed from our interest in analyzing
intergenerational communication.

<figure>
<img src="man/figures/agepos1.jpg" height="400"
alt="two people conversing" />
<figcaption aria-hidden="true">two people conversing</figcaption>
</figure>

## Nuts and bolts

ConversationAlign transforms raw language data into simultaneous time
series objects spanning 40 possible dimensions (affective, lexical, and
semantic). Here’s an overview of how the package transforms language to
time series data for computing alignment indices<br/>

<figure>
<img src="man/figures/overview.png" height="500"
alt="overview of ConversationAlign" />
<figcaption aria-hidden="true">overview of
ConversationAlign</figcaption>
</figure>

## Installation

Install the development version of ConversationAlign from
[GitHub](https://github.com/) by entering the following in your console
or script:

``` r
install.packages("devtools")
devtools::install_github("Reilly-ConceptsCognitionLab/ConversationAlign")
```

## Prep your language transcripts

ConversationAlign will work on txt or csv files. It can handle Otter.ai
formmated files, but it can also handle a home brew of your own
preferred format as long as your transcript has at least two columns
(their order in your transcript does not matter). The first (header) row
of your transcript must designate the interlocutor (person producing the
output) and the text. When prepping your raw transcripts, be careful to
mark these columns as:<br/> 1) Interlocutor <br/> 2) Text <br/>

Each conversation dyad should be saved as a separate file (e.g.,
MaryJoe_FirstDateTalk.txt). This is important for how ConversationAlign
will read your data into R, append document IDs, and split each dyad
into a separate list. ConversationAlign runs many operations on each
dyad and ultimately binds those data into a summary dataframe.
ConversationAlign marks each conversation with a unique event_id
populated from its filename (be deliberate about naming!). All other
metadata (e.g., age, timestamps, grouping variables) in your language
transcripts will be retained. <br/>

Move your raw transcripts into a folder. The default folder name
ConversationAlign will search for on your machine is ‘my_transcripts’.
However, if you want to specify your own folder name that’s fine too.
You will call that path as an argument to the first function called
read_dyads(). <br/> <br/>

## Read your transcripts into R

### read_dyads()

This function will read all your files and concatenate them into a
single dataframe, appending document IDs. You can call this dataframe
whatever you like. read_dyads will default to reading all csv and txt
files in a folder called my_transcripts. Just remember that when you are
finished processing a set of transcripts, make sure to move them out of
that folder. You can think of ‘my_transcripts’ as a staging area for
loading data into ConversationAlign.

``` r
MyRawLangSamples <- read_dyads()
#if you want to specify a different folder, supply your own path
MyRawLangSamples <- read_dyads("/my_custompath")
```

## Clean your transcripts: clean_dyads

clean_dyas uses regular expressions to clean and format your data. The
function also omits stopwords using a custom stopword list, and it
lemmatizes (converts all words to their dictionary entries) unless you
tell it not to (lemmatize=T is the default). Run ‘clean_dyads’ on the
object you just assembled by running the ‘read_dyads’ function in the
last step.

``` r
MyCleanLangSamples <- clean_dyads()
```

## Align your transcripts

Prompts user to specify one or more variables to align on from a lookup
database (lookup_db) reflecting published word norms from numermous
sources (e.g., afffectvec, Kuperman norms, Brysbaert norms, etc.). Yokes
data to each word then structures a dataframe by speaker and exchange
across each dyad. <br/>

myvars \<- select.list(c(“admiration”, “anger”, “animosity”,
“anticipation”, “anxiety”, “aoa”, “awe”, “boredom”, “calmness”,
“closeness”, “comfort”, “compatibility”, “concreteness”, “confusion”,
“contempt”, “disgust”, “distance”, “dominance”, “doubt”, “empathy”,
“encouragement”, “excitement”, “fear”, “friendliness”, “gratitude”,
“happiness”, “hostility”, “interest”, “joy”, “lg10wf”, “love”,
“n_letters”, “relieved”, “sadness”, “satisfaction”, “stress”,
“surprise”, “tension”, “trust”, “valence”

``` r
align_dyads <- function(clean_ts_df) {
```

## Summarize transcripts

``` r
#TBD
```

## Get in touch!

Contact <jamie_reilly@temple.edu> for feedback and assistance.
