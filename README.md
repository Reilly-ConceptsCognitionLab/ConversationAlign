
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
<img src="man/figures/overview.png" height="700"
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

<br/> <br/>

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
transcripts will be retained. <br/> <br/>

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
loading data into ConversationAlign. <br/>

``` r
MyRawLangSamples <- read_dyads()
#if you want to specify a different folder, supply your own path
MyRawLangSamples <- read_dyads("/my_custompath")
```

<br/>

## Clean your transcripts: clean_dyads

‘clean_dyads’ uses regular expressions to clean and format your data.
The function also omits stopwords using a custom stopword list, and it
lemmatizes (converts all words to their dictionary entries) unless you
tell it not to (lemmatize=T is the default). Run ‘clean_dyads’ on the
object you just assembled by running the ‘read_dyads’ function in the
last step. <br/>

``` r
MyCleanLangSamples <- clean_dyads(MyRawLangSamples) #default is lemmatize=T
#If you do NOT want your language sample lemmatized, change the lemmatize argument to F
MyCleanLangSamples <- clean_dyads(MyRawLangSamples, lemmatize=F)
```

<br/>

## Align your transcripts: align_dyads

This is where a lot of the magic happens. align_dyads will take the
cleaned dataframe you created in the last step and yoke values to every
word by indexing a lookup database. This database was populated with
published word norms from numermous sources (e.g., afffectvec, Kuperman
norms, Brysbaert norms, etc.). Yokes data to each word then structures a
dataframe by speaker and exchange across each dyad. <br/>

You will be prompted to select one of more variables to yoke data to
that will be used in later steps to compute alignment indices. Here are
your choices: <br/>

1.  anger
2.  anxiety
3.  boredom
4.  closeness
5.  confusion
6.  dominance
7.  doubt
8.  empathy
9.  encouragement
10. excitement
11. guilt
12. happiness
13. hope
14. hostility
15. politeness
16. sadness “aff_stress”, “aff_surprise”, “aff_trust”, “aff_valence”,
    “lex_age_acquisition”, “lex_letter_count_raw”,
    “lex_morphemecount_raw”, “lex_prevalence”, “lex_senses_polysemy”,
    “lex_wordfreqlg10_raw”, “sem_arousal”, “sem_concreteness”,
    “sem_diversity”, “sem_neighbors”

``` r
align_dyads <- function(clean_ts_df) {
```

## Summarize transcripts

``` r
#TBD
```

## Get in touch!

Contact <jamie_reilly@temple.edu> for feedback and assistance.
