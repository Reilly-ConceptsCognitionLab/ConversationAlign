
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ConversationAlign

Open-source software for computing main effects and indices of alignment
across coversation partners in dyadic conversation transcripts. <br>

<!-- badges: start -->

[![GitHub tag (latest by
date)](https://img.shields.io/github/v/tag/Reilly-ConceptsCognitionLab/SemanticDistance?color=blue)](https://github.com/Reilly-ConceptsCognitionLab/ConversationAlign/tags)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/reilly-lab/ConversationAlign/graphs/commit-activity)
[![Depsy](https://img.shields.io/badge/depsy-analyzed-blue.svg)](http://depsy.org/r/package/ConversationAlign)
[![License: GPL
v3+](https://img.shields.io/badge/License-GPL%20v3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/reilly-lab/ConversationAlign/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/reilly-lab/ConversationAlign/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- ```{r, echo=F, out.width='70%', out.height='60%', out.extra='style="float:right; padding:5px"'} -->

<!--  knitr::include_graphics("man/figures/convo.jpeg") -->

<!-- ``` -->

ConversationAlign analyzes alignment between interlocutors (conversation
partners) engaged in two-person conversations. ConversationAlign works
on language transcripts. It can handle text files (.txt) or comma
separated value (.csv) spreadsheet style files. ConversationAlign
transforms raw language data into simultaneous time series objects
spanning 30 possible dimensions via an embedded lookup database. <br>

# Overview

Here’s a schematic of how ConversationAlign processes your conversation
transcript.

<figure>
<img src="man/figures/overview.png" height="600"
alt="overview of ConversationAlign" />
<figcaption aria-hidden="true">overview of
ConversationAlign</figcaption>
</figure>

# Before

<span style="display: inline-block; padding: 0.2em 0.4em; margin: 0.1em; background-color: #8B0000; color: white; font-weight: bold; border-radius: 3px; font-family: Arial, sans-serif;">
How to prep your language transcripts for processing in
ConversationAlign </span> <br>

ConversationAlign can handle a home brew of your own preferred format.
The order of your columns does not matter. Any other data in your
transcripts (e.g., metadata, timestamps, grouping variables, physio
data) will be retained. Don’t worry about stripping punctuation or
splitting your transscripts across rows. ConversationAlign will do that
for you.

**Note: ConversationAlign can ONLY process dyadic conversation
transcripts (i.e., two person dialogues)** <br>

Conditions/Precautions: <br> 1) Your raw transcript MUST contain at
least two columns, delineating interlouctor (e.g., Mary or Joe) and
text. 2) Label your talker/interlocutor column as ‘Interlocutor’,
‘Speaker’, or ‘Participant’ <br> 3) Label your text column as ‘Text’,
‘Utterance’, or ‘Turn’. <br> 4) Save each conversation transcript
somwehere on your computer as a separate file (CSV or txt work
best).<br> 5) Be careful/deliberate about your filenaming convention.
The filename for each conversation will become its event ID (or
document_id) in the dataframe ConversationAlign processes. <br> 6) Move
all your individual conversation transcripts to be analyzed into one
folder (e.g., “my_transcripts”). This folder should ideally by nested in
the same directory you are running your R script in. <br> 7) If you have
metadata (e.g., age, timestamps, grouping variables), you can either
append this to your original transcript or merge the metdata as a
separate file. This is a useful option when you have many individual
difference and demographic details. <br>

# Installation

Install the development version of ConversationAlign from
[GitHub](https://github.com/) using the `devtools` package.

``` r
# Check if devtools is installed, if not install it
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load devtools
library(devtools)

# Check if ConversationAlign is installed, if not install from GitHub
if (!require("ConversationAlign", quietly = TRUE)) {
  devtools::install_github("Reilly-ConceptsCognitionLab/ConversationAlign")
}

# Load SemanticDistance
library(ConversationAlign)
```

# Read your transcripts into R

## read_dyads()

`read_dyads()` will read all your files and concatenate them into a
single dataframe, appending document IDs. You can call this dataframe
whatever you like. `read_dyads()` will default to reading all csv and
txt files in a folder called my_transcripts. Just remember that when you
are finished processing a set of transcripts, make sure to move them out
of that folder. You can think of your document folder as a staging area
for loading data into `ConversationAlign`.

``` r
MyRawLangSamples <- read_dyads()
#if you want to specify a different folder, supply your own path
MyRawLangSamples <- read_dyads("/my_custompath")
```

| Event_ID | Participant_ID | RawText |
|:---|:---|:---|
| taylorellen | Taylor | That was an amazing montage. |
| taylorellen | Ellen | I know. And that’s that’s no no one’s in there. We’re not gonna do it to you today. |
| taylorellen | Taylor | It’s not amateur hour. No, |
| taylorellen | Ellen | no, I’m not going to your because I can’t talk how I did it the first time you were the best scare. Well, I don’t know. Sarah Paulson was pretty good, too. But |
| taylorellen | Taylor | that was a near death experience |
| taylorellen | Ellen | it kind of was you went down |
| taylorellen | Taylor | Yeah. |
| taylorellen | Ellen | Were there injuries? |
| taylorellen | Taylor | There were several ways I could have died. |
| taylorellen | Ellen | Well,now you’re being dramatic. |
| taylorellen | Taylor | Yeah, |
| taylorellen | Ellen | yeah. No, that’s extreme. |
| taylorellen | Taylor | Yeah. |
| taylorellen | Ellen | It- I really just was watching that backstage. And I was just thinking like, thank you so much for going so above and beyond to make people happy. Yeah, |
| taylorellen | Taylor | well, |
| taylorellen | Ellen | that’s not a good example of it |

# Clean your transcripts

## clean_dyads()

`clean_dyads()` uses numerous regex to clean and format the data your
just read into R in the previous step. Although there are many cleaning
steps, here are the big ones: <br> 1) to lowercase <br> 2) omit
stopwords <br> 3) replace contractions (e.g., ‘you’re’ to ‘you are’)
<br> 4) tick marks to apostrophes <br> 5) hypens to spaces <br> 6) omits
numerals <br> 7) omits/squishes extraneous white space <br> 8)
lemmatization <br> <br>

`ConversationAlign` calls the `textstem` package as a dependency to
lemmatize your language transcript. This converts morphologiocal
derivatives to their root forms. The default is lemmatize=T. Sometimes
you want to retain language output in its native form. If this is the
case, change the argument in clean_dyads to lemmatize=F. `clean_dyads()`
outputs word count metrics pre/post cleaning by dyad and interlocutor.
This can be useful if you are interested in whether one person just
doesn’t produce many words or produces a great deal of empty utterances.

``` r
MyCleanLangSamples <- clean_dyads(MyRawLangSamples) #default is lemmatize=TRUE
#If you do NOT want your language sample lemmatized, change the lemmatize argument to F or FALSE
MyCleanLangSamples <- clean_dyads(MyRawLangSamples, lemmatize=FALSE)
```

| Event_ID | Participant_ID | TurnCount | CleanText | NWords_ByPersonTurn_RAW | NWords_ByPersonTurn_CLEAN |
|:---|:---|---:|:---|---:|---:|
| taylorellen | Taylor | 1 | amazing | 5 | 2 |
| taylorellen | Taylor | 1 | montage | 5 | 2 |
| taylorellen | Ellen | 2 | no | 18 | 5 |
| taylorellen | Ellen | 2 | s | 18 | 5 |
| taylorellen | Ellen | 2 | not | 18 | 5 |
| taylorellen | Ellen | 2 | gonna | 18 | 5 |
| taylorellen | Ellen | 2 | today | 18 | 5 |
| taylorellen | Taylor | 3 | not | 5 | 4 |
| taylorellen | Taylor | 3 | amateur | 5 | 4 |
| taylorellen | Taylor | 3 | hour | 5 | 4 |
| taylorellen | Taylor | 3 | no | 5 | 4 |
| taylorellen | Ellen | 4 | no | 33 | 12 |
| taylorellen | Ellen | 4 | I | 33 | 12 |
| taylorellen | Ellen | 4 | not | 33 | 12 |
| taylorellen | Ellen | 4 | not | 33 | 12 |

<!-- ![Example of cleaned transcripts from Taylor Swift-Ellen DeGeneres Interview, 2013](man/figures/example3_ts_edg.jpeg){width=300px, height=400px} <br/> -->

# Align your transcripts

## align_dyads()

This is where a lot of the magic happens. align_dyads will take the
cleaned dataframe you created in the last step and yoke values to every
word by indexing a lookup database. The `align_dyads()` step yokes data
to each word in the cleaned transcript text then structures a dataframe
by speaker (“Participant_ID”), exchange (“exchangecount”), and turn
(“turncount”) across each dyad (“event_id”). You will be prompted to
select one of more variables (and up to three) to yoke data to that will
be used in later steps to compute alignment indices. You will be shown a
menu wherein you can select up to three variables to be yoked to your
text. Following the menu steps, enter the number of each variable you
would like with a space separating values (e.g., “10 14 19”). <br>

Here are your choices of dimensions to align on: <br> anger, anxiety,
boredom, closeness, confusion, dominance, doubt, empathy, encouragement,
excitement, guilt, happiness, hope, hostility, politeness, sadness,
stress, surprise, trust, valence, age of acquisition, word length (by
letters), morphemes per turn, prevalence (how many people know this
word), number of word senses (polysemy), word frequency (lg10), arousal,
concreteness, semantic diversity, and semantic neighbors. <br>

Run `align_dyads()` on the cleaned dyads object you created using the
`clean_dyads()` function.

``` r
MyAlignedDyads <- align_dyads(MyCleanLangSamples)
```

<img src="man/figures/example4_ts_edg.jpeg" height="200"
alt="Example aligned transcripts anger, anxiety, boredom from Taylor Swift-Ellen DeGeneres Interview, 2013" />
<br/>

# Summarize transcripts

This last step consists of three methods, each of which computes a
seperate index of alignment.

## summarize_dyads_auc()

This returns the difference time series AUC (dAUC) for every variable of
interest you specified. For example, summarize_dyads_auc will append
dAUC values for hostility (if that’s what you’re interested in). <br>

## summarize_dyads_covar()

This returns a spearman correlation coefficient and range of lagged
Pearson correlation coefficients for each variable of interest. A vector
of Lags/leads for Pearson correlations are supplied as a parameter.<br>

## summarize_dyads_slope()

This return the intercept and slope of a simple linear regression for
each interlocutor and the difference time series over each variable of
interest. This provides a measure of change over time, providing
information on who aligns to whom. <br>

``` r
MyFinalDataframe_AUC <- summarize_dyads_auc(MyAlignedDyads, resample = T) #resample=T computes AUC by homogenizing the length of all dyads to the shortest tramscript (number of turns) 

MyFinalDataframe_Covar <- summarize_dyads_covar(MyAlignedDyads, lags = c(-2, -1, 1, 2)) # lags are supplied as a vector, defaulting to a range of [-3, 3]

MyFinalDataframe_Slope <- summarize_dyads_slope(MyAlignedDyads, resample = F) # for auc and slope, when resample=F, it becomes much more difficult to compare metrics between conversations of different lengths.
```

# Caveat emptor

## Things you must be careful about

Any analysis of language comes with assumptions and potential bias. For
example, there are some instances where a researcher might care about
morphemes and grammatical elements such as ‘the’, ‘a’, ‘and’, etc.. The
default for ConversationAlign is to omit these as stopwords and to
average across all open class words (e.g., nouns, verbs) in each turn by
interlocutor. There are some specific cases where this can all go wrong.
Here’s what you need to consider: <br/>

1.  <span style="color:red;">Stopwords </span>: The package omits
    stopwords. [CLICK HERE](https://osf.io/atf5q/) to inspect the list.
    We included greetings, idioms, filler words, numerals, and pronouns.
    <br>

2.  <span style="color:red;">Lemmatization </span>: The package will
    lemmatize your language transcripts by default. Lemmatization
    transforms inflected forms (e.g., standing, stands) into their root
    or dictionary entry (e.g., stand). This helps for yoking offline
    values (e.g., happiness, concreteness) to each word and also entails
    what NLP folks refer to as ‘term aggregation’. However, sometimes
    you might NOT want to lemmatize. You can easily change this option
    by using the argument, “lemmatize=FALSE,” to the clean_dyads
    function below. <br>

3.  <span style="color:red;">Sample Size Issue 1: exchange count</span>:
    The program derives correlations and AUC for each dyad as metrics of
    alignment. If there are 40 exchanges (80 turns) between conversation
    partners, the R value will be computed over 40 data points. For
    conversations less than about 30 turns, you should not trust the R
    values that ConversationAlign outputs. <br>

4.  <span style="color:red;">Sample Size Issue 2 </span>: matching to
    lookup database: ConversationAlign works by yoking values from a
    lookup database to each word in your language transcript. Some
    variables have lots of values characterizing many English words.
    Other variables (e.g., age of acquisition) only cover about 30k
    words. When a word in your transcript does not have a ‘match’ in the
    lookup datase, ConversationAlign will return an NA which will not go
    into the average of the words for that interlocutor and turn. This
    can be dangerous when there are many missing values. Beware! <br>

5.  <span style="color:red;">Compositionality </span>: ConversationAlign
    is a caveman in its complexity. It matches a value to each word as
    if that word is an island. Phenomena like polysemy (e.g., bank) and
    the modulation of one word by an intensifier (e.g., very terrible)
    are not handled. This is a problem for many of the affective
    measures but not for lexical variables like word length. <br>

6.  <span style="color:red;"> Resampling for AUC </span>:
    summarize_dyads will output AUC values quantifying the distance
    between interlocutors on any dimension you specify. AUC will vary
    depending on the length of the dyad. Therefore, it is often
    necessary to resample (downsample) your dyads so that they are all
    of an equivalent length. This will get very wonky and
    uninterpretable for very short exchanges.

# Background

Here are some documents describing how we derived our stopword list and
lookup databases. <br>

1.  **Preprint** <br> Our PsyArXiv preprint describing the method(s) in
    greater detail is referenced as: Sacks, B., Ulichney, V., Duncan,
    A., Helion, C., Weinstein, S., Giovannetti, T., … Reilly, J. (2025,
    March 12). *ConversationAlign: Open-Source Software for Analyzing
    Patterns of Lexical Use and Alignment in Conversation Transcripts*.
    [Click Here](https://osf.io/preprints/psyarxiv/7xqnp_v1) to read our
    preprint. It was recently invited for revision at Behavior Rsearch
    Methods. We will update when/if eventually accepted there! <br>

2.  **Methods for creating internal lookup database** <br>
    ConversationAlign contains a large, internal lexical
    lookup_database. [Click
    Here](https://reilly-lab.github.io/ConversationAlign_LookupDatabaseCreation.html)
    to see how we created this by merging other offline psycholinguistic
    databases into one. <br>

3.  **Variable Key for ConversationAlign** <br> ConversationAlign
    currently allows users to compute alignment dynamics across 30
    different lexical, affective, and semantic dimensions.[Click
    Here](https://reilly-lab.github.io/ConversationAlign_VariableLookupKey.pdf)
    to link to a variable key. <br>

4.  **Stopwords** <br> ConversationAlign includes options for two
    different stopword lists, MIT and Temple. We generated the Temple
    stopword list after noticing that the MIT stopword list contains
    many open class words that are really NOT stopwords (at least by
    their POS tags). [Click
    Here](https://reilly-lab.github.io/StopwordListRevision_Apr25.html)
    for a description of the Temple stopword list and a discussion of
    why you should be VERY careful if you do decide to use the MIT
    stopword list. <br>

# Get in touch!

Contact <jamie_reilly@temple.edu> for feedback and assistance.
