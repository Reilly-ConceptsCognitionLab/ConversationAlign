
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ConversationAlign

<!-- badges: start -->
<!-- badges: end -->

## Overview

<figure>
<img src="man/figures/convo.jpeg" height="300"
alt="two people conversing" />
<figcaption aria-hidden="true">two people conversing</figcaption>
</figure>

ConversationAlign is an R package for analyzing alignment between
interlocutors (conversation partners) engaged in dyadic conversations
(two-person conversations). ConversationAlign will work on one or more
natural language transcripts of the following file types: txt (text
files), csv (comma separated values files), otter.ai transcripts saved
as txt files.

## Last updated: 2023-10-18

*Please note that ConversationAlign is currently still in development.
Please use with caution for now.*

## Nuts and bolts

<figure>
<img src="man/figures/overview.png" height="500"
alt="overview of ConversationAlign" />
<figcaption aria-hidden="true">overview of
ConversationAlign</figcaption>
</figure>

ConversationAlign transforms raw language data into simultaneous time
series objects spanning 40 possible dimensions (affective, lexical, and
semantic psycholinguistic variables). Here’s an overview of how the
package transforms language to time series data for computing alignment
indices. <br/>

## Caveats: Things you must be careful about!

Any analysis of language transcripts comes with assumptions and
potential bias. For example, there are some instances where a researcher
might care about morphemes and grammatical elements such as ‘the’, ‘a’,
‘and’, etc.. The default for ConversationAlign is to omit these as
stopwords and to average across all open class words (e.g., nouns,
verbs) in each turn by interlocutor. There are some specific cases where
this can all go wrong. Here’s what you need to consider: <br/>

1.  Stopwords: The package omits stopwords. [See the stopword list
    here](https://osf.io/atf5q/) if you would like to inspect this list.
    We included greetings, idioms, filler words, numerals, and pronouns
    in the omissions list. <br/>

2.  Lemmatization: The package will lemmatize your language transcripts
    by default. Lemmatization transforms inflected forms (e.g.,
    standing, stands) into their root or dictionary entry (e.g., stand).
    This helps for yoking offline values (e.g., happiness, concreteness)
    to each word and also entails what NLP folks refer to as ‘term
    aggregation’. However, sometimes you might NOT want to lemmatize.
    You can easily change this option by using the argument,
    “lemmatize=FALSE,” to the clean_dyads function below. <br/>

3.  Sample Size Issue 1– exchange count: The program derives
    correlations and AUC for each dyad as metrics of alignment. If there
    are 40 exchanges (80 turns) between conversation partners, the R
    value will be computed over 40 data points. For conversations less
    than about 30 turns, you should not trust the R values that
    ConversationAlign outputs. <br/>

4.  Sample Size Issue 2 – matching to lookup database: ConversationAlign
    works by yoking values from a lookup database to each word in your
    language transcript. Some variables have lots of values
    characterizing many English words. Other variables (e.g., age of
    acquisition) only cover about 13k words. When a word in your
    transcript does not have a ‘match’ in the lookup datase,
    ConversationAlign will return an NA which will not go into the
    average of the words for that interlocutor and turn. This can be
    dangerous when there are many missing values. Beware! <br/>

5.  Insensitivity to surrounding words: ConversationAlign is a caveman
    in its complexity. It matches a value to each word as if that word
    is an island. Phenomena like polysemy (e.g., bank) and the
    modulation of one word by an intensifier (e.g., very terrible) are
    not handled. This is a problem for many of the affective measures
    but not for lexical variables like word length. <br/>

6.  Interpretation (particularly for affective measures): As noted
    prior, ConversationAlign works by yoking values from a lookup
    database to each word in your language transcript. Most affective
    measures come from the database, AffectVec (Raji & deMelo, 2020,
    DOI: 10.1145/3366423.3380068). In general terms, the AffectVec is a
    database created using machine learning techniques that generate
    word embeddings, which are vector representations of words in
    continuous vector space, based on their co-occurrence patterns. As
    such, ConversationAlign may provide you with affective values that
    may not be consistent with human ratings of affective intensity in a
    provided text. It is worth carefully considering and reading about
    methods used to create the AffectVec database prior to interpreting
    any of these affective measures, particularly in scientific
    research. <br/>

7.  Potential for reflection of human bias: As with many other machine
    learning/artificial intelligence techniques, the methods used to
    create the psycholinguistic database measures which comprise the
    ConversationAlign lookup database may generate word embedding values
    that reflect human biases in their calculation (see Caliskan,
    Bryson, & Narayanan, 2017, DOI: 10.1126/science.aal4230 and Hovy &
    Prabhumoye, 2021, DOI: 10.1111/lnc3.12432 to read more). <br/>

## Installation

Install the development version of ConversationAlign from
[GitHub](https://github.com/) by entering the following in your console
or script:

``` r
install.packages("devtools")
devtools::install_github("Reilly-ConceptsCognitionLab/ConversationAlign")
```

<br/>

# Before you start

ConversationAlign takes natural language transcripts between pairs of
interlocutors. The package can handle Otter.ai formatted files, but it
can also handle a home brew of your own preferred format as long as your
transcript has at least two columns containing (1) identifiers for the
interlocutors in the interaction (e.g., speaker names, random
participant IDs) and (2) raw text from their interaction. The order of
these two columns in your transcript does not matter.<br/>

Some users may be interested in analyzing distinguishable dyads, wherein
members of interlocutor pairs are consistently differentiated by a
meaningful characteristic (e.g., student-teacher, parent-child,
boss-employee, etc.). Dyads are considered distinguishable if their
members can *all* be differentiated in this way, but are considered
indistinguishable if interlocutors in at least one dyad share this
characteristic (e.g., parent-parent). If you do have a distinguishable
grouping variable, you can either prepare a separate file with other
metadata (e.g., grouping variable, age, neuropsychological assessment
scores) and merge it with your finalized dataframe processed through
ConversationAlign (this will be prompted at the align_dyads step), or
you can include it in your original raw transcripts and it will be
retained. <br/>

## Prep your language transcripts

ConversationAlign will work on .txt or .csv files. The first (header)
row of your transcript must designate the interlocutor (person producing
the output) and the text. When prepping your raw transcripts, be careful
to mark these columns as follows using one of these options
(case-insensitive). The order of your columns does not matter:<br/> 1)
‘Interlocutor’, ‘Speaker’, or ‘Participant’<br/> 2) ‘Text’, ‘Utterance’,
or ‘Turn’ <br/>

<img src="man/figures/example1_ts_edg.jpeg" height="200"
alt="Example of input transcripts from Taylor Swift-Ellen DeGeneres Interview, 2013" />
<br/>

If working with multiple transcripts, each dyadic conversation
transcript should be saved as a separate file (e.g.,
MaryJoe_FirstDateTalk.txt and MaryJoe_SecondDateTalk.txt). This is
important for how ConversationAlign will read your data into R, append
document IDs, and split each dyad into a separate list.
ConversationAlign runs many operations on each dyad and ultimately binds
those data into a summary dataframe. ConversationAlign marks each
conversation with a unique event_id populated from its filename (be
deliberate about naming!). All other metadata (e.g., age, timestamps,
grouping variables) in your language transcripts will be retained. <br/>
<br/>

Move all your raw transcripts into the same folder. The default folder
name ConversationAlign will search for on your machine is
‘my_transcripts’. However, if you want to specify your own folder name
that’s fine too. You will call that path as an argument to the first
function called read_dyads(). <br/> <br/>

# Read your transcripts into R

## read_dyads()

This function will read all your files and concatenate them into a
single dataframe, appending document IDs. You can call this dataframe
whatever you like. read_dyads will default to reading all csv and txt
files in a folder called my_transcripts. Just remember that when you are
finished processing a set of transcripts, make sure to move them out of
that folder. You can think of ‘my_transcripts’ as a staging area for
loading data into ConversationAlign. <br/>

<br/>

``` r
MyRawLangSamples <- read_dyads()
#if you want to specify a different folder, supply your own path
MyRawLangSamples <- read_dyads("/my_custompath")
```

<br/>

<img src="man/figures/example2_ts_edg.jpeg" height="200"
alt="Example of read transcripts from Taylor Swift-Ellen DeGeneres Interview, 2013" />
<br/>

# Clean your transcripts

## clean_dyads()

‘clean_dyads’ uses regular expressions to clean and format your data.
Specifically, the function puts all words into lowercase, replaces tick
marks with apostrophes for contractions, replaces contractions with
complete words (e.g., “can’t” becomes “cannot”), replaces hyphens with
spaces, omits numeric characters (e.g., “3rd”, “5th”), and removes
extraneous white space (e.g., ” ” becomes ” “). The function also omits
stopwords using a custom stopword list, and it lemmatizes (converts all
words to their dictionary entries, so”standing”, “stood”, “stands”, and
“stand” all become “stand”) unless you specify otherwise (lemmatize=TRUE
is the default). Run ‘clean_dyads’ on the object you just assembled by
running the ‘read_dyads’ function in the last step. The clean_dyads
function also provides metrics for wordlength, wordcount, and words
removed during the clean step. <br/>

<br/>

``` r
MyCleanLangSamples <- clean_dyads(MyRawLangSamples) #default is lemmatize=TRUE
#If you do NOT want your language sample lemmatized, change the lemmatize argument to F or FALSE
MyCleanLangSamples <- clean_dyads(MyRawLangSamples, lemmatize=FALSE)
```

<br/>

<img src="man/figures/example3_ts_edg.jpeg" height="200"
alt="Example of cleaned transcripts from Taylor Swift-Ellen DeGeneres Interview, 2013" />
<br/>

# Align your transcripts

## align_dyads()

This is where a lot of the magic happens. align_dyads will take the
cleaned dataframe you created in the last step and yoke values to every
word by indexing a lookup database. This database was populated with
published word norms from numerous sources (AffectVec for affective
measures beginning with the prefix “aff\_”, Raji & deMelo, 2020, DOI:
10.1145/3366423.3380068; Kuperman norms and Brysbaert norms for semantic
and lexical measures respectively beginning with the prefixes “sem\_”
and “lex\_”, etc.). The “align” step yokes data to each word in the
cleaned transcript text then structures a dataframe by speaker
(“Participant_ID”), exchange (“exchangecount”), and turn (“turncount”)
across each dyad (“event_id”). <br/>

You will be prompted to select one of more variables (and up to three)
to yoke data to that will be used in later steps to compute alignment
indices. You will be shown a menu wherein you can select up to three
variables to be yoked to your text. Following the menu steps, enter the
number of each variable you would like with a space separating values
(e.g., “10 14 19”). <br/>

Here are your choices: <br/>

anger, anxiety, boredom, closeness, confusion, dominance, doubt,
empathy, encouragement, excitement, guilt, happiness, hope, hostility,
politeness, sadness, stress, surprise, trust, valence, age of
acquisition, word length (by letters), morphemes per turn, prevalence
(how many people know this word), number of word senses (polysemy), word
frequency (lg10), arousal, concreteness, semantic diversity, and
semantic neighbors. <br/>

After you select the variables you would like yoked to your text, you
will be asked whether there is any metadata you would like yoked to your
data You can supply the filepath of a csv file containing metadata that
will be merged to the aligned data at this juncture, or click “Enter” to
skip this step. <br/>

Run align_dyads on the cleaned dyads object you created using the
clean_dyads function.<br/>

<br/>

``` r
MyAlignedDyads <- align_dyads(MyCleanLangSamples)
```

<br/>

<img src="man/figures/example4_ts_edg.jpeg" height="200"
alt="Example of aligned transcripts with metics for anger, anxiety, and boredom (via AffectVec) from Taylor Swift-Ellen DeGeneres Interview, 2013" />
<br/>

# Summarize transcripts

## summarize_dyads()

Lastly, using the object produced by the prior align step, you can
obtain metrics of linguistic alignment between interlocutors on
variables yoked to your text during “align”. From this step, you will
gain metrics for (1) means for each variable per interlocutor per turn
(beginning with prefix “mean\_”), (2) the area under the curve
(beginning with prefix “auc\_”) per variable per dyad per exchange, and
(3) Spearman’s correlation coefficient (beginning with prefix
(“S_rho\_”) per variable per dyad per exchange. The means are of each
variable usage are calculated per interlocutor per turn for each
dimension within each dyad, and is calculated using the “mean” function
from the “base” package in R (R Core Team, 2023). AUC denotes the area
beneath the curve of the absolute difference between speakers’
expression on each variable over exchanges during their interaction
across all exchanges within each dyad, and is calculated using the “AUC”
function from the “DescTools” package in R (Signorell et al.,
<https://andrisignorell.github.io/DescTools/>). Spearman’s correlation
coefficient provides a metric of the overall association between two
interlocutors’ scores on each variable in their language over their
exchanges, and is calculated using the “cor.test(method =”spearman”)”
function from the “stats” package in R (R Core Team, 2023). This means
that within dyads, each interlocutor will have independent means for
each variable, but both interlocutors should have the same AUC and
Spearman’s correlation coefficient as one another. <br/>

<br/>

``` r
MySummarizedDyads <- summarize_dyads(MyAlignedDyads)
```

<br/>

# Get in touch!

Contact <jamie_reilly@temple.edu> for feedback and assistance.
