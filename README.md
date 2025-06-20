
<!-- README.md is generated from README.Rmd. Please edit that file -->

<br> <br> <br>

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

# An Introduction to ConversationAlign

ConversationAlign is a tool for computing main effects and alignment
dynamics in 2-person conversation transcripts. At present,
ConversationAlign is capable of analyzing synchrony across more than 30
different affective, lexical, and semantic dimensions. Email [Jamie
Reilly](mailto:reillyj@temple.edu) to suggest more. In the demo to
follow, we will analyze a real conversation transcript included in the
package. This transcript reflects a classic interview between American
radio host, Terry Gross, and comedian, Marc Maron, first broadcast on
National Public Radio (2013). Here are the first 20 lines.

``` r
knitr::kable(head(MaronGross_2013, 10), 
             format = "pipe", 
             col.names = c("**speaker**", "**text**"))
```

| **speaker** | **text** |
|:---|:---|
| MARON | I’m a little nervous but I’ve prepared I’ve written things on a piece of paper |
| MARON | I don’t know how you prepare I could ask you that - maybe I will But this is how I prepare - I panic |
| MARON | For a while |
| GROSS | Yeah |
| MARON | And then I scramble and then I type some things up and then I handwrite things that are hard to read So I can you know challenge myself on that level during the interview |
| GROSS | Being self-defeating is always a good part of preparation |
| MARON | What is? |
| GROSS | Being self-defeating |
| MARON | Yes |
| GROSS | Self-sabotage |

Conversation is ideally a cooperative endeavor where both parties modify
the form and content of their own production to align with each other.
This phenomenon is known as alignment. People align across many, many
dimensions including word choices and emotional coloring.
`ConversationAlign` can measure both differences (e.g., Do older people
use more concrete words than younger people?) and synchrony (e.g., Do
people of similar educational attainment show higher rates of
alignment?) across many factors. In planning your analysis, we recommend
choose your factors with a directional hypothesis in mind. <br>

## Preparing Your Data

- `ConversationAlign` works **ONLY** on dyadic language transcripts
  (i.e., 2-person dialogues).
- Your raw transcript **MUST** contain at least two columns,
  interlocutor (i.e., speaker) and text.
- The order of your columns does not matter.
- Metadata will be conserved (e.g., timestamps, grouping variables,
  physio values).
- Don’t worry about stripping punctuation or splitting words across
  rows. As long as the corresponding text within each turn is marked by
  a talkerID, ConversationAlign will split text and append all rowwise
  labels.
- Label your talker/interlocutor column as ‘Interlocutor’, ‘Speaker’, or
  ‘Participant’.
- Label your text column as ‘Text’, ‘Utterance’, or ‘Turn’.
- Save each conversation transcript somwehere on your computer as a
  separate file (CSV or txt work best).
- Be careful/deliberate about your filenaming convention. The filename
  for each conversation will become its event ID (or document_id). You
  might need this when processing large corpora. <br>
- Stage all your individual conversation transcripts to be analyzed into
  one folder (e.g., “my_transcripts”). This folder should ideally by
  nested in the same directory you are running your R script in. <br>
  <br>

# Step 1: Read Transcripts into R

## read_dyads()

`read_dyads()` will import all of the conversation transcripts on your
machine’s target folder into R. The function will also concatenate all
individual transcripts into a single dataframe. Each transcript’s
filename will become its `Event_ID` in the dataframe. If you want to
skip the read step and format your dataframe manually in R, you will
need three columns named exactly: 1. `Event_ID` column header with
unique marker for each conversation transcript (variable as factor) <br>
2. `Paricipant_ID` column header delineating who is producing the text
on a given line (variable as factor) <br> 3. `RawText` column header
containing the raw text (punctuation, unsplit, uncleaned, etc,) <br>

Name your concatenated dataframe anything you like (e.g.,
MyKidConversations). `read_dyads()` defaults to scanning a folder called
`my_transcripts` within the directory you are running any scripts in.
`read_dyads()` will import all `*.csv`, `*.txt`, and Otter `*.ai` files
that exist within that folder. You can also dump your transcripts in a
folder labelled however you like by specifing a custom path. Here we
will import a conversation transcript representing a 2013 NPR interview
(USA) between Marc Maron and Terry Gross, titled [Marc Maron: A Life
Fueled By ‘Panic And
Dread’](https://www.npr.org/transcripts/179014321).<br>

<span style="color: darkred;">Arguments to `read_dyads` include:</span>
<br> 1. **folder_name**: default is ‘my_transcripts’, change path to
your folder name<br>

``` r
#Example of custom path
#MyConvo <- read_dyads(folder_name = 'mycomputer/my_lang_transcripts')
```

## read_1file()

Preps one transcript already in your R environment for
ConversationAlign. We will use read_1file() to prep the Marc Maron and
Terry Gross transcript. Look at how the column headers have changed and
the object name (MaronGross_2013) is now the Event_ID (a document
identifier), <br>

<span style="color: darkred;">Arguments to `read_1file` include:</span>
<br> 1. **my_dat**: object already in your R environment containing text
and speaker information.

``` r
Maron_Prepped <- read_1file(MaronGross_2013)

#print first ten rows of header
knitr::kable(head(Maron_Prepped, 10), format = "pipe")
```

| Event_ID | Participant_ID | RawText |
|:---|:---|:---|
| MaronGross_2013 | MARON | I’m a little nervous but I’ve prepared I’ve written things on a piece of paper |
| MaronGross_2013 | MARON | I don’t know how you prepare I could ask you that - maybe I will But this is how I prepare - I panic |
| MaronGross_2013 | MARON | For a while |
| MaronGross_2013 | GROSS | Yeah |
| MaronGross_2013 | MARON | And then I scramble and then I type some things up and then I handwrite things that are hard to read So I can you know challenge myself on that level during the interview |
| MaronGross_2013 | GROSS | Being self-defeating is always a good part of preparation |
| MaronGross_2013 | MARON | What is? |
| MaronGross_2013 | GROSS | Being self-defeating |
| MaronGross_2013 | MARON | Yes |
| MaronGross_2013 | GROSS | Self-sabotage |

# Clean your transcripts

## clean_dyads()

`clean_dyads()` uses numerous regex to clean and format the data your
just read into R in the previous step. `ConversationAlign` applies an
ordered sequence of cleaning steps beginning with transforming all of
your raw text to lowercase. The package eventually splits the text into
a one-word-per row format after a number of transformations, including:
replace contractions (e.g., ‘you’re’ to ‘you are’), gsub tick marks to
apostrophes, gsub hypens to spaces, omit numerals, omit non-alphabetic
characters, squish extraneous white space. There are two additional
arguments you need to be VERY careful about (i.e.,lemmatization and
stopword removal). Let’s briefly review these options. <br>

**Stopwords:** Many NLP and computer science researchers consider
stopwords (e.g., the, a, I, you) as semantically empty indices. This
view contrasts with linguists and people in the know about language who
view closed class words as integral for building meaning. Nevertheless,
the decision to omit stopwords has merit. Words such as ‘is’, ‘the’, ‘a’
have overwhelming lexical frequency and will tend to dominate your
document term matrix. Most of us are more interested in open-class words
(e.g., nouns, adjectives, verbs). For algorithms such as
ConversationAlign that treat language as a continuous bag-of-words,
stopword removal is often essential. Be careful about the strategy you
use to remove stopwords. This procedure typically involves matching
lookup databases that are composed of fixed lists of stopwords. Some of
these lists are minimalist and highly conservative (e.g., only omitting
function words). Other stopword lists (e.g., MIT) are quite liberal and
include substantial numbers of open class words that you might not want
to remove. `ConversationAlign` includes several options for stopword
omission, including: <br> <br> 1) **None** - leave my text alone! <br>
2) **SMART** (english) - for provenance of original source [CLICK
HERE](https://search.r-project.org/CRAN/refmans/stopwords/html/data_stopwords_smart.html).
To inspect the actual stopwords [CLICK HERE](https://osf.io/4jhza) <br>
3) **MIT_Stops** - from MedialLab. To inspect the actual stopword list
[CLICK
HERE](https://web.media.mit.edu/~lieber/Teaching/Common-Sense-Course/Stop-Words.Text)
<br> 4) **Temple_Stopwords25** - from our lab. We very carefully
constructed this stopword list after tagging POS from the MIT list and
then omitting open-class words. We also included idioms and greetings
(multiword utterances). ConversationAlign’s default argument for is
omitting stopwords is to apply the `Temple_Stopwords25` list. We
recommend familiarizing yourself with the characteristics of this list
by [clicking here to read about its
construction](https://reilly-lab.github.io/Jamie_Stopwords.html) and
[clicking here to inspect the list itself](https://osf.io/dc5k7). <br>
<br>

**Lemmatization:** `ConversationAlign` calls the `textstem` package as a
dependency to lemmatize your language transcript. This converts
morphologiocal derivatives to their root forms. The default is
lemmatize=T. Sometimes you want to retain language output in its native
form. If this is the case, change the argument in clean_dyads to
lemmatize=F. `clean_dyads()` outputs word count metrics pre/post
cleaning by dyad and interlocutor. This can be useful if you are
interested in whether one person just doesn’t produce many words or
produces a great deal of empty utterances. <br>

<span style="color: darkred;">Arguments to `clean_dyads` include:</span>
<br> 1) **read_ts_df** = name of the dataframe created during
`read_dyads()` <br> 2) **which_stopwords** = quoted argument specifying
stopword list, options include `none`, `MIT_stops`, `SMART`,
`CA_OriginalStops`, or `Temple_Stopwords25`. Default is
`Temple_Stopwords25`. `CA_OriginalStops` is the stopword list originally
bundled with `ConversationAlign` during its early development. <br> 3)
**lemmatize** = lemmatize strings converting each entry to its
dictionary form, default is TRUE

``` r
#defaults to function call are 'Temple_Stopwords25' and lemmatize=TRUE
Maron_Cleaned <- clean_dyads(read_ts_df=Maron_Prepped, which_stoplist = "Temple_Stopwords25", lemmatize=TRUE) 
knitr::kable(head(Maron_Cleaned, 15), format = "pipe")
```

# Align your transcripts

## align_dyads()

This is where the magic happens. `align_dyads()` will take the cleaned
dataframe you created in the last step and yoke values to every word by
indexing a lookup database. The `align_dyads()` step yokes data to each
word in the cleaned transcript text then structures a dataframe by
speaker (“Participant_ID”), exchange (“exchangecount”), and turn
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

Any analysis of language comes with assumptions and potential bias. For
example, there are some instances where a researcher might care about
morphemes and grammatical elements such as ‘the’, ‘a’, ‘and’, etc.. The
default for ConversationAlign is to omit these as stopwords and to
average across all open class words (e.g., nouns, verbs) in each turn by
interlocutor. There are some specific cases where this can all go wrong.
Here are some things to consider: <br>

1.  <span style="color:red;">Stopwords </span>: `ConversationAlign`
    omits stopwords by default applying a customized stopword list,
    `Temple_Stopwords25`. [CLICK HERE](https://osf.io/dc5k7) to inspect
    the list. This stopword list includes greetings, idioms, filler
    words, numerals, and pronouns.

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

# Get in touch!

Contact <jamie_reilly@temple.edu> for feedback and assistance.

# References

Lewis, David D., et al. (2004) “Rcv1: A new benchmark collection for
text categorization research.” Journal of machine learning research 5:
361-397.
