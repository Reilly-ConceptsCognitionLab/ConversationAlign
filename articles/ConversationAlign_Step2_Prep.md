# ConversationAlign_Step2_Prep

## Cleaning, Formatting, Aligning Norms to Your Data

Lots of wild operations happen in this next step that transform your
unstructured text to numeric time series objects aggregated by
conversation and interlocutor. It is important that you have a handle on
what
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
does and what processes such as lemmatization and stopword removal
mean.  

[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
uses numerous regex to clean and format the data your just read into R
in the previous step. `ConversationAlign` applies an ordered sequence of
cleaning steps on the road toward vectorizing your original text into a
one-word-per row format. These steps include: converting all text to
lowercase, expanding contractions, omitting all non-alphabetic
characters (e.g., numbers, punctuation, line breaks). In addition to
text cleaning, users guide options for stopword removal and
lemmatization. During formatting
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
will prompt you to select up to three variables for computing alignment
on. This works by joining values from a large internal lookup database
to each word in your language transcript.
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
is customizable via the following arguments.

#### Stopword removal

There are two important arguments regarding stopword removal.
`omit_stops` specifies whether or not to remove stopwords.
`which_stopwords` specifies which stopword list you would like to apply
with the default being `Temple_stops25`. The full list of choices is:
`none`, `SMART_stops`, `CA_orig_stops`. `MIT_stops`, and
`Temple_stops25`. Stopword removal is an important, yet also
controversial step in text cleaning.  

#### Lemmatization

`ConversationAlign` calls the `textstem` package as a dependency to
lemmatize your language transcript. This converts morphologiocal
derivatives to their root forms. The default is lemmatize=T. Sometimes
you want to retain language output in its native form. If this is the
case, change the argument in clean_dyads to lemmatize=F. `clean_dyads()`
outputs word count metrics pre/post cleaning by dyad and interlocutor.
This can be useful if you are interested in whether one person just
doesn’t produce many words or produces a great deal of empty
utterances.  

#### Dimension Selection

This is where the magic happens.
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
will yoke published norms for \>40 possible dimensions to every content
word in your transcript (up to 3 at a time). This join is executed by
merging your vectorized conversation transcript with a huge internal
lexical database with norms spanning over 100k English words.
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
will prompt you to select anywhere from 1 to 3 target dimensions at a
time. Enter the number corresponding to each dimension of interest
separated by spaces and then hit enter (e.g., 10 14 19)
`ConversationAlign` will append a published norm if available (e.g.,
concreteness, word length) to every running word in your transcript.
These quantitative values are used in the subsequent
[`summarize_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/summarize_dyads.md)
step to compute alignment.  

## `prep_dyads()`

-Cleans, formats, and vectorizes conversation transwcripts to a
one-word-per-row format -Yokes psycholinguistic norms for up to three
dimensions at a time (from \<40 possible dimensions) to each content
word. -Retains metadata  

Arguments to `prep_dyads`:  
1) **dat_read**= name of the dataframe created during
[`read_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/read_dyads.md)  
2) **omit_stops**= T/F (default=T) option to remove stopwords 3)
**lemmatize**= lemmatize strings converting each entry to its dictionary
form, default is `lemmatize=TRUE`  
4) **which_stoplist**= quoted argument specifying stopword list, options
include `none`, `MIT_stops`, `SMART`, `CA_OriginalStops`, or
`Temple_stops25`. Default is `Temple_stops25`  
5) **remove_backchannel**= logical, should turns comprised entirely of
stopwords be removed or preserved as NAs. NAs are filled in future
steps. Defaults is `FALSE`.

``` r
#Example of running the function
NurseryRhymes_Prepped <- prep_dyads(dat_read=NurseryRhymes, lemmatize=TRUE, omit_stops=T, which_stoplist="Temple_stops25")
```

### Example of a prepped dataset

This embedded as external data in the package with ‘anger’ values yoked
to each word.

``` r
knitr::kable(head(NurseryRhymes_Prepped, 20), format = "simple", digits=2)
```

| Event_ID   | Participant_ID | Exchange_Count | Turn_Count | Text_Prep | Text_Clean | emo_anger |
|:-----------|:---------------|---------------:|-----------:|:----------|:-----------|----------:|
| ItsySpider | Yin            |              1 |          1 | the       | NA         |        NA |
| ItsySpider | Yin            |              1 |          1 | itsy      | itsy       |     -0.02 |
| ItsySpider | Yin            |              1 |          1 | bitsy     | bitsy      |     -0.02 |
| ItsySpider | Yin            |              1 |          1 | spider    | spider     |      0.04 |
| ItsySpider | Yin            |              1 |          1 | climbed   | climb      |     -0.09 |
| ItsySpider | Yin            |              1 |          1 | up        | up         |     -0.06 |
| ItsySpider | Yin            |              1 |          1 | the       | NA         |        NA |
| ItsySpider | Yin            |              1 |          1 | water     | water      |     -0.17 |
| ItsySpider | Yin            |              1 |          1 | spout     | spout      |      0.05 |
| ItsySpider | Maya           |              1 |          2 | down      | down       |      0.03 |
| ItsySpider | Maya           |              1 |          2 | came      | come       |     -0.13 |
| ItsySpider | Maya           |              1 |          2 | the       | NA         |        NA |
| ItsySpider | Maya           |              1 |          2 | rain      | rain       |      0.07 |
| ItsySpider | Maya           |              1 |          2 | and       | NA         |        NA |
| ItsySpider | Maya           |              1 |          2 | washed    | wash       |      0.06 |
| ItsySpider | Maya           |              1 |          2 | the       | NA         |        NA |
| ItsySpider | Maya           |              1 |          2 | spider    | spider     |      0.04 |
| ItsySpider | Maya           |              1 |          2 | out       | out        |      0.04 |
| ItsySpider | Yin            |              2 |          3 | out       | out        |      0.04 |
| ItsySpider | Yin            |              2 |          3 | came      | come       |     -0.13 |
