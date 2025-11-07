# ConversationAlign_Introduction

A good conversation is a cooperative endeavor where both parties modify
the form and content of their own production to align with each other.
This is a phenomenon known as alignment. People align across many
dimensions including the words they choose and the affective tenor of
their prosody. `ConversationAlign` measures dynamics of lexical use
between conversation partners across more than 40 semantic, lexical,
phonological, and affective dimensions. Before launching into your
analyses, there are some important use caveats to consider.

### Caveats for Using ConversationAlign

- Language analyses often proliferate in complexity. Spend the extra
  time to keep careful records and a logical organization system (e.g.,
  smart filenaming, variable keys, a formalized processing pipeline).
- `ConversationAlign` only works on dyadic language transcripts (i.e.,
  2-person dialogues).
- `ConversationAlign` does NOT parse turns automatically. The software
  will aggregate all words produced by one speaker across sentences and
  rows until a switch occurs in the ‘speaker’ column.
- `ConversationAlign` will strip punctuation and other special
  characters automatically.
- `ConversationAlign` will split/vectorize your text into a
  one-word-per-row format, retaining all variable labels.
- `ConversationAlign` will retain all meta-data throughout text
  processing (e.g., timestamps, grouping variables). `ConversationAlign`
  is pretty good at detecting and repairing unconventional font encoding
  systems, but it will not catch everything, You will find all sorts of
  hidden junk when you copy/paste interview transcripts from random
  websites are from YouTube. Inspect your transcripts to make sure they
  are what you think they are before launching into a complex
  computational analysis.

### Prepare your Transcripts Outside of the Package

- Save each conversation transcript as a separate file (`*.txt`,
  `*.csv`, or Otter `*.ai`).
- Be deliberate about your filenaming convention. Each transcript’s
  filename will become its unique document identifier (Event_ID) when
  importing into `ConversationAlign`.  
- Store all the transcripts you want to analyze in the same folder
  (e.g., ’/my_transcripts).
- Your transcript folder and analysis scripts should ideally exist
  within the same directory.
- Each raw comversation transcript **MUST** nominally contain at least
  two columns, talker/interlocutor and text.  
- Name your talker/interlocutor column as ‘Interlocutor’, ‘Speaker’, or
  ‘Participant’ (not case sensitive).
- Name your text column as ‘Text’, ‘Utterance’, or ‘Turn’ (not case
  sensitive).

## Installation

Install and load the development version of `ConversationAlign` from
[GitHub](https://github.com/) using the `devtools` package.

``` r
# Load SemanticDistance
library(ConversationAlign)
```

### Calibaration Transcripts Included in `ConversationAlign`

`ConversationAlign`contains two sample conversation transcripts that are
pre-load when you call the package. These are: **MaronGross_2013:**
Interview transcript of Marc Maron and Terry Gross on NPR (2013).  
**NurseryRhymes:** Three nursery rhymes looping same phrases formatted
as conversations, cleaned, and aligned to illustrate how the formatting
pipeline reshaopes conversation transcripts.

#### NurseryRhymes

``` r
knitr::kable(head(NurseryRhymes, 20), format = "simple")
```

| Event_ID   | Participant_ID | Text_Raw                                             |
|:-----------|:---------------|:-----------------------------------------------------|
| ItsySpider | Yin            | The itsy-bitsy spider climbed up the water spout     |
| ItsySpider | Maya           | Down came the rain and washed the spider out         |
| ItsySpider | Yin            | Out came the sun, and dried up all the rain          |
| ItsySpider | Maya           | And the itsy-bitsy spider climbed up the spout again |
| ItsySpider | Yin            | The itsy-bitsy spider climbed up the water spout     |
| ItsySpider | Maya           | Down came the rain and washed the spider out         |
| ItsySpider | Yin            | Out came the sun, and dried up all the rain          |
| ItsySpider | Maya           | And the itsy-bitsy spider climbed up the spout again |
| ItsySpider | Yin            | The itsy-bitsy spider climbed up the water spout     |
| ItsySpider | Maya           | Down came the rain and washed the spider out         |
| ItsySpider | Yin            | Out came the sun, and dried up all the rain          |
| ItsySpider | Maya           | And the itsy-bitsy spider climbed up the spout again |
| ItsySpider | Yin            | The itsy-bitsy spider climbed up the water spout     |
| ItsySpider | Maya           | Down came the rain and washed the spider out         |
| ItsySpider | Yin            | Out came the sun, and dried up all the rain          |
| ItsySpider | Maya           | And the itsy-bitsy spider climbed up the spout again |
| ItsySpider | Yin            | The itsy-bitsy spider climbed up the water spout     |
| ItsySpider | Maya           | Down came the rain and washed the spider out         |
| ItsySpider | Yin            | Out came the sun, and dried up all the rain          |
| ItsySpider | Maya           | And the itsy-bitsy spider climbed up the spout again |

``` r
str(NurseryRhymes)
#> 'data.frame':    228 obs. of  3 variables:
#>  $ Event_ID      : chr  "ItsySpider" "ItsySpider" "ItsySpider" "ItsySpider" ...
#>  $ Participant_ID: chr  "Yin" "Maya" "Yin" "Maya" ...
#>  $ Text_Raw      : chr  "The itsy-bitsy spider climbed up the water spout" "Down came the rain and washed the spider out" "Out came the sun, and dried up all the rain" "And the itsy-bitsy spider climbed up the spout again" ...
```

#### Maron-Gross Interview

Here’s one from a 2013 NPR interview (USA) between Marc Maron and Terry
Gross, titled [Marc Maron: A Life Fueled By ‘Panic And
Dread’](https://www.npr.org/transcripts/179014321).  

``` r
knitr::kable(head(MaronGross_2013, 20), format = "simple")
```

| speaker | text                                                                                                                                                                       |
|:--------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| MARON   | I’m a little nervous but I’ve prepared I’ve written things on a piece of paper                                                                                             |
| MARON   | I don’t know how you prepare I could ask you that - maybe I will But this is how I prepare - I panic                                                                       |
| MARON   | For a while                                                                                                                                                                |
| GROSS   | Yeah                                                                                                                                                                       |
| MARON   | And then I scramble and then I type some things up and then I handwrite things that are hard to read So I can you know challenge myself on that level during the interview |
| GROSS   | Being self-defeating is always a good part of preparation                                                                                                                  |
| MARON   | What is?                                                                                                                                                                   |
| GROSS   | Being self-defeating                                                                                                                                                       |
| MARON   | Yes                                                                                                                                                                        |
| GROSS   | Self-sabotage                                                                                                                                                              |
| MARON   | Yes                                                                                                                                                                        |
| GROSS   | Key                                                                                                                                                                        |
| MARON   | Right so you do that?                                                                                                                                                      |
| GROSS   | I sometimes do that                                                                                                                                                        |
| MARON   | How often?                                                                                                                                                                 |
| GROSS   | I try not to do that I do that more in life than I do in radio                                                                                                             |
| MARON   | Really?                                                                                                                                                                    |
| GROSS   | Yeah                                                                                                                                                                       |
| MARON   | Like today?                                                                                                                                                                |
| GROSS   | Life is harder than radio                                                                                                                                                  |

``` r
str(MaronGross_2013)
#> 'data.frame':    546 obs. of  2 variables:
#>  $ speaker: chr  "MARON" "MARON" "MARON" "GROSS" ...
#>  $ text   : chr  " I'm a little nervous but I've prepared I've written things on a piece of paper" " I don't know how you prepare I could ask you that - maybe I will But this is how I prepare - I panic" " For a while" " Yeah" ...
```

## Caveat emptor

Any analysis of language comes with assumptions and potential bias. For
example, there are some instances where a researcher might care about
morphemes and grammatical elements such as ‘the’, ‘a’, ‘and’, etc.. The
default for ConversationAlign is to omit these as stopwords and to
average across all open class words (e.g., nouns, verbs) in each turn by
interlocutor. There are some specific cases where this can all go wrong.
Here are some things to consider:  

1.  Stopwords : `ConversationAlign` omits stopwords by default applying
    a customized stopword list, `Temple_Stopwords25`. [CLICK
    HERE](https://osf.io/dc5k7) to inspect the list. This stopword list
    includes greetings, idioms, filler words, numerals, and pronouns.

2.  Lemmatization : The package will lemmatize your language transcripts
    by default. Lemmatization transforms inflected forms (e.g.,
    standing, stands) into their root or dictionary entry (e.g., stand).
    This helps for yoking offline values (e.g., happiness, concreteness)
    to each word and also entails what NLP folks refer to as ‘term
    aggregation’. However, sometimes you might NOT want to lemmatize.
    You can easily change this option by using the argument,
    “lemmatize=FALSE,” to the clean_dyads function below.  

3.  Sample Size Issue 1: Exchange Count: The program derives
    correlations and AUC for each dyad as metrics of alignment. For very
    brief conversations (\<30 turns), the likelihood of unstable or
    unreliable estimates is high.  

4.  Sample Size Issue 2 : matching to lookup database: ConversationAlign
    works by yoking values from a lookup database to each word in your
    language transcript. Some variables have lots of values
    characterizing many English words. Other variables (e.g., age of
    acquisition) only cover about 30k words. When a word in your
    transcript does not have a ‘match’ in the lookup datase,
    ConversationAlign will return an NA which will not go into the
    average of the words for that interlocutor and turn. This can be
    dangerous when there are many missing values. Beware!  

5.  Compositionality : ConversationAlign is a caveman in its complexity.
    It matches a value to each word as if that word is an island.
    Phenomena like polysemy (e.g., bank) and the modulation of one word
    by an intensifier (e.g., very terrible) are not handled. This is a
    problem for many of the affective measures but not for lexical
    variables like word length.  

## Background and Supporting Materials

1.  **Preprint**  
    Our PsyArXiv preprint describing the method(s) in greater detail is
    referenced as: Sacks, B., Ulichney, V., Duncan, A., Helion, C.,
    Weinstein, S., Giovannetti, T., … Reilly, J. (2025, March 12).
    *ConversationAlign: Open-Source Software for Analyzing Patterns of
    Lexical Use and Alignment in Conversation Transcripts*. [Click
    Here](https://osf.io/preprints/psyarxiv/7xqnp_v1) to read our
    preprint. It was recently invited for revision at Behavior Rsearch
    Methods. We will update when/if eventually accepted there!  

2.  **Methods for creating internal lookup database**  
    ConversationAlign contains a large, internal lexical
    lookup_database. [Click
    Here](https://reilly-lab.github.io/ConversationAlign_LookupDatabaseCreation.html)
    to see how we created this by merging other offline psycholinguistic
    databases into one.  

3.  **Variable Key for ConversationAlign**  
    ConversationAlign currently allows users to compute alignment
    dynamics across \>40 different lexical, affective, and semantic
    dimensions.[Click
    Here](https://reilly-lab.github.io/ConversationAlign_VariableLookupKey.pdf)
    to link to a variable key.  

## References

Lewis, David D., et al. (2004) “Rcv1: A new benchmark collection for
text categorization research.” Journal of machine learning research 5:
361-397.
