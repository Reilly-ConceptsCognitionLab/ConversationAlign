# prep_dyads

Cleans, vectorizes and appends lexical norms to all content words in a
language corpus. User guides options for stopword removal and
lemmatization. User selects up to three psycholinguistic dimensions to
yoke norms on each content word in the original conversation transcript.

## Usage

``` r
prep_dyads(
  dat_read,
  lemmatize = TRUE,
  omit_stops = TRUE,
  which_stoplist = "Temple_stops25",
  remove_backchannel = FALSE,
  verbose = TRUE
)
```

## Arguments

- dat_read:

  dataframe produced from read_dyads() function

- lemmatize:

  logical, should words be lemmatized (switched to base morphological
  form), default is TRUE

- omit_stops:

  option to remove stopwords, default TRUE

- which_stoplist:

  user-specified stopword removal method with options including "none",
  "SMART", "MIT_stops", "CA_OriginalStops", or "Temple_Stopwords25".
  "Temple_Stopwords25 is the default list

- remove_backchannel:

  logical, should turns that are full of stopwords (e.g., "Uhm yeah") be
  preserved as NAs or removed. Removal will 'squish' the turn before and
  after together into one. If NAs are preserved they are later
  interpolated.

- verbose:

  display detailed output such as error messages and progress (default
  is TRUE)

## Value

dataframe with text cleaned and vectorized to a one word per-row format.
Lexical norms and metadata are appended to each content word. Cleaned
text appears under a new column called 'Text_Clean'. Any selected
dimensions (e.g., word length) and metadata are also appended to each
word along with speaker identity, turn, and Event_ID (conversation
identifier).
