# corpus_analytics

Produces a table of corpus analytics including numbers of complete
observations at each step, word counts, lexical diversity (e.g., TTR),
stopword ratios, etc. Granularity of the summary statistics are guided
by the user (e.g., by conversation, by conversation and speaker,
collapsed all)

## Usage

``` r
corpus_analytics(dat_prep)
```

## Arguments

- dat_prep:

  takes dataframe produced from the df_prep() function

## Value

dataframe with summary statistics (mean, SD, range) for numerous corpus
analytics (e.g., token count, type-token-ratio, word-count-per-turn) for
the target conversation corpus. Summary data structured in table format
for easy export to a journal method section.
