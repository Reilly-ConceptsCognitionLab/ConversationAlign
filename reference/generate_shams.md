# generate_shams

Generates a permutation of each individual dyad. Shuffled dyads may act
as controls to their originals.

## Usage

``` r
generate_shams(df_prep, seed = NULL)
```

## Arguments

- df_prep:

  Output dataframe of prep_dyads().

- seed:

  (Optional) a seed for reproducibility in random sampling

## Value

A dataframe similar to prepped dyads, with each participant's time
series randomly shuffled.
