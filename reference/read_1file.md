# read_1file

Reads pre-formatted dyadic (2 interlocutor) conversation transcript
already imported into your R environment.

## Usage

``` r
read_1file(my_dat)
```

## Arguments

- my_dat:

  one conversation transcript already in the R environment

## Value

a dataframe formatted with 'Event_ID', "Participant_ID", "Text_Raw"
fields – ready for clean_dyads()
