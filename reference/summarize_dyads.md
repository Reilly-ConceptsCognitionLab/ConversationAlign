# summarize_dyads

Calculates and appends 3 measures for quantifying alignment. Appends the
averaged value for each selected dimension by turn and speaker.
Calculates and Spearman's rank correlation between interlocutor time
series and appends by transcript. Calculates the area under the curve of
the absolute difference time series between interlocutor time series.
The length of the difference time series can be standardized the
shortest number of exchanges present in the group using an internally
defined resampling function, called with resample = TRUE. Spearman's
rank correlation and area under the curve become less reliable for dyads
under 30 exchanges.

## Usage

``` r
summarize_dyads(
  df_prep,
  custom_lags = NULL,
  sumdat_only = TRUE,
  corr_type = "Pearson"
)
```

## Arguments

- df_prep:

  produced in the align_dyads function

- custom_lags:

  integer vector, should any lags be added in addition to -2, 0, 2

- sumdat_only:

  default=TRUE, group and summarize data, two rows per conversation, one
  row for each participant, false will fill down summary statistics
  across all exchanges

- corr_type:

  option for computing lagged correlations turn-by-turn covariance
  (default='Pearson')

## Value

either: - a grouped dataframe with summary data aggregated by
converation (Event_ID) and participant if sumdat_only=T. - the origoinal
dataframe 'filled down' with summary data (e.g., AUC, turn-by-turn
correlations) for each conversation is sumdat_only=F.
