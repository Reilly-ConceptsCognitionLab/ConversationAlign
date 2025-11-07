# ConversationAlign_Step3_Summarize

This is the final step where `ConversationAlign` will compute summary
statistics including main effects and alignment statistics for the
vectorized dataframe you produced using
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md).
Users have several options for how to output their data, and these
choices should be guided by your analysis strategy. For example, a
linear mixed effects approach might involve modeling the rise and fall
of values across turns. In contrast, a standard ANOVA would work on
grouped summary data.

- Main effects for each dimension of interest aggregated by Conversation
  (Event_ID) and Person (Participant_ID). For example, main effects of
  concreteness would involve aggregated means of all the concreteness
  values for words produced by Mary vs words produced by Dave in an
  individual conversation.
- dAUC_raw: difference area under the curve reflecting the difference
  between interlocutors at each turn on each dimension uncorrected for
  conversation length. For example, if Mary’s concreteness at Exchange 1
  was 8 (on a scale of 0-9), and Dave’s concreteness on Exchange 1 was
  4, the difference between Mary and Dave in this one-turn conversation
  would be 4. dAUC reflects area across all turns estimated using the
  trapezoidal rule.
- dAUC_scaled100: normalized AUC value to 100 turns using proportional
  scaling. e.g., (Observed AUC/Turns Raw) = (Normalized AUC)/100).
- Lead_Corr2: Pearson or Spearman lagged correlation reflecting
  turn-by-turn covariance across partners for each specified dimension
- Lag_Corr2: Lead correlation
- Who_Talked_First: Interlocutor who started the conversation (needed
  for interepreting lead/lag stats)

Arguments to
[`summarize_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/summarize_dyads.md)
include:  
1) **df_prep**= dataframe created by
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)function  
2) **custom_lags**= default is NULL, any additional user-specified
lagged correlations. will automatically produce lead of 2 turns,
immediate response, lag of 2 turns for each dimension of interest.  
3) **sumdat_only**= boolean default is TRUE, produces grouped summary
dataframe with averages by conversation and participant for each
alignment dimension, FALSE retrains all of the original rows, filling
down empty rows of summary statistics for the conversation (e.g., AUC)  
4) **corr_type**= default=‘Pearson’, other option ‘Spearman’ for
computing turn-by-turn correlations across interlocutors for each
dimension of interest.

``` r
MarySumDat <- summarize_dyads(df_prep = NurseryRhymes_Prepped, custom_lags=NULL, sumdat_only = TRUE, corr_type='Pearson') 
colnames(MarySumDat)
#>  [1] "Event_ID"               "Participant_ID"         "Talked_First"          
#>  [4] "Dimension"              "Dimension_Mean"         "AUC_raw_Immediate"     
#>  [7] "AUC_scaled50_Immediate" "AUC_raw_Lag1"           "AUC_scaled50_Lag1"     
#> [10] "TurnCorr_Lead2"         "TurnCorr_Immediate"     "TurnCorr_Lag2"
knitr::kable(head(MarySumDat, 15), format = "simple", digits = 3)
```

| Event_ID   | Participant_ID | Talked_First | Dimension | Dimension_Mean | AUC_raw_Immediate | AUC_scaled50_Immediate | AUC_raw_Lag1 | AUC_scaled50_Lag1 | TurnCorr_Lead2 | TurnCorr_Immediate | TurnCorr_Lag2 |
|:-----------|:---------------|:-------------|:----------|---------------:|------------------:|-----------------------:|-------------:|------------------:|---------------:|-------------------:|--------------:|
| ItsySpider | Maya           | Yin          | emo_anger |          0.001 |             0.783 |                  1.630 |        0.749 |             1.560 |             -1 |                 -1 |            -1 |
| ItsySpider | Yin            | Yin          | emo_anger |         -0.033 |             0.783 |                  1.630 |        0.749 |             1.560 |             -1 |                 -1 |            -1 |
| JackJill   | Ana            | Franklin     | emo_anger |         -0.066 |             3.729 |                  4.662 |        3.634 |             4.542 |              1 |                  1 |             1 |
| JackJill   | Franklin       | Franklin     | emo_anger |          0.030 |             3.729 |                  4.662 |        3.634 |             4.542 |              1 |                  1 |             1 |
| LittleLamb | Dave           | Mary         | emo_anger |         -0.001 |             1.486 |                  1.486 |        1.456 |             1.456 |             NA |                 NA |            NA |
| LittleLamb | Mary           | Mary         | emo_anger |         -0.031 |             1.486 |                  1.486 |        1.456 |             1.456 |             NA |                 NA |            NA |

## Generating sham conversations

Some research questions would benefit from the use of conversations that
control for some temporal effects. The function
[`generate_shams()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/generate_shams.md)
accepts the output of
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
and returns a data frame in the same structure with each interlocutor’s
time series randomly shuffled. Since the output has the same format as
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)
output, it can easily be supplied to
[`summarize_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/summarize_dyads.md)
and compared to the real conversations.

Arguments to
[`generate_shams()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/generate_shams.md)
include:  
1) **df_prep**= dataframe created by
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)function  
2) **seed**= numeric, a number to supply as a seed. This allows for
reproducible results.  

``` r
MaryShams <- generate_shams(df_prep = NurseryRhymes_Prepped, seed = 10)
MarySumDatShams <- summarize_dyads(df_prep = MaryShams, custom_lags=NULL, sumdat_only = TRUE, corr_type='Pearson') 
knitr::kable(head(MarySumDatShams, 15), format = "simple", digits = 3)
```

| Event_ID   | Participant_ID | Talked_First | Dimension | Dimension_Mean | AUC_raw_Immediate | AUC_scaled50_Immediate | AUC_raw_Lag1 | AUC_scaled50_Lag1 | TurnCorr_Lead2 | TurnCorr_Immediate | TurnCorr_Lag2 |
|:-----------|:---------------|:-------------|:----------|---------------:|------------------:|-----------------------:|-------------:|------------------:|---------------:|-------------------:|--------------:|
| ItsySpider | Maya           | Maya         | emo_anger |          0.002 |             0.759 |                  1.580 |        0.701 |             1.459 |          0.100 |                 -1 |         0.100 |
| ItsySpider | Yin            | Maya         | emo_anger |         -0.032 |             0.759 |                  1.580 |        0.701 |             1.459 |          0.100 |                 -1 |         0.100 |
| JackJill   | Ana            | Ana          | emo_anger |         -0.066 |             3.729 |                  4.662 |        3.634 |             4.542 |         -0.158 |                  1 |        -0.158 |
| JackJill   | Franklin       | Ana          | emo_anger |          0.030 |             3.729 |                  4.662 |        3.634 |             4.542 |         -0.158 |                  1 |        -0.158 |
| LittleLamb | Dave           | Dave         | emo_anger |         -0.001 |             1.486 |                  1.486 |        1.456 |             1.456 |             NA |                 NA |            NA |
| LittleLamb | Mary           | Dave         | emo_anger |         -0.031 |             1.486 |                  1.486 |        1.456 |             1.456 |             NA |                 NA |            NA |
