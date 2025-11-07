# ConversationAlign_Step4_Analytics

This is a helpful addition to `ConversationAlign` that will generate a
variety of corpus analytics (e.g., word count, type-token-ratio) for
your conversation corpus. The output is in a summary table that is
readily exportable to to the specific journal format of your choice
using any number of packages such as `flextable` or `tinytable`.  

Generate your corpus analytics on the dataframe you created with
`prep_dyads`.

Arguments to `corpus_analytics` include:  
1) **dat_prep**= dataframe created by
[`prep_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/prep_dyads.md)function  

``` r
NurseryRhymes_Analytics <-  corpus_analytics(dat_prep=NurseryRhymes_Prepped)
knitr::kable(head(NurseryRhymes_Analytics, 15), format = "simple", digits = 2)
```

| measure                                       |    mean | stdev |    min |    max |
|:----------------------------------------------|--------:|------:|-------:|-------:|
| total number of conversations                 |    3.00 |    NA |     NA |     NA |
| token count all conversations (raw)           | 1506.00 |    NA |     NA |     NA |
| token count all conversations (post-cleaning) | 1032.00 |    NA |     NA |     NA |
| exchange count (by conversation)              |   38.00 | 13.11 |  24.00 |  50.00 |
| word count raw (by conversation)              |  502.00 | 47.03 | 456.00 | 550.00 |
| word count clean (by conversation)            |  344.00 | 48.66 | 312.00 | 400.00 |
| cleaning retention rate (by conversation)     |    0.68 |  0.04 |   0.64 |   0.73 |
| morphemes-per-word (by conversation)          |    1.00 |  0.00 |   1.00 |   1.00 |
| letters-per-word (by conversation)            |    4.22 |  0.14 |   4.12 |   4.38 |
| lexical frequency lg10 (by conversation)      |    3.67 |  0.18 |   3.48 |   3.84 |
| words-per-turn raw (by conversation)          |    7.08 |  2.13 |   5.50 |   9.50 |
| words-per-turn clean (by conversation)        |    4.83 |  1.44 |   4.00 |   6.50 |
| TTR raw (by conversation)                     |    0.03 |  0.01 |   0.02 |   0.04 |
| TTR clean (by conversation)                   |    0.04 |  0.02 |   0.02 |   0.05 |

\`
