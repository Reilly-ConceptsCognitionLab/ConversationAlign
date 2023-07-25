
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ConversationAlign: An R package for analyzing alignment between speakers in natural language transcripts

<!-- badges: start -->
<!-- badges: end -->

## Overview

## Installation

Install the development version of ConversationAlign from
[GitHub](https://github.com/) by typing the following in your console or
script (make sure you have devtools installed):

``` r
# install.packages("devtools")
devtools::install_github("Reilly-ConceptsCognitionLab/ConversationAlign")
```

## Prep your language transcripts

Move your Otter.ai text files or CSV files into a folder. The default
folder name is ‘my_transcripts’. If you want to specify your own folder
name in the root directory that’s fine too. You will call that path as
an argument to the first function called read_dyads()

## Read your transcripts into R

### read_dyads()

This will read all your files and concatenate them into a single
dataframe, appending document IDs

``` r
myrawtranscripts <- read_dyads()
myrawtranscripts <- read_dyads(/my_customfolder)  #if specifying a custom folder path
```

## Clean your transcripts

This step uses regular expressions to clean and format your data,
eliminating stopwords, changing the case to lower, omitting whitespaces
and non-alphabetic characters, etc.

``` r
#takes object from the read_dyads step
mycleantranscripts <- clean_dyads(myrawtranscripts)
```

## Align your transcripts

``` r
#TBD
```

## Inspect your transcripts

``` r
#TBD
```

## Analyze your transcripts

``` r
#TBD
```

## Get in touch

Email Jamie Reilly with any feedback.
