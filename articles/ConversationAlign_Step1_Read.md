# ConversationAlign_Step1_Read

## Reading data into R for ConversationAlign

Half the battle with R is getting your data imported and formatted. This
is especially true for string data and working with text.
`ConversationAlign` uses a series of sequential functions to import,
clean, and format your raw data. You **MUST** run each of these
functions. They append important variable names and automatically
reshape your data.  

## Prepping your data for import

- `ConversationAlign` works **ONLY** on dyadic (i.e., two person)
  conversation transcripts.
- Each transcript must nominally contain two colummns, one column should
  delineate the interlocutor (person who produced the text), and another
  column should contain the text itself.
- `ConversationAlign` contains an import function called
  [`read_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/read_dyads.md)
  that will scan a target folder for text samples.
- [`read_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/read_dyads.md)
  will import all of your transcripts into R and concatenate them into a
  single dataframe.
- [`read_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/read_dyads.md)
  will append each transcript’s filename as a unique identifier for that
  conversation. This is SUPER important to remember when analyzing your
  data.
- Store each of your individual conversation transcripts (`.csv`,
  `.txt`, `.ai`) that you wish to concatenate into a corpus in a folder.
  `ConversationAlign` will search for a folder called `my_transcripts`
  in the same directory as your script. However, feel free to name your
  folder anything you like. You can specify a custom path as an argument
  to read_dyads()
- Each transcript must nominally contain two columns of data
  (Participant and Text). All other columns (e.g., meta-data) will be
  retained.

### `read_dyads()`

Here are some exampples of
[`read_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/read_dyads.md)
in action. There is only one argument to
[`read_dyads()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/read_dyads.md),
and that is `my_path`. This is for supplying a quoted directory path to
the folder where your transcripts live. Remember to treat this folder as
a staging area! Once you are finished with a set of transcripts and
don’t want them read into `ConversationAlign` move them out of the
folder, or specify a new folder. Language data tends to proliferate
quickly, and it is easy to forget what you are doing. Be a CAREFUL
secretary, and record your steps.

Arguments to `read_dyads` include:  
1. **my_path**: default is ‘my_transcripts’, change path to your folder
name  

``` r
#will search for folder 'my_transcripts' in your current directory
MyConvos <- read_dyads()

#will scan custom folder called 'MyStuff' in your current directory, concatenating all files in that folder into a single dataframe
MyConvos2 <- read_dyads(my_path='/MyStuff')
```

### `read_1file()`

- Read single transcript already in R environment. We will use
  [`read_1file()`](https://reilly-conceptscognitionlab.github.io/ConversationAlign/reference/read_1file.md)
  to prep the Marc Maron and Terry Gross transcript. Look at how the
  column headers have changed and the object name (MaronGross_2013) is
  now the Event_ID (a document identifier),  

Arguments to `read_1file` include:  
1. **my_dat**: object already in your R environment containing text and
speaker information.

``` r
MaryLittleLamb <- read_1file(MaronGross_2013)
#print first ten rows of header
knitr::kable(head(MaronGross_2013, 15), format = "pipe")
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

  
