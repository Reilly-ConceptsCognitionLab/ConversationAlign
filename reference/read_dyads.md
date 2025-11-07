# read_dyads

Reads pre-formatted dyadic (2 interlocutor) conversation transcripts
from your machine. Transcripts must be either csv or txt format. IF you
are supplying a txt file, your transcript must be formatted as an
otter.ai txt file export. Your options for using csv files are more
flexible. ConversationAlign minimally requires a csv file with two
columns, denoting interlocutor and text. Each separate conversation
transcript should be saved as a separate file. ConversationAlign will
use the file names as a document ID. Within the read dyads function, set
the my_path argument as the directory path to the local folder
containing your transcripts on your machine (e.g., "my_transcripts").
Please see our github page for examples of properly formatted
transcripts:
https://github.com/Reilly-ConceptsCognitionLab/ConversationAlign

## Usage

``` r
read_dyads(my_path = "my_transcripts")
```

## Arguments

- my_path:

  folder of conversation transcripts in csv or txt format

## Value

a dataframe where each individual conversation transcript in a user's
directory has been concatenated. read_dyads appends a unique document
identifier to each conversation transcript appending its unique filename
as a factor level to 'Event_ID'.
