---
title: 'ConversationAlign: An R package for Computing Linguistic Alignment and Corpus Analytics in Dyadic Conversation Transcripts'
tags:
  - R
  - conversation analysis
  - alignment
  - psycholinguistics
  - natural language processing
authors:
  - name: Jamie Reilly
    orcid: 0000-0002-0891-438X
    equal-contrib: false
    corresponding: true
    affiliation: "1, 2"
    
  - name: Benjamin Sacks
    orcid: 0009-0005-7323-6625 
    equal-contrib: true
    corresponding: false
    affiliation: 2
    
  - name: Virginia Ulichney
    orcid: 0000-0002-8170-1313
    equal-contrib: false
    corresponding: false
    affiliation: 2
    
  - name: Gus Cooney
    orcid: 0000-0003-4082-7515 
    equal-contrib: false
    corresponding: false
    affiliation: "3"
    
  - name: Chelsea Helion
    orcid: 0000-0002-3149-1493
    equal-contrib: false
    corresponding: false
    affiliation: "1,2"
    
affiliations:
  - name: Department of Communication Sciences and Disorders, Temple University, United States
    index: 1
  - name: Department of Psychology and Neuroscience, Temple University, United States
    index: 2
  - name: Wharton School, University of Pennsylvania, United States
    index: 3

correspondence: jamie.reilly@temple.edu
date: " 9 July 2025"
bibliography: paper.bib
abstract: |
 `ConversationAlign` is an R package that executes a series of operations upon one or more conversation transcripts (i.e., two-person dialogues). Transcripts nominally contain at least two variables (speaker  identity and text).`ConversationAlign` will retain all other meta-data such as timestamps, demographics, and grouping variables.  `ConversationAlign` imports raw transcripts into R, appends unique document identifiers, and concatenates all conversations into a single dataframe. `ConversationAlign` generates corpus analytics characterizing the conversation transcript(s) of interest. Users guide a number of text cleaning operations such as stopword removal and lemmatization. The package ultimately vectorizes the original text into a one-word-per-row format. `ConversationAlign` yokes published norms to each content word spanning more than 40 lexical, affective, and semantic dimensions (e.g. word length, morphological complexity, arousal, valence). `ConversationAlign` outputs summary data for each conversation including main effects and indices of local and global alignment for each specified dimension of interest.   
keywords: |
  conversation analysis; discourse; language processing; alignment
authorcontributions: |
  JR, VU, and BS conceived the software. All authors drafted and edited the paper.
funding: |
  This research was funded by R01 DC013063 from the US National Institute on Deafness and Other Communicative Disorders (NIH/NIDCD).
conflictsofinterest: |
  The authors declare no conflicts of interest.
output: rticles::joss_article 
keep_tex: true
csl: apa.csl
journal: JOSS
---

# Statment of Need
Conversation is among the most complex social and linguistic behaviors that humans routinely undertake. In a dyadic interaction, two conversation partners modify the form and content of their own production to align with each other [@pickering_understanding_2021]. This process, known as linguistic alignment, occurs across many dimensions (e.g., affective coloring, prosody, gesture, formality, semantic and syntactic complexity).   `ConversationAlign` offers an automated approach to computing empirical indices alignment between conversation partners across one or more conversations with coverage of over 40 distinct psycholinguistic dimensions (e.g., word length, valence, concreteness, morphological complexity).  Although other open-source software applications exist for text corpus analyses, such as Quanteda [@benoit_quanteda_2018] and Korpus [@michalke_korpus_2018], we know of R-specific packages that offer a comprehensive text processing pipeline capable of cleaning, formatting, transforming, and summarizing raw conversation transcripts (but for Python see ALIGN [@duran_align_2019]).

Recent advances in Natural Language Processing (NLP) and the dissemination of corpora such as CANDOR [@reece_candor_2023] are creating fertile for the study of dialogue. `ConversationAlign` is a open-source R package that leverages many of these advances as a tool for examining conversation dynamics at an unprecedented scale. `ConversationAlign` is **NOT** a large language model (e.g., GPT, DeepSeek). Its algorithms do not use generative artificial intelligence and instead index an internal lexical lookup database with coverage of over 100,000 English words across more than 40 unique dimensions spanning affective (e.g., happiness, valence), semantic (e.g., concreteness, semantic density), lexical (e.g., age-of-scqusition, morphological complexity), and phonological (e.g., word length, syllable length) information. `ConversationAlign` joins content words from conversation transcripts of any length to their corresponding values in this lookup database, effectively transforming words into quantitative time series objects aggregated by speaker, turn, and conversation. Figure 1 illustrates the primary steps undertaken by `ConversationAlign` in executing these transformations. <br>

![Overview of ConversationAlign Pipeline\label{fig:demo}](overview.png){width=70%}


# Key Components of the ConversationAlign Pipeline
`ConversationAlign` processes dyadic (2-person) conversation transcripts (`*.csv`, `*.txt)` via a series of four customizable functions: <br>


1. **read_dyads()**:  imports one or more conversation transcripts into R, concatenating all transcripts into a single dataframe marked with its unique filename as a document identifier. <br>
2. **prep_dyads()**:  executes numerous text cleaning and formatting operations (e.g., to lowercase, expand contractions, remove special characters, squish whitespace). Options include stopword removal, stopword list specification, and lemmatization. `prep_dyads()' splits the raw text into a one word per row format then prompts the user to select up to three dimensions for computing main effects and alignment. `prep_dyads()' returns a dataframe with values for the variables of interest (e.g., word length, word frequency, valence) to each running content word. <br>
3. **summarize_dyads()**:  produces a summary dataframe with main effects and alignment indices for the user-specified variables of interest summarized by conversation (Event_ID) and participant (Participant_ID). Alignment indices include: a) lagged spearman R correlation values reflecting turn-by-turn covariance between interlocutors across each dimension of interest (e.g., Mary uses unpleasant words, Dave immediately responds with unpleasant words); b) dAUC: global distance between partners by conversation across each variable of interest (e.g., 'pleasantness' distance between Dave and Mary across all turns). `summarize_dyads()` produces raw AUC and AUC normalized to a fixed conversation length (i.e., 50 exchanges, 100 turns) to promote standardization/comparison across different conversation durations. <br>
4. **corpus_analytics()**: produces text analytics and descriptive statistics for your conversation corpus, including  total number of tokens, average number of turns per conversation, average number of words-per-turn by conversation, average word length (letter count) by conversation, type token ratio by converation (for comprehensive list see package documentation). Summary dataframe readily exportable to a table for journal submission. <br>


 `ConversationAlign's` core algorithm(s) transform raw text data into numeric time series yoked to specific dimensions of interest selected by the user. This approach moves beyond simple comparisons of means to potentially capture causal relationships (e.g., John mirrors increases in Mary's word length, but Mary does not recirpocate this pattern). The alignment indices generated by `ConversationAlign` measure both local (e.g., turn-by-turn) and global (AUC) distance between conversation partners across each dimension of interest. 

# Uses of `ConversationAlign`
We anticipate that `ConversationAlign` will have many theoretical and applied applications for measuring and modeling conversation dynamics. Example applications include: <br>
- Assesing alignment dynamics between conversation partners across individual difference factors (e.g., age, culture, education level, socio-economic status). <br>
- Asessing pre/post changes in naturalistic language use as a function of a specific intervention (e.g., meta-cognitive training for traumatic brain injury). <br>
- Measuring alignment dynamics between friends (and rivals) to elucidate semantic, affective, and lexical dynamics that mark 'good' conversations. <br>
- Examining alignment (and misalignment) between people with neurological disorders and their significant others (spouses, friends, children) to improve the quality of communication and reduce the prevalence of communication breakdown. <br>
- Syncrhonizing language with physiological data (e.g., biosignals) to examine real-time coupling between  interacting people and brains. <br>

## Concluding Remarks
We look forward to supporting the user community in their efforts to better understand, measure, and model communication using `ConversationAlign`. 

# Acknowledgements
We are grateful to Sarah Weinstein, Tania Giovannetti, and Anna Duncan for conceptual and methodological advice. <br>

This work was supported by US Public Health Service grant R01 DC013063 (JR) from the National Institute on Deafness and Communicative Disorders (NIH/NIDCD).

# References

