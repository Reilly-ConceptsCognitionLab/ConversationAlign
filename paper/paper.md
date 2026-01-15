---
title: 'ConversationAlign: An R package for Computing Linguistic Alignment and Corpus Analytics in Dyadic Conversation Transcripts'
tags:
  - R
  - conversation analysis
  - alignment
  - language
  - discourse
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

date: "15 January 2026"
bibliography: paper.bib
funding: |
 R01 DC013063 (NIH/NIDCD)
output: rticles::joss_article 
---

# Summary
 `ConversationAlign` executes a series of operations upon one or more conversation transcripts (i.e., two-person dialogues).  `ConversationAlign` imports raw language transcripts into R, appends unique document identifiers, and concatenates all conversations into a single dataframe. `ConversationAlign` generates corpus analytics and executes text cleaning operations such as stopword removal and lemmatization. The package vectorizes text and yokes published norms to each content word spanning more than 40 lexical, affective, and semantic dimensions. `ConversationAlign` outputs summary statistics each conversation including main effects and indices of local and global alignment for each specified dimension of interest.   
 
# Software Design
 `ConversationAlign` was designed as a user-friendly R package with no proprietary components or input from AI (large language models). We were intentional about making the software accessible to users who do not have  extensive backgrounds in computational linguistics or Natural Language Processing.

# Statment of Need
Although many excellent text analysis applications exist (e.g., `Quanteda` [@benoit:2018] and `Korpus` [@michalke:2018]), we know of no R packages that are tailored to the unique demands of conversation analysis (but for Python see ALIGN [@duran:2019]). `ConversationAlign` offers a comprehensive text processing pipeline and novel algoirthms for computing linguistic alignment in 2-person dialogues. This software offers standardization and automation advantages that are in great need in a field that has historically relied heavily upon manual coding systems and subjective human judgment.

# Research Impact Statement
 `ConversationAlign` has supported two peer-reviewed publications to date in the cognitive neuroscience and psychological methods journals `Cortex` [@reilly:2025] and `Behavior Research Methods` (article in press). 
The software is relatively new, and there are limited indices of uptake within the broader language research community.

# Background
Conversation is among the most complex behaviors that humans routinely undertake. In a dyadic interaction, conversation partners modify the form and content of their own production to align with each other [@pickering:2021]. This process, known as linguistic alignment, occurs across many dimensions. `ConversationAlign` offers an automated approach to computing linguistic alignment across >40 distinct psycholinguistic dimensions (e.g., word length, valence, concreteness), leveraging recent advances in natural language processing to examine dynamics of human interaction at an unprecedented scale.  

# AI Usage
`ConversationAlign` is **NOT** a large language model (LLM). It instead indexes an static lexical lookup database populated with published norms for >100,000 English words across more than 40 unique dimensions. We used DeepSeek to troubleshoot elements of code and to generate regular expressions (regex) for complex pattern matching. We did not use AI to write this paper or generate segments of code. 

# References
