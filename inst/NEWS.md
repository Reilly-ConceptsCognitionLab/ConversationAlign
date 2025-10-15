# ConversationAlign 0.4.0

# - 2025-10-12

## Breaking Changes

This is a major release including several new features and small changes.

### Added
- New function ``generate_shams`` to shuffle the order of interlocutor turns within each conversation. This creates a group of control conversations.
- New argument ``remove_backchannel`` in ``prep_dyads`` allows users to remove turns composed entirely of stopwords, preventing them from being filled in later stages.

### Modified
- ``summarize_dyads`` now computes dAUC for a lagged time series in addition to the unchanged. This is designed to account for both ways that interlocutors can be paired in each exchange.
- Updated names of standardized dAUC columns to reflect that they are standardized to 50 turns.
- Updates to documentation and website for new and modified features.



# ConversationAlign 0.3.1

# - 2025-7-29

## Breaking changes

This is a minor release involving one bug repair


### Modified
-  prep_dyads() added several missing cleaning functions that had been inadvertantly omitted during an earlier merge in preparation for CRAN



# ConversationAlign 0.3.0

# - 2025-7-21

## Breaking changes

This is a minor release involving mostly optimization of functions. 

### Added
-   Software license now LGPL 3.0

### Modified
-   Omitted prior GPL software license
-   Path to Journal of Open Source Software (JOSS) now points to main



# ConversationAlign 0.2.0

# - 2025-7-15

## Breaking changes

This is a major release. We have included many new features and fixed numerous bugs.

### Added
-   Added news (you're reading it now), discussions, and issues/bug tracker to Github repo
-   ``prep_dyads()`` that combines two previous preparation steps `clean_dyads()` and `align_dyads()`
-   New function `read_1file()` that formats a conversation transcript already in your R environment 
-   Improved handling of contractions with error detection/substition of non-ASCII character apostrophes. Contraction expansion (e.g., 'they're' to 'they are') now handled by a separate internal function call using custom regex.   
-   Added argument ``omit_stops`` to ``prep_dyads`` specifying optional stopword removal 
-   Added argument ``which_stoplist`` to `prep_dyads`` specifying a four stopword list options (SMART_stops, MIT_stops, Temple_stops25, CA_orig_stops). 
-   Added pdf manual for ``ConversationAlign``
-   Validation checks to `read_dyads()` will throw error and warning message if any transcript has more than or less than two participants for any individual conversation (Event_ID)
-   Added `corpus_analytics()` function that produces a variety of descriptive summary statistics (e.g., n-conversations, type token ratio, average turn length) in a table format.

### Modified
-   `summarize_dyads()` restructured in significant ways including the addition of three new arguments: `custom_lags` default lags for correlation are set at -2.0,2 users are free to specifiy additional lags , default is NULL, `sumdat_only` produces a summary dataframe with values averaged to two rows per conversation (one for each participant, `corr_type` specifies correlation to apply to lagged data 
-    Resampling of AUC in `summarize_dyads()` has been omitted in favor of proportionally rescaling dAUC to a standardized/fixed number of turns (100)
-    Modified internal lookup datase ``lookup_db``, replaced with ``lookup_Jul25``, includes many more dimensions and options for scaled vs. raw versions of variables
-    Moved all internal data to a different repository (ConversationAlign_Data) in order to get the package within CRAN file size constraints.  Package now loads data from external HTTPs source
-    Omitted outside calls to libraries from functions

### Fixed
-  Warnings and checks (variable names, etc) to read, clean, align functions

### Deprecated
-   We are in the process of phasing out our original stopword list from earlier versions of ConversationAlign (pre 2025). We have included the old list as an optional call to the argument clean_dyads() as ``CA_OriginalStops``. However, the default stopword list is new - ``Temple_Stops25``
-   Eliminated several dimensions from lookup database: prevalence, hope, stress, politeness, empathy, prevalence, closeness, encouragement, hope, doubt, hostility, surprise 
-   clean_dyads() and `align_dyads()`
