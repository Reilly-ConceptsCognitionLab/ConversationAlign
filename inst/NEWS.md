# ConversationAlign 0.2.0

# - 2025-6-8

## Breaking changes

This is a major release. We have included many new features and fixed numerous bugs.

### Added
-   Added news (you're reading it now)
-   Opened discussions and issues fora
-   New function ``read_1file()`` that reads and formats a conversation transcript that is already in your R environment 
-   Improved handling of contractions in ``clean_dyads()`` with error detection/substition of non-ASCII character apostrophes. Contraction expansion (e.g., 'they're' to 'they are') now handled by a separate internal function call using custom regex.   
-   Added argument ``omit_stops`` to ``clean_dyads()`` specifying optional stopword removal 
-   Added argument ``which_stoplist`` to ``clean_dyads()`` specifying a four stopword list options (SMART_stops, MIT_stops, Temple_stops25, CA_orig_stops). 
-   Added pdf manual for ``ConversationAlign``

### Fixed
-  Warnings and checks (variable names, etc) to read, clean, align functions

### Deprecated
-   We are in the process of phasing out our original stopword list from earlier versions of ConversationAlign (<2025). We have included the old list as an optional call to the argument clean_dyads() as ``CA_OriginalStops``. However, the default stopword list is new - ``Temple_Stopwords23``

