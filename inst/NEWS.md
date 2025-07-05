# ConversationAlign 0.2.0

# - 2025-6-8

## Breaking changes

This is a major release. We have included many new features and fixed numerous bugs.

### Added
-   Added news (you're reading it now)
-   Opened discussions and issues fora
-   Added function ``prep_dyads()`` that combines two previous preparation steps ``clean_dyads()` and ``align_dyads``
-   New function ``read_1file()`` that reads and formats a conversation transcript that is already in your R environment 
-   Improved handling of contractions with error detection/substition of non-ASCII character apostrophes. Contraction expansion (e.g., 'they're' to 'they are') now handled by a separate internal function call using custom regex.   
-   Added argument ``omit_stops`` to ``prep_dyads`` specifying optional stopword removal 
-   Added argument ``which_stoplist`` to `prep_dyads`` specifying a four stopword list options (SMART_stops, MIT_stops, Temple_stops25, CA_orig_stops). 
-   Added pdf manual for ``ConversationAlign``
-   Revised internal lookup datase ``lookup_db``, replaced with ``lookup_Jul25``, includes many more dimensions and options for scaled vs. raw versions of variables
-   Added validation step to read_dyads() will throw error and warning message if any transcript has more than or less than two participants for any individual conversation (Event_ID)

### Fixed
-  Warnings and checks (variable names, etc) to read, clean, align functions

### Deprecated
-   We are in the process of phasing out our original stopword list from earlier versions of ConversationAlign (pre 2025). We have included the old list as an optional call to the argument clean_dyads() as ``CA_OriginalStops``. However, the default stopword list is new - ``Temple_Stops25``
-   Eliminated several dimensions from lookup database: prevalence, hope, stress, politeness, empathy, prevalence, closeness, encouragement, hope, doubt, hostility, surprise 

