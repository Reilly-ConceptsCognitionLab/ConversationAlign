#' replacements_25
#'
#' String replacement for pattern matching and expanding lots of contractions
#' @name replacements_25
#' @importFrom data.table :=
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @keywords internal
#' @noRd

#my_data <- replace_strings_jrcustom(my_data, "text_column")

replacements_25 <- function(dat, wordcol) {
  # Load required packages
  my_packages <- c("data.table", "dplyr", "magrittr", "rlang")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }


  col <- rlang::sym(wordcol)

  # Apply all replacements in sequence
  dat %>%
    # Contractions starting with a/i
    dplyr::mutate(!!col := gsub("\\baren['’]t\\b", "are not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bcan['’]t\\b", "cannot", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bcould['’]ve\\b", "could have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bcouldn['’]t\\b", "could not", !!col, ignore.case = TRUE)) %>%

    # Contractions starting with d
    dplyr::mutate(!!col := gsub("\\bdidn['’]t\\b", "did not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bdoesn['’]t\\b", "does not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bdon['’]t\\b", "do not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bdunno\\b", "do not know", !!col, ignore.case = TRUE)) %>%

    # Contractions starting with g
    dplyr::mutate(!!col := gsub("\\bgimme\\b", "give me", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bgonna\\b", "going to", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bgotta\\b", "got to", !!col, ignore.case = TRUE)) %>%

    # Contractions starting with h
    dplyr::mutate(!!col := gsub("\\bhadn['’]t\\b", "had not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bhasn['’]t\\b", "has not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bhaven['’]t\\b", "have not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bhe['’]ll\\b", "he will", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bhe['’]s\\b", "he is", !!col, ignore.case = TRUE)) %>%

    # Contractions starting with i
    dplyr::mutate(!!col := gsub("\\bi['’]d\\b", "i would", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bi['’]ll\\b", "i will", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bi['’]m\\b", "i am", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bi['’]ve\\b", "i have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bisn['’]t\\b", "is not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bit['’]ll\\b", "it will", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bit['’]s\\b", "it is", !!col, ignore.case = TRUE)) %>%

    # Contractions starting with k-m
    dplyr::mutate(!!col := gsub("\\bkinda\\b", "kind of", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\blemme\\b", "let me", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bmight['’]ve\\b", "might have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bmightn['’]t\\b", "might not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bmust['’]ve\\b", "must have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bmustn['’]t\\b", "must not", !!col, ignore.case = TRUE)) %>%

    # Contractions starting with n-w
    dplyr::mutate(!!col := gsub("\\bneedn['’]t\\b", "need not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bshe['’]ll\\b", "she will", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bshe['’]s\\b", "she is", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bshould['’]ve\\b", "should have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bshouldn['’]t\\b", "should not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bsorta\\b", "sort of", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bthat['’]s\\b", "that is", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bthere['’]d\\b", "there would", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bthey['’]ll\\b", "they will", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bthey['’]re\\b", "they are", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bthey['’]ve\\b", "they have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwanna\\b", "want to", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwasn['’]t\\b", "was not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwe['’]ll\\b", "we will", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwe['’]re\\b", "we are", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwe['’]ve\\b", "we have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bweren['’]t\\b", "were not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwon['’]t\\b", "will not", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwould['’]ve\\b", "would have", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\bwouldn['’]t\\b", "would not", !!col, ignore.case = TRUE)) %>%

    # Contractions starting with y
    dplyr::mutate(!!col := gsub("\\byou['’]d\\b", "you would", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\byou['’]ll\\b", "you will", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\byou['’]re\\b", "you are", !!col, ignore.case = TRUE)) %>%
    dplyr::mutate(!!col := gsub("\\byou['’]ve\\b", "you have", !!col, ignore.case = TRUE))
}
