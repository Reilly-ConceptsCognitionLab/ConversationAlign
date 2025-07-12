#' replacements_25
#'
#' String replacement for pattern matching and expanding lots of contractions
#' @name replacements_25
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @keywords internal
#' @noRd

replacements_25 <- function(dat, wordcol) {
  # Load required packages
  my_packages <- c("data.table", "dplyr", "magrittr")
  for (pkg in my_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }

  # Apply all replacements in sequence
  dat %>%
    # Contractions starting with a/i
    dplyr::mutate({{wordcol}} := gsub("\\baren't\\b", "are not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bcan't\\b", "cannot", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bcould've\\b", "could have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bcouldn't\\b", "could not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bdidn't\\b", "did not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bdoesn't\\b", "does not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bdon't\\b", "do not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bdunno\\b", "do not know", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bgimme\\b", "give me", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bgonna\\b", "going to", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bgotta\\b", "got to", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bhadn't\\b", "had not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bhasn't\\b", "has not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bhaven't\\b", "have not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bhe'll\\b", "he will", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bhe's\\b", "he is", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bi'd\\b", "i would", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bi'll\\b", "i will", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bi'm\\b", "i am", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bi've\\b", "i have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bisn't\\b", "is not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bit'll\\b", "it will", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bit's\\b", "it is", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bkinda\\b", "kind of", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\blemme\\b", "let me", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bmight've\\b", "might have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bmightn't\\b", "might not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bmust've\\b", "must have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bmustn't\\b", "must not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bneedn't\\b", "need not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bshe'll\\b", "she will", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bshe's\\b", "she is", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bshould've\\b", "should have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bshouldn't\\b", "should not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bsorta\\b", "sort of", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bthat's\\b", "that is", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bthere'd\\b", "there would", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bthey'll\\b", "they will", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bthey're\\b", "they are", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bthey've\\b", "they have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwanna\\b", "want to", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwasn't\\b", "was not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwe'll\\b", "we will", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwe're\\b", "we are", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwe've\\b", "we have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bweren't\\b", "were not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwon't\\b", "will not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwould've\\b", "would have", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\bwouldn't\\b", "would not", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\byou'd\\b", "you would", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\byou'll\\b", "you will", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\byou're\\b", "you are", {{wordcol}}, ignore.case = TRUE)) %>%
    dplyr::mutate({{wordcol}} := gsub("\\byou've\\b", "you have", {{wordcol}}, ignore.case = TRUE))
}
