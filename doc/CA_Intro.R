## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=F-------------------------------------------------
# Check if devtools is installed, if not install it
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load devtools
library(devtools)

# Check if ConversationAlign is installed, if not install from GitHub
if (!require("ConversationAlign", quietly = TRUE)) {
  devtools::install_github("Reilly-ConceptsCognitionLab/ConversationAlign")
}

# Load SemanticDistance
library(ConversationAlign)

## ----eval=T, message=F, warning=F---------------------------------------------
knitr::kable(head(NurseryRhymes, 20), format = "simple")
str(NurseryRhymes)

## -----------------------------------------------------------------------------
knitr::kable(head(MaronGross_2013, 20), format = "simple")
str(MaronGross_2013)

