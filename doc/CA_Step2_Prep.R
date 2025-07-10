## ----message=FALSE, warning=F, echo=F-----------------------------------------
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

