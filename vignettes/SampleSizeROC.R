## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## ------------------------------------------------------------------------
UtilOutputReport(dataset03, stMethod = "DBMH", FOM = "Wilcoxon", overwrite = TRUE, ReportFileFormat = "xlsx")

