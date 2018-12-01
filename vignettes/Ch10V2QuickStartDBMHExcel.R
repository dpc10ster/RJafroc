## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## ------------------------------------------------------------------------
UtilOutputReport(dataset03, method = "DBMH", FOM = "Wilcoxon", overwrite = TRUE, ReportFileName = "DBMH.xlsx", ReportFileFormat = "xlsx")

## ------------------------------------------------------------------------
UtilOutputReport(dataset03, method = "ORH", FOM = "Wilcoxon", overwrite = TRUE, ReportFileName = "ORH.xlsx", ReportFileFormat = "xlsx")

