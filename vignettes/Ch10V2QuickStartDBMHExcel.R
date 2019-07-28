## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## ------------------------------------------------------------------------
UtilOutputReport(dataset03, overWrite = TRUE, ReportFileExt = "xlsx")

## ------------------------------------------------------------------------
UtilOutputReport(dataset03, method = "ORH", overWrite = TRUE, ReportFileExt = "xlsx")

