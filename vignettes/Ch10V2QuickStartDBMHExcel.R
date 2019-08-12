## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## ------------------------------------------------------------------------
ret <- UtilOutputReport(dataset03, overWrite = TRUE, ReportFileExt = "xlsx")

## ------------------------------------------------------------------------
ret <- UtilOutputReport(dataset03, method = "ORH", overWrite = TRUE, ReportFileExt = "xlsx")

