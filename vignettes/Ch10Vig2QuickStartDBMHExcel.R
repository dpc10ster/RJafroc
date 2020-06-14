## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## -----------------------------------------------------------------------------
ret <- UtilOutputReport(get("dataset03"), FOM = "Wilcoxon", method = "DBMH", overWrite = TRUE, ReportFileExt = "xlsx")

## -----------------------------------------------------------------------------
ret <- UtilOutputReport(get("dataset03"), FOM = "Wilcoxon", method = "ORH", overWrite = TRUE, ReportFileExt = "xlsx")

