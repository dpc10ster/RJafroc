## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## -----------------------------------------------------------------------------
ret <- UtilOutputReport(get("dataset03"), FOM = "Wilcoxon", method = "DBM", overWrite = TRUE, ReportFileExt = "xlsx")

## -----------------------------------------------------------------------------
ret <- UtilOutputReport(get("dataset03"), FOM = "Wilcoxon", method = "OR", overWrite = TRUE, ReportFileExt = "xlsx")

