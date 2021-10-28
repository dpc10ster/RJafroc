## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
UtilFigureOfMerit(datasetROI, FOM = "ROI")
fom <- UtilFigureOfMerit(datasetROI, FOM = "ROI")

