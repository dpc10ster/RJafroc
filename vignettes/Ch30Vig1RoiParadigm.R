## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
str(datasetROI)
datasetROI$NL[1,1,1,]
mean(datasetROI$NL[,,1:50,])
datasetROI$NL[1,1,51,]
datasetROI$lesionVector[1]
datasetROI$LL[1,1,1,]
x <- datasetROI$LL;mean(x[is.finite(x)])
