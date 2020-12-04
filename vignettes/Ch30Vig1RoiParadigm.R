## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
str(datasetROI)
datasetROI$ratings$NL[1,1,1,]
mean(datasetROI$ratings$NL[,,1:50,])
datasetROI$ratings$NL[1,1,51,]
datasetROI$lesions$perCase[1]
datasetROI$ratings$LL[1,1,1,]
x <- datasetROI$ratings$LL;mean(x[is.finite(x)])

## -----------------------------------------------------------------------------
fileName <- system.file("extdata", "RoiData.xlsx", package = "RJafroc", mustWork = TRUE)
ds <- DfReadDataFile(fileName)
ds$descriptions$type

