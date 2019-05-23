## ----setup, include = FALSE----------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ------------------------------------------------------------------------
str(dataset04)
dataset04$NL[1,1,1,]
mean(dataset04$NL[,,1:50,])
dataset04$NL[1,1,51,]
dataset04$lesionNum[1]
dataset04$LL[1,1,1,]
x <- dataset04$LL;mean(x[is.finite(x)])

## ------------------------------------------------------------------------
fileName <- system.file(
    "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)
ds <- DfReadDataFile(fileName)
ds$dataType

