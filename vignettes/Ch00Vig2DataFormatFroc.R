## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
frocCr <- system.file("extdata", "toyFiles/FROC/frocCr.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr, newExcelFileFormat = TRUE)
str(x)

## -----------------------------------------------------------------------------
x$lesionID

## -----------------------------------------------------------------------------
x$lesionWeight

## -----------------------------------------------------------------------------
x$LL[1,1,1,1]
x$LL[2,1,1,1]
length(x$LL[1,1,1,])

