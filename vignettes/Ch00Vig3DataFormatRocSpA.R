## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
rocSpA <- system.file("extdata", "toyFiles/ROC/rocSpA.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocSpA, newExcelFileFormat = TRUE)
str(x)

## -----------------------------------------------------------------------------
x$descriptions$truthTableStr[,1,1:10,1]

## -----------------------------------------------------------------------------
x$descriptions$truthTableStr[,1,1:10,2]

## -----------------------------------------------------------------------------
x$descriptions$truthTableStr[,2,1:10,1]
x$descriptions$truthTableStr[,2,1:10,2]

## -----------------------------------------------------------------------------
x$descriptions$truthTableStr[,3,1:10,1]
x$descriptions$truthTableStr[,3,1:10,2]

## -----------------------------------------------------------------------------
x$descriptions$truthTableStr[,4,1:10,1]
x$descriptions$truthTableStr[,4,1:10,2]

## -----------------------------------------------------------------------------
x$ratings$NL[,1,1:10,1]

## -----------------------------------------------------------------------------
x$ratings$LL[,1,1:5,1]
x$ratings$LL[,3,1:5,1]

