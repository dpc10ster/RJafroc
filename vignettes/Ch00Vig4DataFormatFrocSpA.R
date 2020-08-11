## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
frocSpA <- system.file("extdata", "toyFiles/FROC/frocSpA.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSpA, newExcelFileFormat = TRUE)
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

