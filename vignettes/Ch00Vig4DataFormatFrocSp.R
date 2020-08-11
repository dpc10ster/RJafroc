## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
frocSp <- system.file("extdata", "toyFiles/FROC/frocSpC.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSp, newExcelFileFormat = TRUE)
str(x)

## -----------------------------------------------------------------------------
x$lesions$perCase
x$lesions$IDs
x$lesions$weights

## -----------------------------------------------------------------------------
x$ratings$NL[,1,1:9,1]
x$ratings$NL[,2,1:9,1]
x$ratings$NL[,3,1:9,1]

## -----------------------------------------------------------------------------
x$ratings$LL[,1,1:15,1]
x$ratings$LL[,2,1:15,1]
x$ratings$LL[,3,1:15,1]

