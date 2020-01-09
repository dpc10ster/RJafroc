## ----setup, include = FALSE----------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ------------------------------------------------------------------------
frocSp <- system.file("extdata", "toyFiles/FROC/frocSp.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSp, newExcelFileFormat = TRUE)
str(x)

## ------------------------------------------------------------------------
x$lesionVector
x$lesionID
x$lesionWeight

## ------------------------------------------------------------------------
x$NL[,1,1:9,1]
x$NL[,2,1:9,1]
x$NL[,3,1:9,1]

## ------------------------------------------------------------------------
x$LL[,1,1:15,1]
x$LL[,2,1:15,1]
x$LL[,3,1:15,1]

