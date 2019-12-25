## ----setup, include = FALSE----------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ------------------------------------------------------------------------
rocSp <- system.file("extdata", "toyFiles/ROC/rocSp.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocSp, newExcelFileFormat = TRUE)
str(x)

## ------------------------------------------------------------------------
x$NL[,1,1:15,1]
x$NL[,2,1:15,1]
x$NL[,3,1:15,1]

## ------------------------------------------------------------------------
x$LL[,1,1:15,1]
x$LL[,2,1:15,1]
x$LL[,3,1:15,1]

