## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
fed <- system.file("extdata", "FrocData.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(fed, newExcelFileFormat = FALSE)
str(x)

## -----------------------------------------------------------------------------
which(x$abnormalCases == 70)
x$NL[which(x$modalityID == "0"),which(x$readerID == "1"),which(x$normalCases == 1),1]
x$LL[which(x$modalityID == "0"),which(x$readerID == "1"),which(x$abnormalCases == 70),1]
x$LL[which(x$modalityID == "a"),which(x$readerID == "1"),which(x$abnormalCases == 70),1]

## -----------------------------------------------------------------------------
rocCr1R <- system.file("extdata", "toyFiles/ROC/rocCr1R.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocCr1R, newExcelFileFormat = TRUE)
str(x)

