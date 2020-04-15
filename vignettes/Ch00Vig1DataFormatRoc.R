## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 1 Truth worksheet for file rocCr.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/rocCrTruth.png")

## -----------------------------------------------------------------------------
rocCr <- system.file("extdata", "toyFiles/ROC/rocCr.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocCr, newExcelFileFormat = TRUE)
str(x)

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 1 FP worksheet for file rocCr.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/rocCrFp.png")

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 1 TP worksheet for file rocCr.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/rocCrTp.png")

## -----------------------------------------------------------------------------
which(x$abnormalCases == 70)
x$NL[which(x$modalityID == "0"),which(x$readerID == "1"),which(x$normalCases == 1),1]
x$LL[which(x$modalityID == "0"),which(x$readerID == "1"),which(x$abnormalCases == 70),1]
x$LL[which(x$modalityID == "a"),which(x$readerID == "1"),which(x$abnormalCases == 70),1]

