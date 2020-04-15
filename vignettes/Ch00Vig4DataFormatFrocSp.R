## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 1: Two views of Truth worksheet for file frocSp.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics(c("images/frocSpTruth.png","images/frocSpTruth2.png"))

## -----------------------------------------------------------------------------
frocSp <- system.file("extdata", "toyFiles/FROC/frocSp.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSp, newExcelFileFormat = TRUE)
str(x)

## -----------------------------------------------------------------------------
x$lesionVector
x$lesionID
x$lesionWeight

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 2: NL/FP worksheet, left, and LL/TP worksheet, right, for file frocSp.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics(c("images/frocSpNL.png","images/frocSpLL.png"))

## -----------------------------------------------------------------------------
x$NL[,1,1:9,1]
x$NL[,2,1:9,1]
x$NL[,3,1:9,1]

## -----------------------------------------------------------------------------
x$LL[,1,1:15,1]
x$LL[,2,1:15,1]
x$LL[,3,1:15,1]

