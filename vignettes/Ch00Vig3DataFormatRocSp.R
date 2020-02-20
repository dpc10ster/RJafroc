## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 1: Truth worksheet for file inst/extdata/toyFiles/ROC/rocSp.xlsx",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/rocSpTruth.png")

## -----------------------------------------------------------------------------
rocSp <- system.file("extdata", "toyFiles/ROC/rocSp.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocSp, newExcelFileFormat = TRUE)
str(x)

## -----------------------------------------------------------------------------
x$truthTableStr[,1,1:15,1]

## -----------------------------------------------------------------------------
x$truthTableStr[,1,1:15,2]

## -----------------------------------------------------------------------------
x$truthTableStr[,2,1:15,1]

## -----------------------------------------------------------------------------
x$truthTableStr[,3,1:15,1]

## -----------------------------------------------------------------------------
x$truthTableStr[,1,16:30,2]

## -----------------------------------------------------------------------------
x$truthTableStr[,1,16:30,1]

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 2 FP/TP worksheets; LEFT=FP, (b) RIGHT=TP",fig.show='hold',fig.align='center'----
knitr::include_graphics(c("images/rocSpFp.png","images/rocSpTp.png"))

## -----------------------------------------------------------------------------
x$NL[,1,1:15,1]
x$NL[,2,1:15,1]
x$NL[,3,1:15,1]

## -----------------------------------------------------------------------------
x$LL[,1,1:15,1]
x$LL[,2,1:15,1]
x$LL[,3,1:15,1]

