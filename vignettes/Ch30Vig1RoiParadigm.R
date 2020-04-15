## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
str(datasetROI)
datasetROI$NL[1,1,1,]
mean(datasetROI$NL[,,1:50,])
datasetROI$NL[1,1,51,]
datasetROI$lesionVector[1]
datasetROI$LL[1,1,1,]
x <- datasetROI$LL;mean(x[is.finite(x)])

## -----------------------------------------------------------------------------
## fileName <- system.file(
##     "extdata", "RoiData.xlsx", package = "RJafroc", mustWork = TRUE)
## ds <- DfReadDataFile(fileName)
## ds$dataType

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 1 two views of Truth worksheet",fig.show='hold',fig.align='center'----
knitr::include_graphics(c("images/ROI-Truth-1.png","images/ROI-Truth-2.png"))

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 2 two views of FP worksheet",fig.show='hold',fig.align='center'----
knitr::include_graphics(c("images/ROI-FP-1.png","images/ROI-FP-2.png"))

## ---- echo=FALSE,out.width="50%",out.height="20%",fig.cap="Fig. 2 TP worksheet",fig.show='hold',fig.align='center'----
knitr::include_graphics("images/ROI-TP-1.png")

