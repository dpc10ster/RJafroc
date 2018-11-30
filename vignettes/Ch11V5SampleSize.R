## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## ------------------------------------------------------------------------
fileName <- "VanDyke.lrc"
rocData <- DfReadDataFile(fileName, format = "MRMC")
retDbm <- StSignificanceTesting(dataset = rocData, FOM = "Wilcoxon", method = "DBMH")
str(retDbm$ciDiffTrtRRRC)

## ------------------------------------------------------------------------
effectSize <- abs(retDbm$ciDiffTrtRRRC$Estimate) + 2*retDbm$ciDiffTrtRRRC$StdErr

