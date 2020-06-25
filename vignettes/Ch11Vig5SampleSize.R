## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## -----------------------------------------------------------------------------
rocData <- dataset02 ##"VanDyke.lrc"
#fileName <- dataset03 ## "Franken1.lrc"
retDbm <- StSignificanceTesting(dataset = rocData, FOM = "Wilcoxon", method = "DBMH")
print(retDbm$RRRC$ciDiffTrt)

## -----------------------------------------------------------------------------
effectSizeOpt <- abs(retDbm$RRRC$ciDiffTrt[1,"Estimate"]) + 2*retDbm$RRRC$ciDiffTrt[1,"StdErr"]

