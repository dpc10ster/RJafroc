## ----setup, include = FALSE----------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## ------------------------------------------------------------------------
UtilFigureOfMerit(datasetROI, FOM = "ROI")
fom <- UtilFigureOfMerit(datasetROI, FOM = "ROI")

## ------------------------------------------------------------------------
ret <- StSignificanceTesting(datasetROI, FOM = "Wilcoxon")
str(ret)

## ------------------------------------------------------------------------
ret$varComp

## ------------------------------------------------------------------------
ret$FTestStatsRRRC$fRRRC
ret$FTestStatsRRRC$ndfRRRC
ret$FTestStatsRRRC$ddfRRRC
ret$FTestStatsRRRC$pRRRC

## ------------------------------------------------------------------------
ret$ciDiffTrtRRRC

## ------------------------------------------------------------------------
ret$FTestStatsFRRC$fFRRC
ret$FTestStatsFRRC$ndfFRRC
ret$FTestStatsFRRC$ddfFRRC
ret$FTestStatsFRRC$pFRRC

## ------------------------------------------------------------------------
ret$ciDiffTrtFRRC

## ------------------------------------------------------------------------
ret$FTestStatsRRFC$fRRFC
ret$FTestStatsRRFC$ndfRRFC
ret$FTestStatsRRFC$ddfRRFC
ret$FTestStatsRRFC$pRRFC


## ------------------------------------------------------------------------
ret$ciDiffTrtRRFC

