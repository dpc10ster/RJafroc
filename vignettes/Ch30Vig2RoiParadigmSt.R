## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)

## -----------------------------------------------------------------------------
UtilFigureOfMerit(datasetROI, FOM = "ROI")
fom <- UtilFigureOfMerit(datasetROI, FOM = "ROI")

## -----------------------------------------------------------------------------
ret <- StSignificanceTesting(datasetROI, FOM = "Wilcoxon")
str(ret)

## -----------------------------------------------------------------------------
ret$ANOVA$VarCom

## -----------------------------------------------------------------------------
ret$RRRC$FTests$FStat[1]
ret$RRRC$FTests$DF
ret$RRRC$FTests$p[1]

## -----------------------------------------------------------------------------
ret$RRRC$ciDiffTrt$Estimate
ret$RRRC$ciDiffTrt$PrGTt
ret$RRRC$ciDiffTrt$CILower
ret$RRRC$ciDiffTrt$CIUpper

## -----------------------------------------------------------------------------
ret$FRRC$FTests$Chisq[1]
ret$FRRC$FTests$DF[1]
ret$FRRC$FTests$p[1]

## -----------------------------------------------------------------------------
ret$FRRC$ciDiffTrt$Estimate
ret$FRRC$ciDiffTrt$PrGTz
ret$FRRC$ciDiffTrt$CILower
ret$FRRC$ciDiffTrt$CIUpper

## -----------------------------------------------------------------------------
ret$RRFC$FTests$F[1]
ret$RRFC$FTests$DF[2]
ret$RRFC$FTests$p[1]

## -----------------------------------------------------------------------------
ret$RRFC$ciDiffTrt$Estimate
ret$RRFC$ciDiffTrt$PrGTt
ret$RRFC$ciDiffTrt$CILower
ret$RRFC$ciDiffTrt$CIUpper


