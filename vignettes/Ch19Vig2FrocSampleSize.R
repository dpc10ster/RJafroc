## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
library(RJafroc)
library(ggplot2)

## ------------------------------------------------------------------------
frocData <- dataset04
ret <- SsFrocNhRsmModel(frocData, c(0.7, 0.2, 0.1))
muMed <- ret$muMed
lambdaMed <- ret$lambdaMed
nuMed <- ret$nuMed
lesDistr <- ret$lesDistr
lesWghtDistr <- ret$lesWghtDistr
scaleFactor <- ret$scaleFactor

## ------------------------------------------------------------------------
aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                            lesDistr = lesDistr, 
                                            lesWghtDistr = lesWghtDistr, type = "ROC")$aucROC
aucwAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                               lesDistr = lesDistr, 
                                               lesWghtDistr = lesWghtDistr, type = "wAFROC")$aucwAFROC

## ------------------------------------------------------------------------
temp2 <- StSignificanceTesting(frocData, FOM = "wAFROC", method = "DBMH", option = "RRRC")
varCompwAFROC <- temp2$varComp

## ------------------------------------------------------------------------

ROC_ES <- 0.05
effectSizewAFROC <- scaleFactor * ROC_ES
JTest <- 5;KTest <- 100

varYTR <- varCompwAFROC$varTR 
varYTC <- varCompwAFROC$varTC
varYEps <- varCompwAFROC$varErr
ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizewAFROC, 
                                 varYTR, varYTC, varYEps, option = "RRRC")
powerwAFROC <- ret$powerRRRC
  
cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, ", Power-wAFROC = ", powerwAFROC, "\n")

## ------------------------------------------------------------------------

ROC_ES <- 0.05
effectSizewAFROC <- scaleFactor * ROC_ES
JTest <- 5

varYTR <- varCompwAFROC$varTR 
varYTC <- varCompwAFROC$varTC
varYEps <- varCompwAFROC$varErr
ret2 <- SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = effectSizewAFROC, method = "DBMH", 
                      list(varYTR = varYTR, varYTC = varYTC, varYEps = varYEps))

cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, 
    ", nCasesRRRC = ", ret2$KRRRC, ", Power-wAFROC = ", ret2$powerRRRC, "\n")

