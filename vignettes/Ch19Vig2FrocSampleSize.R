## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
library(RJafroc)
library(ggplot2)

## -----------------------------------------------------------------------------
frocNhData <- DfExtractDataset(dataset04, trts = c(1,2))
ret <- SsFrocNhRsmModel(frocNhData, lesionPmf = c(0.7, 0.2, 0.1))
muMed <- ret$muMed
lambdaMed <- ret$lambdaMed
nuMed <- ret$nuMed
lesDistr <- ret$lesDistr
lesWghtDistr <- ret$lesWghtDistr
scaleFactor <- ret$scaleFactor

## -----------------------------------------------------------------------------
aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                            lesDistr = lesDistr, 
                                            lesWghtDistr = lesWghtDistr, OpChType = "ROC")$aucROC
aucwAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                               lesDistr = lesDistr, 
                                               lesWghtDistr = lesWghtDistr, OpChType = "wAFROC")$aucwAFROC

## -----------------------------------------------------------------------------
varCompwAFROC  <- StSignificanceTesting(frocNhData, FOM = "wAFROC", method = "DBMH", option = "RRRC")$varComp

## -----------------------------------------------------------------------------
ROC_ES <- 0.05
effectSizewAFROC <- scaleFactor * ROC_ES
J <- 5;K <- 100

varYTR <- varCompwAFROC$varTR 
varYTC <- varCompwAFROC$varTC
varYEps <- varCompwAFROC$varErr
ret <- SsPowerGivenJKDbmVarComp (J = J, K = K, effectSize = effectSizewAFROC, 
                                 varYTR, varYTC, varYEps, option = "RRRC")
powerwAFROC <- ret$powerRRRC
  
cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, ", Power-wAFROC = ", powerwAFROC, "\n")

## -----------------------------------------------------------------------------

varYTR <- varCompwAFROC$varTR 
varYTC <- varCompwAFROC$varTC
varYEps <- varCompwAFROC$varErr
ret2 <- SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = effectSizewAFROC, method = "DBMH", 
                      list(varYTR = varYTR, varYTC = varYTC, varYEps = varYEps))

cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, 
    ", K80RRRC = ", ret2$KRRRC, ", Power-wAFROC = ", ret2$powerRRRC, "\n")

## -----------------------------------------------------------------------------

ret3 <- SsPowerGivenJK(dataset = NULL, J = 6, K = ret2$KRRRC, effectSize = effectSizewAFROC, method = "DBMH", 
                    list(varYTR = varYTR, varYTC = varYTC, varYEps = varYEps))

cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, 
    ", powerRRRC = ", ret3$powerRRRC, "\n")

