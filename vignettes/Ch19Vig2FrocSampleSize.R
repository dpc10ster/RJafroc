## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
library(RJafroc)
library(ggplot2)

## -----------------------------------------------------------------------------
lesDistr <- c(0.7, 0.2, 0.1)
frocNhData <- DfExtractDataset(dataset04, trts = c(1,2))
ret <- SsFrocNhRsmModel(frocNhData, lesDistr = lesDistr)
muMed <- ret$muMed
lambdaMed <- ret$lambdaMed
nuMed <- ret$nuMed
scaleFactor <- ret$scaleFactor

## -----------------------------------------------------------------------------
aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                            lesDistr = lesDistr, OpChType = "ROC")$aucROC
aucwAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                               lesDistr = lesDistr, OpChType = "wAFROC")$aucwAFROC

## -----------------------------------------------------------------------------
varCompwAFROC  <- StSignificanceTesting(frocNhData, FOM = "wAFROC", method = "DBM", analysisOption = "RRRC")$ANOVA$VarCom

## -----------------------------------------------------------------------------
ROC_ES <- 0.05
effectSizewAFROC <- scaleFactor * ROC_ES
J <- 5;K <- 100

varYTR <- varCompwAFROC["VarTR","Estimates"] 
varYTC <- varCompwAFROC["VarTC","Estimates"]
varYEps <- varCompwAFROC["VarErr","Estimates"]
ret <- SsPowerGivenJK(dataset = NULL, FOM = "Wilcoxon", J = J, K = K, analysisOption = "RRRC", 
                      effectSize = effectSizewAFROC, method = "DBM", LegacyCode = TRUE, 
                      list(VarTR = varYTR,
                           VarTC = varYTC,
                           VarErr = varYEps))
powerwAFROC <- ret$powerRRRC

cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, ", Power-wAFROC = ", powerwAFROC, "\n")

## -----------------------------------------------------------------------------
VarTR <- varCompwAFROC["VarTR","Estimates"] 
VarTC <- varCompwAFROC["VarTC","Estimates"]
VarErr <- varCompwAFROC["VarErr","Estimates"]
ret2 <- SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = effectSizewAFROC, method = "DBM", LegacyCode = TRUE,
                            list(VarTR = VarTR, VarTC = VarTC, VarErr = VarErr))

cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, 
    ", K80RRRC = ", ret2$KRRRC, ", Power-wAFROC = ", ret2$powerRRRC, "\n")

## -----------------------------------------------------------------------------

ret3 <- SsPowerGivenJK(dataset = NULL, J = 6, K = ret2$KRRRC, effectSize = effectSizewAFROC, method = "DBM", LegacyCode = TRUE,
                       list(VarTR = VarTR, VarTC = VarTC, VarErr = VarErr))

cat("ROC-ES = ", ROC_ES, ", wAFROC-ES = ", ROC_ES * scaleFactor, 
    ", powerRRRC = ", ret3$powerRRRC, "\n")

