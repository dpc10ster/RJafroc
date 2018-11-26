## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)
library(ggplot2)

## ------------------------------------------------------------------------
retDbm <- StSignificanceTesting(data = dataset02, FOM = "Wilcoxon", method = "DBMH")
effectSize <- retDbm$ciDiffTrtRRRC$Estimate
varCompDBM <- retDbm$varComp
varYTR <- varCompDBM$varComp[3]
varYTC <- varCompDBM$varComp[4]
varYEps <- varCompDBM$varComp[6]
power <- SsPowerGivenJK(J = 10, K = 163, effectSize, method = "DBMH", option = "RRRC",
                        varYTR = varYTR, varYTC = varYTC, varYEps = varYEps)

## ------------------------------------------------------------------------
str(power)

## ------------------------------------------------------------------------
powTab <- SsPowerTable(effectSize = effectSize, desiredPower = 0.8,  
                       method = "DBMH", option = "RRRC", varYTR = varYTR, varYTC = varYTC, varYEps = varYEps)

## ------------------------------------------------------------------------
powTab

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(J = 10, effectSize = effectSize, desiredPower = 0.8,  
                       method = "DBMH", option = "RRRC", varYTR = varYTR, varYTC = varYTC, varYEps = varYEps)

## ------------------------------------------------------------------------
str(ncases)

