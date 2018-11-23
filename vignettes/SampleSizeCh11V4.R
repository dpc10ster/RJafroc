## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## ------------------------------------------------------------------------
retORH <- StSignificanceTesting(data = dataset02, FOM = "Wilcoxon", method = "ORH")
varTR <- retORH$varComp$varCov[2];varTR <- max(varTR,0)
cov1 <- retORH$varComp$varCov[3]
cov2 <- retORH$varComp$varCov[4]
cov3 <- retORH$varComp$varCov[5]
varEps <- retORH$varComp$varCov[6]
effectSize <- retORH$ciDiffTrtRRRC$Estimate
KStar <- length(dataset02$NL[1,1,,1])
power <- SsPowerGivenJK(6, 251, effectSize, method = "ORH", option = "RRRC", 
             cov1 = cov1, cov2 = cov2, cov3 = cov3,
             varEps = varEps, varTR = varTR, KStar = KStar)

## ------------------------------------------------------------------------
str(power)

## ------------------------------------------------------------------------
powTab <- SsPowerTable(effectSize = effectSize,
             method = "ORH", option = "RRRC", 
             cov1 = cov1, cov2 = cov2, cov3 = cov3,
             varEps = varEps, varTR = varTR, KStar = KStar)

## ------------------------------------------------------------------------
powTab

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(J = 10, effectSize = effectSize,  
             method = "ORH", option = "RRRC", 
             cov1 = cov1, cov2 = cov2, cov3 = cov3,
             varEps = varEps, varTR = varTR, KStar = KStar)

## ------------------------------------------------------------------------
str(ncases)

