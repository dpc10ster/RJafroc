## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)
library(ggplot2)

## -----------------------------------------------------------------------------
frocData <- DfExtractDataset(dataset04, trts = c(1,2))
rocData <- DfFroc2Roc(frocData)

## -----------------------------------------------------------------------------
lesDistr <- UtilLesionDistr(frocData)
lesWghts <- UtilLesionWeightsDistr(frocData) # this is needed later

## -----------------------------------------------------------------------------
print(lesDistr)
print(lesWghts)

## -----------------------------------------------------------------------------
I <- dim(frocData$NL)[1]
J <- dim(frocData$NL)[2]
RsmParms <- array(dim = c(I,J,3))
for (i in 1:I) {
  for (j in 1:J)  {
    x1 <- FitRsmRoc(rocData, trt = i, rdr = j, lesDistr)
    RsmParms[i,j,1] <- x1[[1]] # mu
    RsmParms[i,j,2] <- x1[[2]] # lambdaP
    RsmParms[i,j,3] <- x1[[3]] # nuP
  }
}

## -----------------------------------------------------------------------------
muMed <- median(RsmParms[,,1]) 
lambdaPMed <- median(RsmParms[,,2])
nuPMed <- median(RsmParms[,,3])

## -----------------------------------------------------------------------------
temp <- UtilPhysical2IntrinsicRSM(muMed, lambdaPMed, nuPMed)
lambdaMed <- temp$lambda
nuMed <- temp$nu

## -----------------------------------------------------------------------------
aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                            lesDistr = lesDistr, 
                                            lesWghtDistr = lesWghts, OpChType = "ROC")$aucROC
aucwAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                              lesDistr = lesDistr, 
                                              lesWghtDistr = lesWghts, OpChType = "wAFROC")$aucwAFROC

## -----------------------------------------------------------------------------
deltaMu <- seq(0.01, 0.2, 0.01) # values of deltaMu to scan below
esRoc <- array(dim = length(deltaMu));eswAfroc <- array(dim = length(deltaMu))
for (i in 1:length(deltaMu)) {
  esRoc[i] <- PlotRsmOperatingCharacteristics(
    muMed + deltaMu[i], lambdaMed, nuMed, lesDistr = lesDistr, 
    lesWghtDistr = lesWghts, OpChType = "ROC")$aucROC - aucRocNH
  eswAfroc[i] <- PlotRsmOperatingCharacteristics(
    muMed+ deltaMu[i], lambdaMed, nuMed, lesDistr = lesDistr, 
    lesWghtDistr = lesWghts, OpChType = "wAFROC")$aucwAFROC - aucwAfrocNH
  cat("ES_ROC = ", esRoc[i], ", ES_wAFROC = ", eswAfroc[i],"\n")
}

## ---- fig.show='hold', fig.align='center'-------------------------------------
df <- data.frame(es_ROC = esRoc, es_wAFROC = eswAfroc)
p <- ggplot(data = df, aes(x = es_ROC, y = es_wAFROC)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
  geom_point(size = 4) +
  scale_color_manual(values = "black") + 
  theme(axis.title.y = element_text(size = 10,face="bold"),
        axis.title.x = element_text(size = 10,face="bold")) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) 
print(p)

## -----------------------------------------------------------------------------
scaleFactor<-lm(eswAfroc~-1+esRoc) # fit values to straight line thru origin
effectSizeROC <- seq(0.01, 0.1, 0.01)
effectSizewAFROC <- effectSizeROC*scaleFactor$coefficients[1] # r2 = summary(scaleFactor)$r.squared

## -----------------------------------------------------------------------------
temp1 <- StSignificanceTesting(rocData, FOM = "Wilcoxon", method = "DBMH", analysisOption = "RRRC")
temp2 <- StSignificanceTesting(frocData, FOM = "wAFROC", method = "DBMH", analysisOption = "RRRC")
varCompROC <- temp1$ANOVA$VarCom
varCompwAFROC <- temp2$ANOVA$VarCom

## -----------------------------------------------------------------------------
print(varCompROC)
print(varCompwAFROC)

## -----------------------------------------------------------------------------
powerROC <- array(dim = length(effectSizeROC));powerwAFROC <- array(dim = length(effectSizeROC))

JTest <- 5;KTest <- 100
for (i in 1:length(effectSizeROC)) {
  varYTR <- varCompROC["VarTR","Estimates"] # these are pseudovalue based variance components assuming FOM = "Wilcoxon"
  varYTC <- varCompROC["VarTC","Estimates"]
  varYEps <- varCompROC["VarErr","Estimates"]
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizeROC[i], varYTR, varYTC, varYEps, analysisOption = "RRRC")
  powerROC[i] <- ret$powerRRRC
  
  varYTR <- varCompwAFROC["VarTR","Estimates"] # these are pseudovalue based variance components assuming FOM = "wAFROC"
  varYTC <- varCompwAFROC["VarTC","Estimates"]
  varYEps <- varCompwAFROC["VarErr","Estimates"]
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizewAFROC[i], varYTR, varYTC, varYEps, analysisOption = "RRRC")
  powerwAFROC[i] <- ret$powerRRRC
  
  cat("ROC-ES = ", effectSizeROC[i], ", wAFROC-ES = ", effectSizewAFROC[i], 
      ", Power-ROC = ", powerROC[i], ", Power-wAFROC = ", powerwAFROC[i], "\n")
}

## ---- fig.show='hold', fig.align='center'-------------------------------------
df <- data.frame(power_ROC = powerROC, power_wAFROC = powerwAFROC)
p <- ggplot(mapping = aes(x = power_ROC, y = power_wAFROC)) +
  geom_line(data = df, size = 2)+
  scale_color_manual(values = "black") + 
  theme(axis.title.y = element_text(size = 10,face="bold"),
        axis.title.x = element_text(size = 10,face="bold"))  +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)

