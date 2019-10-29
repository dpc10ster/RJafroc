rm(list = ls())
library(ggplot2)
library(RJafroc)

frocData <- DfExtractDataset(dataset04, trts = c(1,2))
rocData <- DfFroc2Roc(frocData)
I <- dim(frocData$NL)[1]
J <- dim(frocData$NL)[2]
lesDistr <- UtilLesionDistribution(frocData)
lesWghtDistr <- UtilLesionWeightsDistr(frocData)
RsmParms <- array(dim = c(I,J,3))
for (i in 1:I) {
  for (j in 1:J)  {
    x1 <- FitRsmRoc(rocData, trt = i, rdr = j, lesDistr)
    RsmParms[i,j,1] <- x1[[1]]
    RsmParms[i,j,2] <- x1[[2]]
    RsmParms[i,j,3] <- x1[[3]]
  }
}

muMed <- median(RsmParms[,,1]) 
lambdaPMed <- median(RsmParms[,,2])
nuPMed <- median(RsmParms[,,3])

temp <- UtilPhysical2IntrinsicRSM(muMed, lambdaPMed, nuPMed)
lambdaMed <- temp$lambda
nuMed <- temp$nu

# calculate NH values for ROC-AUC and wAFROC-AUC
aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                            lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, type = "ROC")$aucROC
aucAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                              lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, type = "wAFROC")$aucwAFROC

# following calculates effect sizes: ROC-ES and wAFROC-ES
deltaMu <- seq(0.01, 0.2, 0.01) # values of deltaMu to scan below
esRoc <- array(dim = length(deltaMu));eswAfroc <- array(dim = length(deltaMu))
for (i in 1:length(deltaMu)) {
  esRoc[i] <- PlotRsmOperatingCharacteristics(muMed + deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                lesDistr, lesWghtDistr = lesWghtDistr, type = "ROC")$aucROC - aucRocNH
  eswAfroc[i] <- PlotRsmOperatingCharacteristics(muMed+ deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                   lesDistr, lesWghtDistr = lesWghtDistr, type = "wAFROC")$aucwAFROC - aucAfrocNH
  cat("ES ROC, wAFROC = ", esRoc[i], eswAfroc[i],"\n")
}
cat("\n")

a<-lm(eswAfroc~-1+esRoc) # fit values to straight line thru origin
effectSizeROC <- seq(0.01, 0.1, 0.001)
effectSizewAFROC <- effectSizeROC*a$coefficients[1] # r2 = summary(a)$r.squared

JTest <- 5;KTest <- 100
temp1 <- StSignificanceTesting(rocData, FOM = "Wilcoxon", method = "DBMH", option = "RRRC")
temp2 <- StSignificanceTesting(frocData, FOM = "wAFROC", method = "DBMH", option = "RRRC")
varCompROC <- temp1$varComp
varCompwAFROC <- temp2$varComp

powerROC <- array(dim = length(effectSizeROC));powerwAFROC <- array(dim = length(effectSizeROC))
for (i in 1:length(effectSizeROC)) {
  varYTR <- varCompROC$varTR
  varYTC <- varCompROC$varTC
  varYEps <- varCompROC$varErr
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizeROC[i], varYTR, varYTC, varYEps, alpha  = 0.05, option = "RRRC")
  powerROC[i] <- ret$powerRRRC

  varYTR <- varCompwAFROC$varTR
  varYTC <- varCompwAFROC$varTC
  varYEps <- varCompwAFROC$varErr
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizewAFROC[i], varYTR, varYTC, varYEps, alpha  = 0.05, option = "RRRC")
  powerwAFROC[i] <- ret$powerRRRC

  cat("ROC-ES = ", effectSizeROC[i], ", wAFROC-ES = ", effectSizewAFROC[i], 
      ", Power-ROC = ", powerROC[i], ", Power-wAFROC = ", powerwAFROC[i], "\n")
}

df <- data.frame(esRoc = esRoc, eswAfroc = eswAfroc)
p <- ggplot(data = df, aes(x = esRoc, y = eswAfroc)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
  geom_point(size = 4) +
  scale_color_manual(values = "black") + 
  theme(axis.title.y = element_text(size = 25,face="bold"),
        axis.title.x = element_text(size = 30,face="bold")) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) 
print(p)

df <- data.frame(powerROC = powerROC, powerwAFROC = powerwAFROC)
p <- ggplot(mapping = aes(x = powerROC, y = powerwAFROC)) +
  geom_line(data = df, size = 2)+
  scale_color_manual(values = "black") + 
  theme(axis.title.y = element_text(size = 25,face="bold"),
        axis.title.x = element_text(size = 30,face="bold"))  +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
