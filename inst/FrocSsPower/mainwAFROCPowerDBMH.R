rm(list = ls()) #mainwAFROCPowerDBMH.R # LOCK lines numbers
library(ggplot2)
library(RJafroc)

# included datasets
fileNames <-  c("TONY", "VD", "FR", "FED", "JT", "MAG", "OPT", "PEN", "NICO",
                "RUS", "DOB1", "DOB2", "DOB3", "FZR")
fileName <- fileNames[fileNames == "FED"]
retFileName <- paste0("allResults", fileName) 
sysAnalFileName <- system.file("ANALYZED/RSM6", retFileName, package = "RJafroc")
if (file.exists(sysAnalFileName)){
  load(sysAnalFileName)
  I <- allResults[[1]]$I
  J <- allResults[[1]]$J
  K1 <- allResults[[1]]$K1
  K2 <- allResults[[1]]$K2
  K <- K1 + K2
} else {
  stop("Results file does not exist; need to run FitRsmRoc() to obtain fitted RSM parameters.")
}

mu <- array(dim = c(I,J));lambdaP <- array(dim = c(I,J)); nuP <- array(dim = c(I,J))

AllResIndx <- 0
for (i in 1:I){
  for (j in 1:J) {
    AllResIndx <- AllResIndx + 1
    mu[i,j] <- allResults[[AllResIndx]]$retRsm$mu
    lambdaP[i,j] <- allResults[[AllResIndx]]$retRsm$lambdaP
    nuP[i,j] <- allResults[[AllResIndx]]$retRsm$nuP
  }
}

muMed <- median(mu[1:2,]) # instead of average, use median to get representative value over whole dataset
nuMed <- median(nuP[1:2,]) # do:
lambdaMed <- median(lambdaP[1:2,]) # do:

lesDistr <- allResults[[1]]$lesDistr
# construct lesion weights, assuming equally weighted lesions
lesWghtDistr <- matrix(-Inf, nrow = nrow(lesDistr), ncol = nrow(lesDistr)+1)
lesWghtDistr[,1] <- lesDistr[,1]
for (i in 1:length(lesDistr[,1])) lesWghtDistr[i,2:(lesDistr[i,1]+1)] <- 1/lesDistr[i,1]
# calculate NH values for ROC-AUC and wAFROC-AUC
aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                            lesDistr = lesDistr, lesWghtDistr = lesWghtDistr,  OpChType = "ROC")$aucROC
aucAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                              lesDistr = lesDistr, lesWghtDistr = lesWghtDistr,  OpChType = "wAFROC")$aucwAFROC

# following calculates effect sizes: ROC-ES and wAFROC-ES
deltaMu <- seq(0.01, 0.2, 0.01) # values of deltaMu to scan below
esRoc <- array(dim = length(deltaMu));eswAfroc <- array(dim = length(deltaMu))
for (i in 1:length(deltaMu)) {
  esRoc[i] <- PlotRsmOperatingCharacteristics(muMed + deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                lesDistr, lesWghtDistr = lesWghtDistr,  OpChType = "ROC")$aucROC - aucRocNH
  eswAfroc[i] <- PlotRsmOperatingCharacteristics(muMed+ deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                   lesDistr, lesWghtDistr = lesWghtDistr,  OpChType = "wAFROC")$aucwAFROC - aucAfrocNH
  cat("ES ROC, wAFROC = ", esRoc[i], eswAfroc[i],"\n")
}
cat("\n")

a<-lm(eswAfroc~-1+esRoc) # fit values to straight line thru origin
effectSizeROC <- seq(0.01, 0.1, 0.001)
effectSizewAFROC <- effectSizeROC*a$coefficients[1] # r2 = summary(a)$r.squared

frocData <- get(sprintf("dataset%02d", 4))
JTest <- 5;KTest <- 100
rocData <- DfFroc2Roc(frocData)
varCompROC <- StSignificanceTesting(rocData, FOM = "Wilcoxon", method = "DBMH", option = "RRRC")$varComp
varCompwAFROC <- StSignificanceTesting(frocData, FOM = "wAFROC", method = "DBMH", option = "RRRC")$varComp

cat("JTest = ", JTest, "KTest = ", KTest, "\n")
powerROC <- array(dim = length(effectSizeROC));powerwAFROC <- array(dim = length(effectSizeROC))
for (i in 1:length(effectSizeROC)) {
  varYTR <- varCompROC$varTR
  varYTC <- varCompROC$varTC
  varYEps <- varCompROC$varErr
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizeROC[i], varYTR, varYTC, varYEps, option = "RRRC")
  powerROC[i] <- ret$powerRRRC

  varYTR <- varCompwAFROC$varTR
  varYTC <- varCompwAFROC$varTC
  varYEps <- varCompwAFROC$varErr
  ret <- SsPowerGivenJKDbmVarComp (J = JTest, K = KTest, effectSize = effectSizewAFROC[i], varYTR, varYTC, varYEps, option = "RRRC")
  powerwAFROC[i] <- ret$powerRRRC

  cat("ROC effect size = ", effectSizeROC[i], ", wAFROC effect size = ", effectSizewAFROC[i], 
      ", Statistical power ROC, wAFROC: ", powerROC[i], ", ", powerwAFROC[i], "\n")
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
