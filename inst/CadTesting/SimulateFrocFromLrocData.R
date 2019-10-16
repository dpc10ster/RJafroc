library(RJafroc)
rm(list = ls())

temp <- datasetCadLroc
NL <- temp$NL
LL <- temp$LLCl

J <- length(NL[1,,1,1])
K <- length(NL[1,1,,1])
K2 <- length(LL[1,1,,1])
K1 <- K - K2

NL <- array(-Inf, dim = c(1,J,K,1))
LL <- array(-Inf, dim = c(1,J,K2,1))

for (j in 1:J) {
  for (k in 1:K1) {
    NL[1,j,k,1] <- temp$NL[1,j,k,1]
  }
}

for (j in 1:J) {
  for (k in 1:K2) {
    NL[1,j,k+K1,1] <- temp$LLIl[1,j,k,1]
  }
}

for (j in 1:J) {
  for (k in 1:K2) {
    LL[1,j,k,1] <- temp$LLCl[1,j,k,1]
  }
}

NL[NL == 0] <- -Inf
LL[LL == 0] <- -Inf

x <- temp$lesionWeight
x[,1] <- 1

dataset <- list(
  NL = NL,
  LL = LL,
  lesionVector = temp$lesionVector,
  lesionID = temp$lesionID,
  lesionWeight = x,
  dataType = "FROC",
  modalityID = temp$modalityID,
  readerID = temp$readerID
)

UtilFigureOfMerit(dataset, FOM = "wAFROC")

datasetCadSimuFroc <- dataset
save("datasetCadSimuFroc", file = "~/GitHub/RJafroc/data/datasetCadSimuFroc.RData")

# froc <- DfExtractDataset(dataset05, trts = 1)
# StSignificanceTestingCadVsRadiologists(froc, FOM = "wAFROC", method = "1T-RRRC")
# 
# roc <- DfExtractDataset(dataset02, trts = 1)
# StSignificanceTestingCadVsRadiologists(roc, FOM = "Wilcoxon", method = "1T-RRRC")



