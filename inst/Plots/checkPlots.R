library(RJafroc)

PlotEmpiricalOperatingCharacteristics(dataset04, opChType = "ROC")
PlotEmpiricalOperatingCharacteristics(dataset04, opChType = "FROC")
PlotEmpiricalOperatingCharacteristics(dataset04, opChType = "AFROC")
PlotEmpiricalOperatingCharacteristics(dataset04, opChType = "wAFROC")
PlotEmpiricalOperatingCharacteristics(dataset04, opChType = "AFROC1")
PlotEmpiricalOperatingCharacteristics(dataset04, opChType = "wAFROC1")

PlotEmpiricalOperatingCharacteristics(dataset04, trts = seq(1,5), rdrs = seq(1,4), opChType = "wAFROC", legend.position = c(0.8, 0.4))

plotT <- list(1, 2)
plotR <- list(seq(1,4), seq(1,4)) # only 4 readers
PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR,  opChType = "ROC")

set.seed(5)
K1 <- 10
K2 <- 10
mu <- 1
lambda <- 1
lesionVector <- rep(1, K2)
nu <- 0.8
zeta1 <- -3
frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, lesionVector)
lrocData <- DfFroc2Lroc(frocData)
p <- PlotEmpiricalOperatingCharacteristics(lrocData, opChType = "LROC" )
print(p)

plotT <- list(1, 2)
plotR <- list(seq(1,5), seq(1,5)) # 5 readers
PlotEmpiricalOperatingCharacteristics(lrocData, trts = plotT, rdrs = plotR,  opChType = "LROC")

