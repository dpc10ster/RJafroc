# rm(list = ls()) # mainOCsRaw.R
# 
# seed <- 1;set.seed(seed)
# mu <- 1;lambda <- 1;nu <- 1
# zeta1 <- -1;K1 <- 5;K2 <- 7
# Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
# 
# frocDataRaw <- SimulateFrocDataset(
#   mu = mu, lambda = lambda, nu = nu, I = 1, J = 1,
#   K1 = K1, K2 = K2, lesionNum = Lk2, zeta1 = zeta1)
# 
# rocDataRaw <- DfFroc2Roc(dataset = frocDataRaw)

# seed <- 1;set.seed(seed)
# # mu <- 0.1;lambda <- 1;nu <- 1 ;zeta1 <- -1;K1 <- 50;K2 <- 70 # Table 13.1
# mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- -1;K1 <- 5;K2 <- 7 # Column 1 in Table 13.2
# # mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- -1;K1 <- 50;K2 <- 70 # Column 2 in Table 13.2
# # mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- +1;K1 <- 50;K2 <- 70 # Column 3 in Table 13.2
# Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
# 
# frocDataRaw <- SimulateFrocDataset(
#   mu = mu, lambda = lambda, nu = nu,
#   I = 1, J = 1, K1 = K1, K2 = K2, lesionNum = Lk2, zeta1 = zeta1
# )
# frocDataBin <- DfBinDataset(frocDataRaw, opChType = "FROC")
# afrocDataBin <- DfBinDataset(frocDataRaw, opChType = "AFROC")

# require(grid)
# seed <- 10;set.seed(seed)
# mu <- 2; sigma <- 1.5; cat("Population AUC = ", pnorm(mu/sqrt(1+sigma^2)), "\n")
# K1 <- 500; K2 <- 500
# fp <- rnorm(K1);tp <- rnorm(K2, mu, sigma)
# zetas <- c(-Inf, 1.5, 2, 2.5, 3, 4, Inf)
# fp1 <- as.numeric(cut(fp, zetas));tp1 <- as.numeric(cut(tp, zetas))
# rocData1 <- Df2RJafrocDataset(fp1, tp1)
# rocData1 <- DfBinDataset(rocData1, opChType = "ROC")


# rm(list = ls())
# library(mvtnorm)
# 
# seed <- 123;set.seed(seed)
# 
# muX <- 1.5;muY <- 3
# alphaX <- 0.4;alphaY <- 0.7
# rhoNor <- 0.3;rhoAbn1 <- rhoNor;rhoAbn2 <- 0.8
# rhoAbn12 <- mean(c(rhoAbn1, rhoAbn2))
# sigmaNor <- rbind(c(1, rhoNor), c(rhoNor, 1))
# sigmaAbn1 <- rbind(c(1, rhoAbn1), c(rhoAbn1, 1))
# sigmaAbn2 <- rbind(c(1, rhoAbn2), c(rhoAbn2, 1))
# sigmaAbn12 <- rbind(c(1, rhoAbn12), c(rhoAbn12, 1))
# 
# # 50/50; 100/100; 1000/1000; 5000/5000
# K1 <- 5000;K2 <- 5000
# p200 <- (1 - alphaX) * (1 - alphaY)
# p2X0 <- alphaX * (1 - alphaY)
# p20Y <- (1 - alphaX) * alphaY
# p2XY <- alphaX * alphaY
# K2Sample <- sample(c("00", "X0", "0Y", "XY"), size = K2, replace = TRUE, prob = c(p200, p2X0, p20Y, p2XY))
# K200 <- sum(K2Sample == "00")
# K2X0 <- sum(K2Sample == "X0")
# K20Y <- sum(K2Sample == "0Y")
# K2XY <- sum(K2Sample == "XY")
# 
# zk1 <- t(rmvnorm(K1, sigma = sigmaNor))
# zk200 <- t(rmvnorm(K200, mean = c(0, 0), sigma = sigmaAbn1))
# zk2X0 <- t(rmvnorm(K2X0, mean = c(muX, 0), sigma = sigmaAbn12))
# zk20Y <- t(rmvnorm(K20Y, mean = c(0, muY), sigma = sigmaAbn12))
# zk2XY <- t(rmvnorm(K2XY, mean = c(muX, muY), sigma = sigmaAbn2))
# 
# zk2 <- cbind(zk200, zk2X0, zk20Y, zk2XY)
# 
# simuData <- Df2RJafrocDataset(zk1, zk2)
# 
# simuDataB <- DfBinDataset(simuData, opChType = "ROC")


# binned <- DfBinDataset(frocData05, opChType = "AFROC")
# PlotEmpiricalOperatingCharacteristics(binned, trts= 1, rdrs = seq(1,9), opChType = "AFROC")
# 
# binned <- DfBinDataset(frocData05, opChType = "ROC")
# PlotEmpiricalOperatingCharacteristics(binned, trts= 1, rdrs = seq(1,9), opChType = "ROC")
# 
# 

# set.seed(1)
# K1 <- 5;K2 <- 7
# FP <- rnorm(K1);TP <- rnorm(K2, mean = 2, sd = 1.5)
# dataset <- Df2RJafrocDataset(FP, TP) # generate the dataset
# datasetB <- DfBinDataset(dataset, opChType = "ROC")
# plotBin <- PlotEmpiricalOperatingCharacteristics(datasetB, trts= 1, rdrs = 1, opChType = "ROC")
# print(plotBin$Plot)
# 
# 
# set.seed(1)
# K1 <- 100;K2 <- 120
# FP <- rnorm(K1);TP <- rnorm(K2, mean = 2, sd = 1.5)
# dataset <- Df2RJafrocDataset(FP, TP) # generate the dataset
# retRaw <- PlotEmpiricalOperatingCharacteristics(dataset, trts= 1, rdrs = 1, opChType = "ROC")
# print(retRaw$Plot) # raw plot
# datasetB <- DfBinDataset(dataset, opChType = "ROC")
# plotBin <- PlotEmpiricalOperatingCharacteristics(datasetB, trts= 1, rdrs = 1, opChType = "ROC")
# print(plotBin$Plot)
# 
# ## FROC data, showing both raw and binned data
# set.seed(1)
# mu <- 1;lambda <- 1;nu <- 1; zeta1 <- 0
# K1 <- 50;K2 <- 70
# Lmax <- 2
# Lk2 <- floor(runif(K2, 1, Lmax + 1))
# frocDataRaw <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, lesionNum = Lk2)
# rocDataRaw <- DfFroc2Roc(frocDataRaw)
# afrocDataRaw <- DfFroc2Afroc(frocDataRaw)
# rocDataBin <- DfBinDataset(rocDataRaw, opChType = "ROC")
# frocDataBin <- DfBinDataset(frocDataRaw, opChType = "FROC")
# afrocDataBin <- DfBinDataset(afrocDataRaw, opChType = "AFROC")
# plotRaw <- PlotEmpiricalOperatingCharacteristics(rocDataRaw, trts = 1, rdrs = 1, opChType = "ROC")
# print(plotRaw$Plot)
# plotBin <- PlotEmpiricalOperatingCharacteristics(rocDataBin, trts = 1, rdrs = 1, opChType = "ROC")
# print(plotBin$Plot)
# plotRaw <- PlotEmpiricalOperatingCharacteristics(frocDataRaw, trts = 1, rdrs = 1, opChType = "FROC")
# print(plotRaw$Plot)
# plotBin <- PlotEmpiricalOperatingCharacteristics(frocDataBin, trts = 1, rdrs = 1, opChType = "FROC")
# print(plotBin$Plot)
# plotRaw <- PlotEmpiricalOperatingCharacteristics(afrocDataRaw, trts = 1, rdrs = 1, opChType = "AFROC")
# print(plotRaw$Plot)
# plotBin <- PlotEmpiricalOperatingCharacteristics(afrocDataBin, trts = 1, rdrs = 1, opChType = "AFROC")
# print(plotBin$Plot)