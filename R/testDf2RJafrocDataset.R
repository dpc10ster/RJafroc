# fp <- c(rep(1,7), rep(2, 3))
# tp <- c(rep(1,5), rep(2, 5))
# ret1 <- Df2RJafrocDataset(fp, tp) # ROC
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret1, trts = 1, rdrs = 1)
# print(retPlot$Plot)
# 
# set.seed(1)
# NL <- rnorm(5)
# LL <- rnorm(7)*1.5 + 2
# ret1 <- Df2RJafrocDataset(NL, LL)
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret1, trts = 1, rdrs = 1)
# print(retPlot$Plot)
# 
# J <- 3;set.seed(1)
# K1 <- 25;K2 <- 35
# z1 <- array(dim = c(J, K1))
# z2 <- array(dim = c(J, K2))
# mu <- 2;sigma <- 1.5
# for (j in 1:J) {
#   z1[j,1:K1] <- rnorm(K1)
#   z2[j,] <- rnorm(K2) * sigma + mu
# }
# ret <- Df2RJafrocDataset(z1, z2)
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret, trts = 1, rdrs = seq(1:J), opChType = "ROC")
# print(retPlot$Plot)
# 
# 
# I <- 2;J <- 3;set.seed(1)
# K1 <- 25;K2 <- 35
# z1 <- array(dim = c(I, J, K1))
# z2 <- array(dim = c(I, J, K2))
# mu <- 2;sigma <- 1.5
# for (i in 1:I) {
#   for (j in 1:J) {
#     z1[i,j,1:K1] <- rnorm(K1)
#     z2[i,j,] <- rnorm(K2) * sigma + mu
#   }
# }
# ret <- Df2RJafrocDataset(z1, z2) ## note absence of lesionNum
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret, trts = seq(1:I), rdrs = seq(1:J), opChType = "ROC")
# print(retPlot$Plot)
# 
# set.seed(1)
# mu <- 1;lambda <- 1;nu <- 1; zeta1 <- 0
# K1 <- 5;K2 <- 7
# Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
# frocDataRaw <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, lesionNum = Lk2)
# NL <- drop(frocDataRaw$NL)
# LL <- drop(frocDataRaw$LL)    
# ret1 <- Df2RJafrocDataset(NL, LL, lesionNum = Lk2) ## note presence of lesionNum
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret1, trts = 1, rdrs = 1, opChType = "AFROC")
# print(retPlot$Plot)
# 
# I <- 2;J <- 3;set.seed(1)
# K1 <- 25;K2 <- 35
# mu <- 1;nuP <- 0.8;lambdaP <- 1;zeta1 <- 0
# lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
# nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
# Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
# z1 <- array(-Inf,dim = c(I,J,K1+K2,40))
# z2 <- array(-Inf,dim = c(I,J,K2,40))
# dimNL <- array(dim=c(I,J,2)) # the last value (2) accommodates case and location indices
# dimLL <- array(dim=c(I,J,2))
# for (i in 1:I) {
#   for (j in 1:J) {
#     frocDataRaw <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, lesionNum = Lk2)
#     dimNL[i,j,] <- dim(drop(frocDataRaw$NL))
#     dimLL[i,j,] <- dim(drop(frocDataRaw$LL))
#     z1[i,j,,1:dimNL[i,j,2]] <- drop(frocDataRaw$NL) # drop the excess location indices
#     z2[i,j,,1:dimLL[i,j,2]] <- drop(frocDataRaw$LL)
#   }
# }
# z1 <- z1[,,,1:max(dimNL[,,2])]
# z2 <- z2[,,,1:max(dimLL[,,2])]
# 
# ret3 <- Df2RJafrocDataset(z1, z2, lesionNum = Lk2) 
# 
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret3, trts = seq(1,I), rdrs = seq(1,J), opChType = "ROC")
# print(retPlot$Plot)
# 
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret3, trts = seq(1,I), rdrs = seq(1,J), opChType = "FROC")
# print(retPlot$Plot)
# 
# retPlot <- PlotEmpiricalOperatingCharacteristics(ret3, trts = seq(1,I), rdrs = seq(1,J), opChType = "AFROC")
# print(retPlot$Plot)
# 
