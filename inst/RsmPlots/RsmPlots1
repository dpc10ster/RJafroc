# RsmPlots.R
rm(list = ls())
rm(list=ls())
library(RJafroc)
set.seed(1)
K2 <- 700;Lmax <- 5;Lk2 <- floor(runif(K2, 1, Lmax + 1))
nLesPerCase <- unique(Lk2);lesDistr <- array(dim = c(length(nLesPerCase), 2))
for (i in nLesPerCase) lesDistr[i, ] <- c(i, sum(Lk2 == i)/K2)

maxLL <- max(lesDistr[,1])
# first column contains the number of lesions
# remaining columns contain the weights; equal weighting assumed
lesWghtDistr <- array(-Inf, dim = c(length(lesDistr[,1]), maxLL+1))
lesWghtDistr[,1] <- lesDistr[,1]
for (i in 1:length(lesDistr[,1])) lesWghtDistr[i,2:(lesDistr[i,1]+1)] <- 1/lesDistr[i,1]

mu <- 10;nu <- 0.99;lambda <- 1

ret1 <- PlotRsmOperatingCharacteristics(mu, lambda, nu, OpChType = "wAFROC", 
                                        lesDistr = lesDistr, lesWghtDistr = lesWghtDistr)
print(ret1$wAFROCPlot)
