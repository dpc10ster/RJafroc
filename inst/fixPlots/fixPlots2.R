rm(list = ls())
library(RJafroc)
library(ggplot2)

seed <- 1
set.seed(seed)
K1 <- 5
K2 <- 7
Lmax <- 2
Lk2 <- floor(runif(K2, 1, Lmax + 1))

mu <- 1.0
nu <- 1
lambda <- 1
zeta1 <- 0

for (i in 1:4) {
  if (i == 1) {
    I <- 1
    J <- 1
  } else if (i == 2) {
    I <- 1
    J <- 2
  } else if (i == 3) {
    I <- 2
    J <- 1
  } else if (i == 4) {
    I <- 2
    J <- 2
  }
  seed <- 1
  set.seed(seed)
  frocCad <- SimulateFrocDataset(
    mu = mu,
    lambda = lambda,
    nu = nu,
    I = I,
    J = J,
    K1 = K1,
    K2 = K2,
    perCase = Lk2,
    zeta1 = zeta1)
  
  
  froc <- PlotEmpiricalOperatingCharacteristics(
    frocCad,
    trts = seq(1:I),
    rdrs= seq(1:J),
    opChType = "FROC", maxDiscrete = 30)
  print(froc$Plot)
}