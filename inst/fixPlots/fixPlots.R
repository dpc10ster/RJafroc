rm(list = ls())
library(RJafroc)
library(abind)
library(ggplot2)

seed <- 1
set.seed(seed)
# these parameters do not change between RAD and CAD observers
nu <- 1
lambda <- 1
K1 <- 5
K2 <- 7
Lmax <- 2
Lk2 <- floor(runif(K2, 1, Lmax + 1))

muCad <- 1.0
muRad <- 1.5
# these are CAD parameters
mu <- muCad
zeta1 <- -Inf
cat("constant parameters:",
    "\nseed = ", seed,
    "\nnu = ", nu,
    "\nlambda = ", lambda,
    "\nK1 = ", K1,
    "\nK2 = ", K2,"\n")
cat("CAD parameters:",
    "\nmu = ", mu,
    "\nzeta1 = ", zeta1 ,"\n")

seed <- 1
set.seed(seed)
frocCad <- SimulateFrocDataset(
  mu = mu,
  lambda = lambda,
  nu = nu,
  I = 1,
  J = 1,
  K1 = K1,
  K2 = K2,
  perCase = Lk2,
  zeta1 = zeta1)
# these are RAD parameters
mu <- muRad
zeta1 <- -Inf
cat("RAD parameters:",
    "\nmu = ", mu,
    "\nzeta1 = ", zeta1 ,"\n")

seed <- 1
set.seed(seed)
frocRad <- SimulateFrocDataset(
  mu = mu,
  lambda = lambda,
  nu = nu,
  I = 1,
  J = 1,
  K1 = K1,
  K2 = K2,
  perCase = Lk2,
  zeta1 = zeta1)
numNLCad <- dim(frocCad$ratings$NL)[4]
numNLRad <- dim(frocRad$ratings$NL)[4]
# the max number of NLs per case in the combined dataset
numNL <- max(numNLCad, numNLRad)
if (numNLCad < numNL){
  # dataset CAD has smaller number of NLs
  # add more -Inf NLs to make
  # the number of NL in two datasets consistent
  NL1 <- abind(
    frocCad$ratings$NL,
    array(-Inf, dim = c(1, 1, K1 + K2, numNL - numNLCad)))
  # combine the two NLs
  NLComb <- abind(
    NL1,
    frocRad$ratings$NL, along = 2)
} else if (numNLRad < numNL){
  # dataset RAD has smaller number of NLs
  # add more -Inf NLs to make
  # the number of NL in two datasets consistent
  NL1 <- abind(
    frocRad$ratings$NL,
    array(-Inf, dim = c(1, 1, K1 + K2, numNL - numNLRad)))
  NLComb <- abind(
    frocCad$ratings$NL,
    NL1, along = 2)
} else {
  # the number of NLs in the two datasets
  # are same, combine them directly
  NLComb <- abind(
    frocCad$ratings$NL,
    frocRad$ratings$NL,
    along = 2)
}

# combine the two LLs
LLComb <- abind(
  frocCad$ratings$LL,
  frocRad$ratings$LL,
  along = 2)

# convert the the combined NLs
# and LLs to an RJafroc dataset
frocComb <- Df2RJafrocDataset(
  NL = NLComb,
  LL = LLComb,
  InputIsCountsTable = FALSE,
  perCase = Lk2)
frocComb$descriptions$readerID <- c("CAD", "RAD")

froc <- PlotEmpiricalOperatingCharacteristics(
  frocComb,
  trts= 1,
  rdrs = c(1, 2),
  opChType = "FROC")
print(froc$Plot)
