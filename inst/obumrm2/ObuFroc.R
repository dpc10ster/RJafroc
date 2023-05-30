rm(list = ls())

library(RJafroc)

source("inst/obumrm2/obu.R")
source("inst/obumrm2/covariance.R")

set.seed(3)
K1 <- 65;K2 <- 70
I <- 2;J <- 5
maxLL <- 3;perCase <- floor(runif(K2, 1, maxLL + 1))
mu <- 2;lambda <- 1;nu <- 0.9;zeta1 <- -1

ds1 <- SimulateFrocDataset(
  mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
  I = I, J = J, K1 = K1, K2 = K2, perCase = perCase, seed = 1, deltaMu = 0.2)

K1 <- 50;K2 <- 55
maxLL <- 2;perCase <- floor(runif(K2, 1, maxLL + 1))

ds2 <- SimulateFrocDataset(
  mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
  I = I, J = J, K1 = K1, K2 = K2, perCase = perCase, seed = 1, deltaMu = 0.2)


FOM <- "wAFROC"

p_fstar_H <- obu (ds1, ds2, FOM)

st1 <- StSignificanceTesting(ds1, FOM = FOM, method = "OR")
st2 <- StSignificanceTesting(ds2, FOM = FOM, method = "OR")

cat("p-val first dataset = ", st1$RRRC$FTests$p[1], "\n")
cat("p-val second dataset = ", st2$RRRC$FTests$p[1], "\n")
cat("p-val combined dataset = ", p_fstar_H, "\n")

# Expected output
# set.seed(1)
# K1 <- 65;K2 <- 70
# I <- 2;J <- 5
# maxLL <- 3;perCase <- floor(runif(K2, 1, maxLL + 1))
# mu <- 2;lambda <- 1;nu <- 0.9;zeta1 <- -1
# K1 <- 50;K2 <- 55
# maxLL <- 2;perCase <- floor(runif(K2, 1, maxLL + 1))
# > source("~/GitHub/RJafroc/inst/obumrm2/ObuIdeaFroc.R")
# p-val first dataset =  0.0056107668 
# p-val second dataset =  0.13090664 
# p-val combined dataset =  0.0027009647 

# set.seed(2)
# > source("~/GitHub/RJafroc/inst/obumrm2/ObuIdeaFroc.R")
# p-val first dataset =  0.1556525 
# p-val second dataset =  0.063609818 
# p-val combined dataset =  0.018315812 

# set.seed(3)
# > source("~/GitHub/RJafroc/inst/obumrm2/ObuIdeaFroc.R")
# p-val first dataset =  0.057864639 
# p-val second dataset =  0.047109361 
# p-val combined dataset =  0.0024507821 
