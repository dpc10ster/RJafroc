rm(list = ls())

library(RJafroc)

source("inst/obumrm2/obu.R")
source("inst/obumrm2/covariance.R")

ds1 <- SimulateRocDataset(I = 2, J = 5, K1 = 25, K2 = 25, 
                          a = 2, deltaA = 0.1, b = 0.8, seed = 201)
ds2 <- SimulateRocDataset(I = 2, J = 5, K1 = 25, K2 = 25, 
                          a = 2, deltaA = 0.1, b = 0.8, seed = 202)


FOM <- "Wilcoxon"

p_fstar_H <- obu (ds1, ds2, FOM)
  
st1 <- StSignificanceTesting(ds1, FOM = FOM, method = "OR")
st2 <- StSignificanceTesting(ds2, FOM = FOM, method = "OR")

cat("p-val first dataset = ", st1$RRRC$FTests$p[1], "\n")
cat("p-val second dataset = ", st2$RRRC$FTests$p[1], "\n")
cat("p-val combined dataset = ", p_fstar_H, "\n")
# Expected output: dataset02 and dataset03
# p-val first dataset =  0.051665686 
# p-val second dataset =  0.11883786 
# p-val combined dataset =  0.1963458
# 
# Expected output: simulation
# ds1 <- SimulateRocDataset(I = 2, J = 5, K1 = 50, K2 = 50, 
#                           a = 2, deltaA = 0.2, b = 0.8, seed = 201)
# ds2 <- SimulateRocDataset(I = 2, J = 5, K1 = 50, K2 = 50, 
#                           a = 2, deltaA = 0.2, b = 0.8, seed = 202)
#                           
# # p-val first dataset =  0.006590623 
# p-val second dataset =  0.00022195508 
# p-val combined dataset =  4.549312e-06 
# ds1 <- SimulateRocDataset(I = 2, J = 5, K1 = 25, K2 = 25, 
#                           a = 2, deltaA = 0.1, b = 0.8, seed = 201)
# ds2 <- SimulateRocDataset(I = 2, J = 5, K1 = 25, K2 = 25, 
#                           a = 2, deltaA = 0.1, b = 0.8, seed = 202)
# > source("~/GitHub/RJafroc/inst/obumrm2/ObuchIdea.R")
# p-val first dataset =  0.013762708 
# p-val second dataset =  0.020637774 
# p-val combined dataset =  9.6753196e-05 