library(RJafroc)
rm(list = ls())
# x <- dataset01
# x <- dataset02
# x <- dataset03
# x <- dataset04
# x <- dataset05
# x <- dataset06
# x <- dataset07
# x <- dataset08
# x <- dataset09
# x <- dataset10
# x <- dataset11
# x <- dataset12
# x <- dataset13
# x <- dataset14
x <- dataset04
I <- length(x$modalityID)
J <- length(x$readerID)
K <- length(x$NL[1,1,,1])
K2 <- length(x$LL[1,1,,1])
K1 <- K - K2

truthTableStr <- array(dim = c(I, J, K, max(x$lesionVector)+1))
truthTableStr[,,1:K1,1] <- 1
for (k2 in 1:K2) {
  truthTableStr[,,k2+K1,(1:x$lesionVector[k2])+1] <- 1
}

# x1 <- system.file("extdata", "datasets/Vortex/CXRinvisible3-20mm.xlsx",
#                           package = "RJafroc", mustWork = TRUE)
# x2 <- DfReadDataFile(x1, newExcelFileFormat = FALSE)
x1 <- system.file("extdata", "datasets/FZ_ALL.xlsx",
                  package = "RJafroc", mustWork = TRUE)
x2 <- DfReadDataFile(x1, newExcelFileFormat = TRUE)

# ds <- list(
#   NL = x$NL, 
#   LL = x$LL, 
#   lesionVector  = x$lesionVector , 
#   lesionID = x$lesionID, 
#   lesionWeight = x$lesionWeight, 
#   dataType = x$dataType, 
#   modalityID = x$modalityID, 
#   readerID = x$readerID,
#   design = "CROSSED", # default when using old read function
#   normalCases = x2$normalCases,
#   abnormalCases = x2$abnormalCases,
#   # normalCases = 1:K1,
#   # abnormalCases = (K1+1):K,
#   truthTableStr = truthTableStr)
# 
# dataset04 <- ds
testthat::expect_equal(dataset04, x2)
# save("dataset14", file = "~/GitHub/RJafroc/data/dataset14.RData")

