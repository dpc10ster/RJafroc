# context("CheckLrocFomsVsHandCalculatedValues4")
# test_that("Check Lroc Fom Vs Hand Calculation from first principles in ChkLrocFoms.xlsx: dataset 4", {
# 
#   set.seed(1)
#   K1 <- 5
#   K2 <- 5
#   mu <- 2
#   lambda <- 1
#   perCase <- rep(1, 5)
#   nu <- 0.8
#   zeta1 <- -3
#   frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, perCase, seed = 1)
#   lrocData <- DfFroc2Lroc(frocData)
#   
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.05))
#   expect_equal(as.vector(ret), c(0.8, 1.0, 0.4, 0.8, 0.4, 0.8, 0.6, 0.6, 0.8, 0.4))
#   
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.05))
#   expect_equal(as.vector(ret), c(0.04, 0.05, 0.02, 0.04, 0.02, 0.04, 0.03, 0.03, 0.04, 0.02))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.2))
#   expect_equal(as.vector(ret), c(0.8, 1.0, 0.4, 0.8, 0.4, 0.8, 0.6, 0.6, 0.8, 0.4))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.2))
#   expect_equal(as.vector(ret), c(0.16, 0.20, 0.08, 0.16, 0.08, 0.16, 0.12, 0.12, 0.16, 0.08))
# })
# 
# context("CheckLrocFomsVsHandCalculatedValues2")
# test_that("Check Lroc Fom Vs Hand Calculation from first principles in ChkLrocFoms.xlsx: dataset 2", {
# 
#   set.seed(2)
#   K1 <- 5
#   K2 <- 5
#   mu <- 2
#   lambda <- 2
#   perCase <- rep(1, 5)
#   nu <- 0.5
#   zeta1 <- -3
#   frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, perCase, seed = 2)
#   lrocData <- DfFroc2Lroc(frocData)
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.05))
#   expect_equal(as.vector(ret), c(0.2, 0.4, 0.6, 0.6, 0.2, 0.2, 0.2, 0.4, 0.6, 0.2))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.05))
#   expect_equal(as.vector(ret), c(0.01, 0.02, 0.03, 0.03, 0.01, 0.01, 0.01, 0.02, 0.03, 0.01))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.1))
#   expect_equal(as.vector(ret), c(0.2, 0.4, 0.6, 0.6, 0.2, 0.2, 0.2, 0.4, 0.6, 0.2))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.1))
#   expect_equal(as.vector(ret), c(0.02, 0.04, 0.06, 0.06, 0.02, 0.02, 0.02, 0.04, 0.06, 0.02))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.2))
#   expect_equal(as.vector(ret), c(0.2, 0.4, 0.6, 0.6, 0.2, 0.2, 0.2, 0.4, 0.6, 0.2))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.2))
#   expect_equal(as.vector(ret), c(0.04, 0.08, 0.12, 0.12, 0.04, 0.04, 0.04, 0.08, 0.12, 0.04))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.6))
#   expect_equal(as.vector(ret), c(0.6, 1.0, 1.0, 0.8, 0.4, 0.2, 0.2, 0.6, 0.6, 0.2))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.6))
#   expect_equal(as.vector(ret), c(0.20, 0.48, 0.52, 0.44, 0.20, 0.12, 0.12, 0.32, 0.36, 0.12))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 1))
#   expect_equal(as.vector(ret), c(0.6, 1.0, 1.0, 0.8, 0.6, 0.2, 0.2, 0.6, 0.6, 0.2))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 1))
#   expect_equal(as.vector(ret), c(0.44, 0.88, 0.92, 0.76, 0.44, 0.20, 0.20, 0.56, 0.60, 0.20))
# 
#   ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "Wilcoxon"))
#   expect_equal(as.vector(ret), c(0.64, 0.88, 0.92, 0.84, 0.62, 0.60, 0.56, 0.84, 0.76, 0.70))
# })
# 
# 
