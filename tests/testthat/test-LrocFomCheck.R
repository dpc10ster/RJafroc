# context("CheckLrocFomsVsHandCalculatedValues4")
test_that("Check Lroc Fom Vs Hand Calculation from first principles in ChkLrocFoms.xlsx: dataset 4", {
# 
# 
  K1 <- 5
  K2 <- 5
  mu <- 2
  lambda <- 1
  perCase <- rep(1, 5)
  nu <- 0.8
  zeta1 <- -3
  frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, perCase, seed = 1)
  lrocData <- DfFroc2Lroc(frocData)

  ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.05))
  expect_equal(as.vector(ret), c(0.2, 0.6, 0.8, 1.0, 0.6, 0.6, 0.8, 0.8, 0.4, 0.6))

  ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.05))
  expect_equal(as.vector(ret), c(0.01, 0.03, 0.04, 0.05, 0.03, 0.03, 0.04, 0.04, 0.02, 0.03))

  ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 0.2))
  expect_equal(as.vector(ret), c(0.2, 0.6, 0.8, 1.0, 0.6, 0.6, 0.8, 0.8, 0.4, 0.6))

  ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 0.2))
  expect_equal(as.vector(ret), c(0.04, 0.12, 0.16, 0.20, 0.12, 0.12, 0.16, 0.16, 0.08, 0.12))
  
  ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "PCL", FPFValue = 1))
  expect_equal(as.vector(ret), c(0.2, 0.8, 0.8, 1.0, 0.6, 0.8, 0.8, 0.8, 0.8, 0.6))
  
  ret <- as.matrix(UtilFigureOfMerit(lrocData, FOM = "ALROC", FPFValue = 1))
  expect_equal(as.vector(ret), c(0.20, 0.76, 0.80, 1.00, 0.60, 0.76, 0.80, 0.80, 0.64, 0.60))
})

