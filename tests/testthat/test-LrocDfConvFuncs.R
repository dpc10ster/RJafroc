contextStr <- "DfFroc2Lroc: LROC related Df conversion functions"
context(contextStr)
test_that(contextStr, {

  dsLroc <- DfFroc2Lroc(dataset05)
  ret1 <- UtilFigureOfMerit(dataset05, FOM = "HrAuc")
  ret2 <- UtilFigureOfMerit(dsLroc, FOM = "Wilcoxon")
  expect_equal(ret1, ret2)

  frocDataset <- DfLroc2Froc(datasetCadLroc)
  ret1 <- UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon")
  ret2 <- UtilFigureOfMerit(frocDataset, FOM = "HrAuc")
  expect_equal(ret1, ret2)

  rocDataset <- DfLroc2Roc(datasetCadLroc)
  ret1 <- UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon")
  ret2 <- UtilFigureOfMerit(rocDataset, FOM = "Wilcoxon")    
  expect_equal(ret1, ret2)

  K1 <- 5
  K2 <- 5
  mu <- 2
  lambda <- 1
  perCase <- rep(1, 5)
  nu <- 0.8
  zeta1 <- -3
  frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, perCase, seed = 1)
  lrocData <- DfFroc2Lroc(frocData)
  rocDataset <- DfLroc2Roc(lrocData)
  ret1 <- UtilFigureOfMerit(lrocData, FOM = "Wilcoxon")
  ret2 <- UtilFigureOfMerit(rocDataset, FOM = "Wilcoxon")    
  expect_equal(ret1, ret2)

})

