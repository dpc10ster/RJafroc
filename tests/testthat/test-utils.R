context("Utils testing")

test_that("UtilAucBinormal", {
  a <- 2;b <- 0.7
  expect_equal(UtilAucBinormal(a,b), 0.9493375)
})

test_that("UtilAucCBM", {
  mu <- 2;alpha <- 0.8
  x <- 0.8370803
  expect_equal(UtilAucCBM(mu,alpha), x, tolerance = 1e-6, scale = x)
})

test_that("UtilAucPROPROC", {
  c1 <- .2;da <- 1.5
  x <- 0.8558655
  expect_equal(UtilAucPROPROC(c1,da), x, tolerance = 1e-6, scale = x)
})

test_that("UtilAucsRSM", {
  mu <- 1;lambdaP <- 1;nuP <- 1
  lesDistr <- rbind(c(1, 0.9), c(2, 0.1)) 
  x <- 0.8470065
  expect_equal(
    UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucROC, x, tolerance = 1e-6, scale = x)
  x <- 0.8071583
  expect_equal(
    UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucAFROC, x, tolerance = 1e-6, scale = x)
})

test_that("UtilFigureOfMerit", {
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(dataset = dataset02, FOM = "Wilcoxon"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "wAFROC"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "wAFROC1"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "AFROC1"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "MaxLLF"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "MaxNLF"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "MaxNLFAllCases"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "ExpTrnsfmSp"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "SongA2"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "SongA1"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "HrSp"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "HrSe"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "HrAuc"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(dataset = datasetCadLroc, FOM = "ALROC", FPFValue = 0.2), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilPseudoValues", {
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset05), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset02, FOM = "Wilcoxon"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset02, FOM = "MaxNLF"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset02, FOM = "ExpTrnsfmSp"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset02, FOM = "HrSp"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset02, FOM = "MaxLLF"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset02, FOM = "HrSe"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilPseudoValues(dataset02, FOM = "MaxLLF"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilMeanSquaresDBMH", {
  tmp <- tempfile()
  expect_known_output(
    UtilMeanSquares(dataset02, FOM = "Wilcoxon"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilLesionDistribution", {
  tmp <- tempfile()
  expect_known_output(
    UtilLesionWeights (UtilLesionDistribution(dataset01)), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilLesionWeights (UtilLesionDistribution(dataset02)), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    UtilLesionWeights (UtilLesionDistribution(datasetCadLroc)), 
    tmp, print = TRUE, update = TRUE)
})




