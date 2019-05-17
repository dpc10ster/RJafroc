context("Fitting routines")

test_that("FitBinormalRoc", {
  tmp <- tempfile()
  expect_known_output(
    FitBinormalRoc(dataset02), 
    tmp, print = TRUE, update = TRUE)
})

test_that("FitBinormalRoc", {
  tmp <- tempfile()
  expect_known_output(
    FitBinormalRoc(DfBinDataset(dataset05, desiredNumBins = 5, opChType = "ROC")), 
    tmp, print = TRUE, update = TRUE)
})

test_that("FitCbmRoc", {
  tmp <- tempfile()
  expect_known_output(
    FitCbmRoc(dataset02), 
    tmp, print = TRUE, update = TRUE)
})

test_that("FitCorCbm", {
  skip_on_travis()
  skip_on_cran()
  tmp <- tempfile()
  expect_known_output(
    FitCorCbm(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(4,7))), 
    tmp, print = TRUE, update = TRUE)
})

test_that("FitRsmRoc", {
  tmp <- tempfile()
  expect_known_output(
    FitRsmRoc(dataset02, UtilLesionDistribution(dataset02)), 
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    FitRsmRoc(datasetDegenerate, UtilLesionDistribution(datasetDegenerate)), 
    tmp, print = TRUE, update = TRUE)
})
