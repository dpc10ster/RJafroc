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

test_that("skip FitCorCbm", {
  skip_on_cran()
  skip_on_os("mac")
  skip_on_travis()
  tmp <- tempfile()
  expect_known_output(
    skip_on_os("mac"),
    FitCorCbm(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(4,7))), 
    tmp, print = TRUE, update = TRUE)
})

test_that("FitRsmRoc", {
  tmp <- tempfile()
  expect_known_output(
    FitRsmRoc(dataset02, UtilLesionDistribution(dataset02)), 
    tmp, print = TRUE, update = TRUE)
})
