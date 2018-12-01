context("Significance testing")

test_that("UtilMeanSquaresORH", {
  tmp <- tempfile()
  expect_known_output(
    UtilMeanSquares(dataset05, method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("SignificanceTestingDBMH", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("SignificanceTestingORH", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("SignificanceTestingORH", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "wAFROC", method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("StSignificanceTestingSingleFixedFactor", {
  tmp <- tempfile()
  singleFactorData <- DfExtractDataset(dataset02, 1, 1:4)
  expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "Wilcoxon"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("StSignificanceTestingCrossedModalities", {
  tmp <- tempfile()
  crossedFileName <- system.file(
    "extdata", "includedCrossedModalitiesData.xlsx", package = "RJafroc", mustWork = TRUE)
  crossedData <- DfReadCrossedModalities(crossedFileName)  
  expect_known_output(
    StSignificanceTestingCrossedModalities(datasetCrossedModality, 1), 
    tmp, print = TRUE, update = TRUE)
})

test_that("StSignificanceTestingCadVsRadiologists", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "singleModality"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("StSignificanceTestingCadVsRadiologists", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (dataset09, FOM = "Wilcoxon", method = "dualModality"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("StSignificanceTestingCadVsRadiologists", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTestingCadVsRadiologists (
      datasetCadLroc, FOM = "PCL", option = "RRRC", method = "dualModality", FPFValue = 0.05), 
    tmp, print = TRUE, update = TRUE)
})
