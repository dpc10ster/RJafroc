context("Significance testing excluding CAD")

test_that("SignificanceTestingDBMH", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, option = "RRRC"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, option = "FRRC"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, option = "RRFC"),
    tmp, print = TRUE, update = TRUE)

  ds <- DfExtractDataset(dataset02, rdrs = 1) # to test single reader dataset
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(ds),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, VarCompFlag = TRUE),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(datasetROI),
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
    StSignificanceTesting(dataset05, FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "MaxNLF"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "ExpTrnsfmSp"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "HrSp"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "MaxLLF"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "HrSe"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "MaxNLF", method = "ORH"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "MaxLLF", method = "ORH"),
    tmp, print = TRUE, update = TRUE)
  
  skip_on_cran()
  skip_on_travis()
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "MaxNLF", covEstMethod = "Bootstrap", method = "ORH"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "MaxLLF", covEstMethod = "Bootstrap", method = "ORH"),
    tmp, print = TRUE, update = TRUE)
  
  expect_error(StSignificanceTesting(dataset05, FOM = "MaxNLF", covEstMethod = "Bootstrap", method = "DBMH"))
})


test_that("StSignificanceTestingSingleFixedFactor", {
  tmp <- tempfile()
  singleFactorData <- DfExtractDataset(dataset02, 1, 1:4)
  expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "Wilcoxon"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  singleFactorData <- DfExtractDataset(dataset05, 1, 1:4)
  expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  singleFactorData <- DfExtractDataset(dataset05, 1:2, 4)
  expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "wAFROC"),
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

