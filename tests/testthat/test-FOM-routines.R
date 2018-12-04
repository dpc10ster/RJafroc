context("UtilFigureOfMerit tests")

test_that("ROI paradigm", {
  fom <- UtilFigureOfMerit(datasetROI, FOM = "ROI")
  expect_equal(fom[1,3], 0.8579279, tolerance = 1e-7)
  
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(datasetROI, FOM = "ROI"), 
    tmp, print = TRUE, update = TRUE)
  
})


test_that("LROC FOM tests", {
  fom <- UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon")
  expect_equal(fom[6], 0.7686979, tolerance = 1e-7)
  
  fom <- UtilFigureOfMerit(datasetCadLroc, FOM = "PCL", FPFValue = 0.2)
  expect_equal(fom[6], 0.6598214, tolerance = 1e-7)
  
  fom <- UtilFigureOfMerit(datasetCadLroc, FOM = "ALROC")
  expect_equal(fom[6], 0.1000335, tolerance = 1e-6)
  
  tmp <- tempfile()
  expect_known_output(
    UtilFigureOfMerit(datasetCadLroc, FOM = "ALROC"), 
    tmp, print = TRUE, update = TRUE)
})


test_that("ROC and FROC FOMs", {
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
})
