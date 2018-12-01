context("Data file routines")

test_that("DfReadDataFileJAFROC", {
  tmp <- tempfile()
  fileName <- system.file("extdata", "includedRocData.xlsx", 
                          package = "RJafroc", mustWork = TRUE)
  expect_known_output(
    DfReadDataFile(fileName), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfReadDataFileMRMC", {
  tmp <- tempfile()
  fileName <- system.file("extdata", "includedRocData.csv", 
                          package = "RJafroc", mustWork = TRUE)
  expect_known_output(
    DfReadDataFile(fileName, format = "MRMC"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("Df2RJafrocDataset", {
  set.seed(1)
  NL <- rnorm(5)
  LL <- rnorm(7)*1.5 + 2
  tmp <- tempfile()
  expect_known_output(
    Df2RJafrocDataset(NL, LL), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfFroc2Afroc", {
  tmp <- tempfile()
  expect_known_output(
    DfFroc2Afroc(dataset05), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfFroc2Roc", {
  tmp <- tempfile()
  expect_known_output(
    DfFroc2Roc(dataset05), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfReadCrossedModalities", {
  crossedFileName <- system.file(
    "extdata", 
    "includedCrossedModalitiesData.xlsx", 
    package = "RJafroc", 
    mustWork = TRUE)
  tmp <- tempfile()
  expect_known_output(
    DfReadCrossedModalities(crossedFileName), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfExtractDataset", {
  tmp <- tempfile()
  expect_known_output(
    DfExtractDataset(dataset05, rdrs = c(1, 3)), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfSaveDataFile", {
  tmp <- tempfile(fileext = ".xlsx")
  expect_known_output(
    DfSaveDataFile(dataset = dataset05, fileName = "rocData2.xlsx", format = "JAFROC"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfSaveDataFile", {
  tmp <- tempfile(fileext = ".xlsx")
  expect_known_output(
    DfSaveDataFile(dataset = dataset02, fileName = "rocData2.csv", format = "MRMC"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfSaveDataFile", {
  tmp <- tempfile(fileext = ".xlsx")
  expect_known_output(
    DfSaveDataFile(dataset = dataset02, fileName = "rocData2.lrc", format = "MRMC", 
                   dataDescription = "ExampleROCdata1"), 
    tmp, print = TRUE, update = TRUE)
})



