context("File I/O routines")

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

