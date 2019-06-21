context("Output report")

test_that("UtilOutputReport text format", {
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset03, overwrite = TRUE), # ROC
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset05, overwrite = TRUE, FOM = "MaxLLF" ), # FROC
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset05, overwrite = TRUE, FOM = "MaxNLF", method = "ORH" ), # FROC
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilOutputReportExcel", {
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset03, ReportFileFormat = "xlsx", overwrite = TRUE), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilOutputReport Error", {
  fn <- system.file("extdata", "includedRocData.lrc", 
                    package = "RJafroc", mustWork = TRUE)  
  expect_error(
    UtilOutputReport(DataFileName = fn, overwrite = TRUE)) # If DataFileName is specified, then DataFileFormat must be specified
  
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData")) # If DataFileName is specified, then DataFileFormat must be specified
  
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData", DataFileFormat = "txt")) # data file format has to be JAFROC or MRMC,  or iMRMC
  
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData.tx1", DataFileFormat = "MRMC")) # data file must be *.lrc or excel
  
})

