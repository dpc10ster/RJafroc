context("Output report")

test_that("UtilOutputReportTxt", {
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset03, overwrite = TRUE), 
    tmp, print = TRUE, update = TRUE)

  fn <- system.file("extdata", "includedRocData.lrc", 
                    package = "RJafroc", mustWork = TRUE)  
  expect_error(
    UtilOutputReport(DataFileName = fn, overwrite = TRUE))
  
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(DataFileName = fn, DataFileFormat = "MRMC", ReportFileFormat = "txt", overwrite = TRUE), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilOutputReportExcel", {
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset03, ReportFileFormat = "xlsx", overwrite = TRUE), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilOutputReport Error", {
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData"))
})

test_that("UtilOutputReport Error", {
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData", DataFileFormat = "txt"))
})

test_that("UtilOutputReport Error", {
  expect_error(
    UtilOutputReport(
      dataset02, DataFileName = "MyData.tx1", DataFileFormat = "MRMC"))
})

test_that("UtilOutputReport Error", {
  expect_error(
    UtilOutputReport(
      dataset02, DataFileName = "MyData.txt", DataFileFormat = "JAFROC"))
})
