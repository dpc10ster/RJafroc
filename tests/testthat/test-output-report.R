context("Output report")

test_that("UtilOutputReportTxt", {
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset03, FOM = "Wilcoxon", overwrite = TRUE), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilOutputReportExcel", {
  tmp <- tempfile()
  expect_known_output(
    UtilOutputReport(dataset03, FOM = "Wilcoxon", ReportFileFormat = "xlsx", overwrite = TRUE), 
    tmp, print = TRUE, update = TRUE)
})
