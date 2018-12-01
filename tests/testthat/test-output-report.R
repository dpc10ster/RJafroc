context("Output report")
tmp <- tempfile()
expect_known_output(
  UtilOutputReport(dataset03, FOM = "Wilcoxon", ReportFileName = "", overwrite = TRUE), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile(fileext = ".xlsx")
expect_known_output(
  UtilOutputReport(dataset03, FOM = "Wilcoxon", ReportFileFormat = "xlsx", overwrite = TRUE), 
  tmp, print = TRUE, update = TRUE)
