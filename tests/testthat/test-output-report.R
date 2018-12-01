context("Output report")
tmp <- tempfile(fileext = ".txt")
expect_known_output(
  UtilOutputReport(dataset03, FOM = "Wilcoxon", overwrite = TRUE), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile(fileext = ".xlsx")
expect_known_output(
  UtilOutputReport(dataset03, FOM = "Wilcoxon", ReportFileFormat = "xlsx", overwrite = TRUE), 
  tmp, print = TRUE, update = TRUE)
