context("Significance testing excluding CAD")

test_that("UtilMeanSquares", {
  expect_error(UtilMeanSquares(dataset05, FOM = "Wilcoxon", method = "ORH"))
  
  expect_error(UtilMeanSquares(dataset01))
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilMeanSquares(dataset05, method = "DBMH", FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")

  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilMeanSquares(dataset05, method = "ORH", FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilMeanSquares(dataset02, FOM = "Wilcoxon", method = "ORH"),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
})


