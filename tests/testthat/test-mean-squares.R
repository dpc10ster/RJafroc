context("Significance testing excluding CAD")

test_that("UtilMeanSquares", {
  expect_error(UtilMeanSquares(dataset05, FOM = "Wilcoxon", method = "ORH"))
  
  expect_error(UtilMeanSquares(dataset02))
  
  tmp <- tempfile()
  expect_known_output(
    UtilMeanSquares(dataset05, method = "DBMH"),
    tmp, print = TRUE, update = TRUE)

  tmp <- tempfile()
  expect_known_output(
    UtilMeanSquares(dataset05, method = "ORH"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  expect_known_output(
    UtilMeanSquares(dataset02, method = "ORH"),
    tmp, print = TRUE, update = TRUE)
})


