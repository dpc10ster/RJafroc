context("Significance testing")

test_that("UtilMeanSquaresDBMH", {
  tmp <- tempfile()
  expect_known_output(
    UtilMeanSquares(dataset02, FOM = "Wilcoxon"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("UtilMeanSquaresORH", {
  tmp <- tempfile()
  expect_known_output(
    UtilMeanSquares(dataset05, method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("SignificanceTestingDBMH", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("SignificanceTestingORH", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("SignificanceTestingORH", {
  tmp <- tempfile()
  expect_known_output(
    StSignificanceTesting(dataset05, FOM = "wJAFROC", method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
})
