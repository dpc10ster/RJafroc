context("Compare3ProperRocFits")

test_that("Compare3ProperRocFits", {
  tmp <- tempfile()
  expect_known_output(
    Compare3ProperRocFits(1,1,reAnalyze = TRUE), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    Compare3ProperRocFits(), 
    tmp, print = TRUE, update = TRUE)
})

