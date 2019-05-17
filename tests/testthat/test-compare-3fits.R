test_that("Compare3ProperRocFits", {
  tmp <- tempfile()
  expect_known_output(
    Compare3ProperRocFits(1,1,reAnalyze = TRUE), 
    tmp, print = FALSE)
  
  tmp <- tempfile()
  expect_known_output(
    Compare3ProperRocFits(1,1,reAnalyze = TRUE), 
    tmp, print = TRUE)
  
})

