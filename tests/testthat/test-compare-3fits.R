test_that("Compare3ProperRocFits", {
  
  skip_on_cran()
  skip_on_travis()
  
  fn <- paste0(test_path(), '/tempValues/Compare3ProperRocFits01')
  if (!file.exists(fn)) {
    expect_known_output(Compare3ProperRocFits(1,1,reAnalyze = TRUE),
                        fn, print = TRUE, update = TRUE)
  }
  expect_known_output(
    Compare3ProperRocFits(1,1,reAnalyze = TRUE),
    fn, print = TRUE, update = FALSE)
  
  fn <- paste0(test_path(), '/tempValues/Compare3ProperRocFits02')
  if (!file.exists(fn)) {
    expect_known_output(
      Compare3ProperRocFits(3,3,reAnalyze = TRUE, showPlot = TRUE),
      fn, print = TRUE, update = TRUE)
  }
  
  expect_known_output(
    Compare3ProperRocFits(3,3,reAnalyze = TRUE, showPlot = TRUE),
    fn, print = TRUE, update = FALSE)
  
})

