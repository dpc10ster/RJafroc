test_that("Compare3ProperRocFits", {
  
  skip_on_cran()
  skip_on_travis()
    
  set.seed(1)
  fn <- paste0(test_path(), '/goodValues/Compare3ProperRocFits01')
  if (!file.exists(fn)) {
    x <- Compare3ProperRocFits(1,1,reAnalyze = TRUE)
    x1 <- x
    x <- x$allDatasetsResults
    x <- x[[1]][[1]]$retRsm
    # remove the covariance matrices ...Peter's input
    x[9:10] <- NULL
    save(x, file = fn)
  }
  
  set.seed(1)
  y <- Compare3ProperRocFits(1,1,reAnalyze = TRUE)
  y <- y$allDatasetsResults
  y <- y[[1]][[1]]$retRsm
  y[9:10] <- NULL
 
  load(fn) 
  expect_equal(x, y, tolerance = 0.000001)
  
  # set.seed(1)
  # fn <- paste0(test_path(), '/goodValues/Compare3ProperRocFits02')
  # if (!file.exists(fn)) {
  #   expect_known_output(
  #     Compare3ProperRocFits(3,3,reAnalyze = TRUE, showPlot = TRUE),
  #     fn, print = TRUE, update = TRUE)
  # }
  # 
  # set.seed(1)
  # expect_known_output(
  #   Compare3ProperRocFits(3,3,reAnalyze = TRUE, showPlot = TRUE),
  #   fn, print = TRUE, update = FALSE, tolerance = 0.00001)
  #
  
})

