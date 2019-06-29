# long history behind this, see GitHub discussion with Peter
# basic lesson: dont use expect_known_output() with Compare3ProperRocFits()
test_that("Compare3ProperRocFits", {
  
  skip_on_cran()
  skip_on_travis()
    
  set.seed(1)
  fn <- paste0(test_path(), '/goodValues/Compare3ProperRocFits01.rds')
  if (!file.exists(fn)) {
    x <- Compare3ProperRocFits(1,1,reAnalyze = TRUE)
    x1 <- x; x <- x$allDatasetsResults;x <- x[[1]][[1]]$retRsm
    x[9:10] <- NULL # remove the covariance matrices ...Peter's input
    saveRDS(x, fn) # a per Peter's suggestion to  use this more portable method
  }
  
  set.seed(1)
  y <- Compare3ProperRocFits(1,1,reAnalyze = TRUE)
  y <- y$allDatasetsResults
  y <- y[[1]][[1]]$retRsm
  y[9:10] <- NULL
 
  readRDS(fn) 
  expect_equal(x, y, tolerance = 0.000001) # tolerance is important
  
})

