# long history behind this, see GitHub discussion with Peter
# basic lesson: dont use expect_known_output() with Compare3ProperRocFits()
# 


# > library(RJafroc)
# > devtools::test()
# Loading RJafroc
# Testing RJafroc
# ✔ |  OK F W S | Context
# ✔ |   1       | Compare3ProperRocFits routines - these have inherent randomness; hence need to use tolerance [6.9 s]
# ✔ |   2       | CORCBM routines [1.2 s]
# ✔ |  33       | Data file saving and reading routines [7.5 s]
# ✔ |  33       | Data file saving and reading routines
# ✔ |   6       | Fitting routines [92.1 s]
# ✔ |  17       | UtilFigureOfMerit tests over all datasets
# ✔ |  40       | UtilMeanSquares [1.7 s]
# ✔ |  45       | Output report [7.2 s]
# ✔ |   9       | Plotting routines [2.6 s]
# ✔ |  10       | Sample Size ROC [2.6 s]
# ✔ |  12       | ROC sample size routines [2.9 s]
# ✔ | 124       | Significance testing excluding CAD [12.4 s]
# ✔ |   3       | Simulate data sets
# ✔ |  20       | Significance testing: StSignificanceTestingCadVsRad [15.2 s]
# ✔ |  18       | utils [0.7 s]


context("Compare3ProperRocFits")
test_that("Compare3ProperRocFits routines - these have inherent randomness; hence need to use tolerance", {
  
  skip_on_cran()
  skip_on_travis()
    
  set.seed(1)
  fn <- paste0(test_path(), "/goodValues361/Compare3ProperRocFits/Compare3ProperRocFits01.rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x <- Compare3ProperRocFits(1,1,reAnalyze = TRUE)
    x1 <- x; x <- x$allDatasetsResults;x <- x[[1]][[1]]$retRsm
    x[9:10] <- NULL # remove the covariance matrices ...Peter's input
    saveRDS(x, fn) # as per Peter's suggestion to  use this more portable method
  }
  
  set.seed(1)
  y <- Compare3ProperRocFits(1,1,reAnalyze = TRUE)
  y <- y$allDatasetsResults
  y <- y[[1]][[1]]$retRsm
  y[9:10] <- NULL
 
  x <- readRDS(fn) 
  expect_equal(x, y, tolerance = 0.000001) # tolerance is important
  
})



# # following works after `warn` option in `option`` NOT set to 2 in Compare3RocFits.R
# # as per Peter Philips' bug fix
# test_that("Compare3ProperRocFits", {
#   
#   skip_on_cran()
#   skip_on_travis()
#   
#   fn <- paste0(test_path(), "/tempValues/Compare3ProperRocFits01")
#   # expect_warning(expect_known_output(
#   # expect_known_output(
#   #   # save time by using previously saved values
#   #   # reAnalyze = FALSE to use stored values
#   #   # unfortunately this causes Failures on Travis; differences are in the 6th decimal place
#   #   # which causes failure in hasg test below
#   #   # so reverted to original way
#   #   Compare3ProperRocFits(1,1,reAnalyze = TRUE),
#   #   # fn, print = TRUE, update = TRUE),
#   #   # "Creating reference output")
#   #   fn, print = TRUE, update = TRUE)
# 
#   expect_known_output(
#     # this time calculate from scratch
#     # reAnalyze = TRUE to recompute the values
#     Compare3ProperRocFits(1,1,reAnalyze = TRUE),
#     fn, print = TRUE, update = TRUE)
#   
#   fn <- paste0(test_path(), '/tempValues/Compare3ProperRocFits02')
#   # expect_known_output(
#   #   Compare3ProperRocFits(3,3,reAnalyze = FALSE, showPlot = TRUE),
#   #   fn, print = TRUE, update = TRUE)
# 
#   expect_known_output(
#     Compare3ProperRocFits(3,3,reAnalyze = FALSE, showPlot = TRUE),
#     fn, print = TRUE, update = TRUE)
#   
# })
# 
# # # alternate way of testing
# # using this causes failure on Travis; hash depends on platform?
# # test_that("known hash", {
# # 
# #   expect_known_hash(Compare3ProperRocFits(1,1,reAnalyze = TRUE), hash = '6a90170dda') # value from my machine
# #   expect_known_hash(Compare3ProperRocFits(1,1,reAnalyze = TRUE), hash = 'ee6f623095') # value from Peter
# # 
# # })
# 
