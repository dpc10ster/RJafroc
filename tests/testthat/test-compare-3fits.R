test_that("Compare3ProperRocFits", {
  # tmp <- tempfile()
  x <- Compare3ProperRocFits(1,1, reAnalyze = TRUE)
  y <- x$allDatasetsResults[[1]][[1]]$retRsm$mu
  # expect_known_output(
  #   y,
  #   tmp, print = TRUE)

  # x <- Compare3ProperRocFits(1,1, reAnalyze = TRUE)
  # y1 <- x$allDatasetsResults[[1]][[1]]$retRsm$mu
  expect_equal(y,1.781369284)
  #  tmp <- tempfile()
  # expect_known_output(
  #   y,
  #   tmp, print = TRUE)
  
})

