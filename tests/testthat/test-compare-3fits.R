# test_that("mtcarsExample - THIS WORKS", {
#   tmp <- tempfile()
# 
#   # The first run always succeeds
#   expect_known_output(mtcars[1:10, ], tmp, print = TRUE)
# 
#   # Subsequent runs will suceed only if the file is unchanged
#   # This will succeed:
#   expect_known_output(mtcars[1:10, ], tmp, print = TRUE)
# 
# })

test_that("Compare3ProperRocFits - THIS DOES NOT WORK", {
  tmp <- tempfile(tmpdir = paste0(getwd(),"/tests/testthat"))
  tmp <- tempfile()
  
  #The first run always succeeds
  y <- Compare3ProperRocFits(1,1,reAnalyze = FALSE)$allDatasetsResults[[1]][[1]]$retRsm$mu
  expect_known_output(y, tmp, print = TRUE)

  # Subsequent runs will suceed only if the file is unchanged
  # This will succeed:
  y <- Compare3ProperRocFits(1,1,reAnalyze = FALSE)$allDatasetsResults[[1]][[1]]$retRsm$mu
  expect_known_output(y, tmp, print = TRUE)

})



test_that("known hash - THIS WORKS", {
  
  expect_known_hash(Compare3ProperRocFits(1,1,reAnalyze = FALSE), hash = '6a90170dda')
  
})

