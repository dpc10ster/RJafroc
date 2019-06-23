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

# following works after warn NOT set to 2 in Compare3RocFits.R
# as per Peter Philips' bug fix
test_that("Compare3ProperRocFits", {
  tmp <- tempfile()

  #The first run always succeeds


  # Subsequent runs will suceed only if the file is unchanged
  # reAnalyze = TRUE to recompute the values

})


# test_that("known hash - THIS WORKS", {
# 
#   expect_known_hash(Compare3ProperRocFits(1,1,reAnalyze = FALSE), hash = '6a90170dda')
# 
# })

