# following works after `warn` option in `option`` NOT set to 2 in Compare3RocFits.R
# as per Peter Philips' bug fix
test_that("Compare3ProperRocFits", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    # save time by using previously saved values 
    # reAnalyze = FALSE to recompute the values
    # unfortunately this causes Failures on Travis; differences are in the 6th decimal place
    # so reverted to original way
    Compare3ProperRocFits(1,1,reAnalyze = TRUE),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    # this time calculate from scratch 
    # reAnalyze = TRUE to recompute the values
    Compare3ProperRocFits(1,1,reAnalyze = TRUE),
    tmp, print = TRUE, update = TRUE)
})

# # alternate way of testing
# using this causes failure on Travis; hash depends on platform?
# test_that("known hash", {
# 
#   expect_known_hash(Compare3ProperRocFits(1,1,reAnalyze = TRUE), hash = '6a90170dda') # value from my machine
#   expect_known_hash(Compare3ProperRocFits(1,1,reAnalyze = TRUE), hash = 'ee6f623095') # value from Peter
# 
# })

