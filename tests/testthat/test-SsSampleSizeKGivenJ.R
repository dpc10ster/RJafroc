context("SsSampleSizeKGivenJ tests")
library(RJafroc)


test_that("SsSampleSizeKGivenJ should error on unknown method", {
  expect_error(SsSampleSizeKGivenJ(dataset02, J = 6, method = "UNKNOWN"),"Incorrect method." )
})

test_that("SsSampleSizeKGivenJ should be silent using dataset02 and DBMH", {
  expect_silent(SsSampleSizeKGivenJ(dataset02, J = 6, method = "DBMH"))
})

test_that("SsSampleSizeKGivenJ should be silent using dataset02 and ORH", {
  expect_silent(SsSampleSizeKGivenJ(dataset02, J = 6, method = "ORH"))
})