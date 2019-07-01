# TODO::DPC add sample size tests for an FROC dataset

context("ROC sample size routines")

test_that("SsPowerGivenJKDBMH", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsPowerGivenJK(dataset02, 6, 251, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsPowerGivenJK(dataset02, 6, 251, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
  
})

test_that("SsPowerGivenJKORH", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsPowerGivenJK(dataset02, 6, 251, method = "ORH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsPowerGivenJK(dataset02, 6, 251, method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
  
})

test_that("SsSampleSizeKGivenJ", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsSampleSizeKGivenJ(dataset02, J = 6, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsSampleSizeKGivenJ(dataset02, J = 6, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
  
})

test_that("SsPowerTable", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsPowerTable(dataset02, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsPowerTable(dataset02, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
  
})


