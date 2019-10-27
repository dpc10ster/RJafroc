# TODO::DPC add sample size tests for an FROC dataset

context("ROC sample size routines")

test_that("SsPowerGivenJK DBMH", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsPowerGivenJK(dataset02, FOM = "Wilcoxon", 6, 251, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsPowerGivenJK(dataset02, FOM = "Wilcoxon", 6, 251, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
  
})

test_that("SsPowerGivenJK ORH", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsPowerGivenJK(dataset02, FOM = "Wilcoxon", 6, 251, method = "ORH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsPowerGivenJK(dataset02, FOM = "Wilcoxon", 6, 251, method = "ORH"), 
    tmp, print = TRUE, update = TRUE)
  
})

test_that("SsSampleSizeKGivenJ", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 6, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 6, method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
  
})

test_that("SsPowerTable", {
  skip_on_cran()
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SsPowerTable(dataset02, FOM = "Wilcoxon", method = "DBMH"), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SsPowerTable(dataset02, FOM = "Wilcoxon", method = "DBMH"), 
    tmp, print = TRUE, update = TRUE)
  
})


