context("test datasets")

test_that("test datasets", {
  tmp <- tempfile()
  expect_known_output(
    dataset01, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset02, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset03, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset04, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset05, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset06, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset07, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset08, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset09, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset10, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset11, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset12, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset13, 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    dataset14, 
    tmp, print = TRUE, update = TRUE)
})

