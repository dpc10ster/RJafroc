context("Binning routines")

test_that("DfBinDataset1", {
  tmp <- tempfile()
  expect_known_output(
    DfBinDataset(dataset05, opChType = "ROC"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfBinDataset2", {
  tmp <- tempfile()
  expect_known_output(
    DfBinDataset(dataset05, opChType = "AFROC"), 
    tmp, print = TRUE, update = TRUE)
})
