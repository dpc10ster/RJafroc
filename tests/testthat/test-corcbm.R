test_that("DfCreateCorCbmDataset", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    DfCreateCorCbmDataset(), 
    tmp, print = TRUE), "Creating reference output")
  
  expect_known_output(
    DfCreateCorCbmDataset(), 
    tmp, print = TRUE)
})

test_that("DfExtractCorCbmDataset", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), 
    tmp, print = TRUE), "Creating reference output")
  
  expect_known_output(
    DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), 
    tmp, print = TRUE)
})

