test_that("DfCreate/ExtractCorCbmDataset", {

  fn <- paste0(test_path(), '/goodValues/DfCreateCorCbmDataset01')
  if (!file.exists(fn)) {
   expect_known_output(
      DfCreateCorCbmDataset(), 
      fn, print = TRUE, update = TRUE)
  }
  
  expect_known_output(
    DfCreateCorCbmDataset(), 
    fn, print = TRUE, update = FALSE)
  
  fn <- paste0(test_path(), '/goodValues/DfCreateCorCbmDataset02')
  if (!file.exists(fn)) {
    expect_known_output(
      DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), 
      fn, print = TRUE, update = TRUE)
  }
  
  expect_known_output(
    DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), 
    fn, print = TRUE, update = FALSE)
  
})


