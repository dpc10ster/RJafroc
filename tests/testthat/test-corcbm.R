context("CORCBM routines")

test_that("DfCreate/ExtractCorCbmDataset", {

  fn <- paste0(test_path(), "/goodValues/CORCBM/DfCreateCorCbmDataset")
  if (!file.exists(fn)) {
    ds <- DfCreateCorCbmDataset()
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfCreateCorCbmDataset(), ds)
  # end of test

  fn <- paste0(test_path(), '/goodValues/CORCBM/DfExtractCorCbmDataset')
  if (!file.exists(fn)) {
    ds <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), ds)
  # end of test
  
})


