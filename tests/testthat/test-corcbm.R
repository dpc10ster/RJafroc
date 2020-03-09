context("CORCBM routines")

test_that("DfCreate/ExtractCorCbmDataset", {

  fn <- paste0(test_path(), "/goodValues361/CORCBM/DfCreateCorCbmDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfCreateCorCbmDataset()
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfCreateCorCbmDataset(), ds)
  # end of test

  fn <- paste0(test_path(), '/goodValues361/CORCBM/DfExtractCorCbmDataset', ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), ds)
  # end of test
  
})


