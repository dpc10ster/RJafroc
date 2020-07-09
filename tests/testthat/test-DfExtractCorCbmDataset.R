contextStr <- "DfExtractCorCbmDataset"
context(contextStr)
test_that(contextStr, {
  fn <- paste0(test_path(), "/goodValues361/DfExtractCorCbmDataset/dataset05_1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), ds)
  # end of test
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractCorCbmDataset/dataset05_2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3)), ds)
  # end of test
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractCorCbmDataset/dataset05_3", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2), ds)
  # end of test
  
})



