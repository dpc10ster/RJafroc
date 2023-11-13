contextStr <- "CORCBM: DfCreate/ExtractCorCbmDataset"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/CORCBM/DfCreateCorCbmDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfCreateCorCbmDataset()
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  ds1 <- DfCreateCorCbmDataset()
  expect_equal(ds1$ratings, ds$ratings)
  expect_equal(ds1$lesions, ds$lesions)
  expect_equal(ds1, ds)

  fn <- paste0(test_path(), '/goodValues361/CORCBM/DfExtractCorCbmDataset', ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  ds1 <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
  expect_equal(ds1, ds)

  
})



