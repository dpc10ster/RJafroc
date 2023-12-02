contextStr <- "DfCreateCorCbmDataset"
context(contextStr)
test_that(contextStr, {
  fn <- paste0(test_path(), "/goodValues361/DfCreateCorCbmDataset/example-default", ".rds")
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

  
})




