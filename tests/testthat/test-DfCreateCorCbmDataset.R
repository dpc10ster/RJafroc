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
  expect_equal(DfCreateCorCbmDataset(), ds)
  # end of test
  
  
})



