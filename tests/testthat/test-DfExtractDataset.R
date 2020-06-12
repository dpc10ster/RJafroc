test_that("DfExtractDataset", {
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/datset02Roc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractDataset(dataset02, rdrs = c(1, 2, 3))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractDataset(dataset02, rdrs = c(1, 2, 3)), ds)
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/datset05JTFroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractDataset(dataset05, rdrs = c(1, 2, 3))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractDataset(dataset05, rdrs = c(1, 2, 3)), ds)
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/datasetCrossedModality", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractDataset(datasetCrossedModality, rdrs = c(1, 2, 3, 4, 5))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractDataset(datasetCrossedModality, rdrs = c(1, 2, 3, 4, 5)), ds)
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/datsetCadLroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractDataset(datasetCadLroc, rdrs = c(1, 2, 3, 4, 5))
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfExtractDataset(datasetCadLroc, rdrs = c(1, 2, 3, 4, 5)), ds)
  
})


