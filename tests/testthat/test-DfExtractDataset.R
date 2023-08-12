contextStr <- "DfExtractDataset: toy factorial dataset"
context(contextStr)
test_that(contextStr, {
  
  # FACTRL
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/frocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- DfExtractDataset(ds, rdrs = c(1, 3))
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- DfExtractDataset(ds, rdrs = c(1, 3))
  expect_equal(x1, x2)

})

