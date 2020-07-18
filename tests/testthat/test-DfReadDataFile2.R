contextStr <- "DfReadDataFile toy ROC split-plot A, C and crossed datasets"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/rocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/rocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/rocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/rocSpC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/rocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/rocSpA", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
})
