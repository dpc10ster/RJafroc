contextStr <- "DfReadDataFile real FROC split-plot A dataset"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/1T3Rvs4R.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/1T3Rvs4R", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  expect_equal(temp, ds)
  
  # calculate pseudovalues for split plot A
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/1T3Rvs4R", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "wAFROC")
    saveRDS(temp, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "wAFROC")
  expect_equal(x1, x2)
  
  
})
