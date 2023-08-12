contextStr <- "UtilPseudoValues: FROC FCTRL wAFROC"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)

  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", "wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(ds, FOM = "wAFROC", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(ds, FOM = "wAFROC", FPFValue = 0.2)
  expect_equal(x1, x2)

})


contextStr <- "UtilPseudoValues: FROC FCTRL wAFROC1"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", "wAFROC1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(ds, FOM = "wAFROC1", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(ds, FOM = "wAFROC1", FPFValue = 0.2)
  expect_equal(x1, x2)
  
})






contextStr <- "UtilPseudoValues: FROC FCTRL MaxLLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", "MaxLLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(ds, FOM = "MaxLLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(ds, FOM = "MaxLLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
})









contextStr <- "UtilPseudoValues: FROC FCTRL MaxNLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", "MaxNLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(ds, FOM = "MaxNLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(ds, FOM = "MaxNLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
})





