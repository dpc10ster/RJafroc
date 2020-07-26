contextStr <- "UtilVarCompOR: toy crossed and split plot datasets"
context(contextStr)
test_that(contextStr, {
  
  # FACTRL
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/frocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilORVarComponentsFactorial(temp, FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilORVarComponentsFactorial(temp, FOM = "wAFROC")
  expect_equal(x1, x2)
  
  # SPLIT-PLOT-A
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  # 
  # fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/frocSpA", ".rds")
  # if (!file.exists(fn)) {
  #   warning(paste0("File not found - generating new ",fn))
  #   x1 <- UtilORVarComponentsFactorial(temp, FOM = "wAFROC")
  #   saveRDS(x1, file = fn)
  # }
  # 
  # x1 <- readRDS(fn)
  # x2 <- UtilORVarComponentsFactorial(temp, FOM = "wAFROC")
  # expect_equal(x1, x2)
  
  # SPLIT-PLOT-C
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/frocSpC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilORVarComponentsFactorial(temp, FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilORVarComponentsFactorial(temp, FOM = "wAFROC")
  expect_equal(x1, x2)
  
})


