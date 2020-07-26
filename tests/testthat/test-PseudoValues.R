contextStr <- "UtilPseudoValues FROC small datasets"
context(contextStr)
test_that(contextStr, {
  
  # FACTRL
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)

  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "wAFROC")
  expect_equal(x1, x2)

  # SPLIT-PLOT-A
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)

  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpA", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "wAFROC")
  expect_equal(x1, x2)

  # examine the jkFomValues
  if (all(is.na(x1$jkFomValues[1,1:2,1:5]))) stop("failed this test: NAs present")
  if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")

  # SPLIT-PLOT-C
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)

  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "wAFROC")
  expect_equal(x1, x2)
  
  t <- temp$descriptions$truthTableStr
  f <- x1$jkFomValues
  
  # examine the jkFomValues
  # if (any(is.na(x1$jkFomValues[,1,1:3]))) stop("failed this test: NAs present")
  # if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  # if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  # if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")
  
})
