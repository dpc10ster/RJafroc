contextStr <- "UtilPseudoValues: FROC FCTRL wAFROC"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)

  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", "wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "wAFROC", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "wAFROC", FPFValue = 0.2)
  expect_equal(x1, x2)

})



contextStr <- "UtilPseudoValues: FROC SpC wAFROC"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpC", "wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "wAFROC", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "wAFROC", FPFValue = 0.2)
  expect_equal(x1, x2)
  
  t <- temp$descriptions$truthTableStr
  f <- x1$jkFomValues
  
  # examine the jkFomValues
  # if (any(is.na(x1$jkFomValues[,1,1:3]))) stop("failed this test: NAs present")
  # if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  # if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  # if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")
  
})



contextStr <- "UtilPseudoValues: FROC SpA wAFROC"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpA", "wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "wAFROC", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "wAFROC", FPFValue = 0.2)
  expect_equal(x1, x2)
  
  # examine the jkFomValues
  if (all(is.na(x1$jkFomValues[1,1:2,1:5]))) stop("failed this test: NAs present")
  if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")
  
  
})



contextStr <- "UtilPseudoValues: FROC FCTRL MaxLLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", "MaxLLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "MaxLLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "MaxLLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
})



contextStr <- "UtilPseudoValues: FROC SpC MaxLLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpC", "MaxLLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "MaxLLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "MaxLLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
  t <- temp$descriptions$truthTableStr
  f <- x1$jkFomValues
  
  # examine the jkFomValues
  # if (any(is.na(x1$jkFomValues[,1,1:3]))) stop("failed this test: NAs present")
  # if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  # if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  # if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")
  
})



contextStr <- "UtilPseudoValues: FROC SpA MaxLLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpA", "MaxLLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "MaxLLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "MaxLLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
  # examine the jkFomValues
  if (all(is.na(x1$jkFomValues[1,1:2,1:5]))) stop("failed this test: NAs present")
  if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")
  
  
})




contextStr <- "UtilPseudoValues: FROC FCTRL MaxNLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocCr", "MaxNLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "MaxNLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "MaxNLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
})



contextStr <- "UtilPseudoValues: FROC SpC MaxNLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpC", "MaxNLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "MaxNLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "MaxNLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
  t <- temp$descriptions$truthTableStr
  f <- x1$jkFomValues
  
  # examine the jkFomValues
  # if (any(is.na(x1$jkFomValues[,1,1:3]))) stop("failed this test: NAs present")
  # if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  # if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  # if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")
  
})



contextStr <- "UtilPseudoValues: FROC SpA MaxNLF"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilPseudoValues/frocSpA", "MaxNLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilPseudoValues(temp, FOM = "MaxNLF", FPFValue = 0.2)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilPseudoValues(temp, FOM = "MaxNLF", FPFValue = 0.2)
  expect_equal(x1, x2)
  
  # examine the jkFomValues
  if (all(is.na(x1$jkFomValues[1,1:2,1:5]))) stop("failed this test: NAs present")
  if (!all(is.na(x1$jkFomValues[2,1:2,1:5]))) stop("failed this test: not all NAs")
  if (!all(is.na(x1$jkFomValues[1,3:5,1:5]))) stop("failed this test: not all NAs present")
  if (all(is.na(x1$jkFomValues[2,3:5,1:5]))) stop("failed this test: NAs present")
  
  
})
