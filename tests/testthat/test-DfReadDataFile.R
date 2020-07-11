context("DfReadDataFile, non_JAFROC-1")
test_that("DfReadDataFile, non_JAFROC-1", {
  
  #  data format:
  #  A string specifying the format of the data in the file.
  #  It can be "JAFROC" (the default), "MRMC" or "iMRMC".
  #  For "MRMC" the format is determined by the data file extension
  #  as specified in http://perception.radiology.uiowa.edu/,
  #  i.e., .csv or .txt or .lrc.
  #  For file extension .imrmc the format is described in https://code.google.com/p/imrmc/.
  #
  # dpc 6/29/19 comment
  # ROC is the simplest paradigm and data structure; yet 4 different formats
  #
  
  fileName <- system.file(
    "extdata", "RocData.csv", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/csv", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), ds)
  
})


context("DfReadDataFile, non_JAFROC-2")
test_that("DfReadDataFile, non_JAFROC-2", {
  fileName <- system.file(
    "extdata", "RocData.lrc", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/lrc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }
  
  ds1 <- readRDS(fn)
  ds2 <- DfReadDataFile(fileName, format = "MRMC")
  expect_equal(ds2, ds1)
  
})



context("DfReadDataFile, non_JAFROC-3")
test_that("DfReadDataFile, non_JAFROC-3", {
  fileName <- system.file(
    "extdata", "RocData.imrmc", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/iMRMC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "iMRMC")
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "iMRMC"), ds)
  
})



context("DfReadDataFile, non_JAFROC-4")
test_that("DfReadDataFile, non_JAFROC-4", {
  fileName <- system.file("extdata", "RocData.txt", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/txt", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), ds)
  
})



context("DfReadDataFile, JAFROC-1")
test_that("DfReadDataFile, JAFROC", {
  
  fileName <- system.file(
    "extdata", "Roc.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/jafroc-oldExcelFormat", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }
  
  ds1 <- readRDS(fn)
  ds2 <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  
  expect_equal(ds1, ds2)
  
})



context("DfReadDataFile, JAFROC-2")
test_that("DfReadDataFile, JAFROC", {
  fileName <- system.file(
    "extdata", "Froc.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/jafroc-newExcelFormat", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = FALSE), ds)
  
})



context("DfReadDataFile, JAFROC-3")
test_that("DfReadDataFile, JAFROC", {
  # check sequentialNames option
  fileName <- system.file(
    "extdata", "Froc.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/jafroc-renum", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, sequentialNames = TRUE, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }
  
  ds1 <- readRDS(fn)
  ds2 <- DfReadDataFile(fileName, sequentialNames = TRUE, newExcelFileFormat = FALSE)
  expect_equal(ds2, ds1)
  
})

###########################################################################
context("DfReadDataFile toy ROC datasets")
test_that("DfReadDataFile toy ROC datasets", {
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/rocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/jafroc-rocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/rocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/jafroc-rocSpC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/rocCrStrRdrsTrts.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/rocCrStrRdrsTrts", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  ## The failed checks apply to the Truth Sheet only
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missing2CellsRow5and7.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missingEntireRow5.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missing1CellRow5.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-integerCaseID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-integerLesionID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/incorrectWeights.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-numeric-weight.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

 fileName <- system.file(
   "extdata", "/toyFiles/ROC/duplicatedRow.xlsx", package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
 fileName <- system.file(
   "extdata", "/toyFiles/ROC/incorrectSheetName.xlsx", package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
 fileName <- system.file("extdata", "toyFiles/ROC/rocCrStrRdrsNonUnique.xlsx", 
                  package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
})


###########################################################################
context("DfReadDataFile toy FROC datasets")
test_that("DfReadDataFile toy FROC datasets", {
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/frocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = FALSE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/FrocDataSpCVaryK1K2.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/FrocDataSpCVaryK1K2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)

  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCrStrRdrsTrts.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/frocCrStrRdrsTrts", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
    
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/incorrectCaseIDsInTP.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
  
  y <- system.file("extdata", "toyFiles/FROC/frocCrNonCharInReaderID.xlsx", 
                        package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(y, newExcelFileFormat = TRUE))
  
  y <- system.file("extdata", "toyFiles/FROC/frocCrNonCharInModalityID.xlsx", 
                   package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(y, newExcelFileFormat = TRUE))
  
})

