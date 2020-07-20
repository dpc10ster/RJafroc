library(tools)

contextStr <- "DfReadDataFile jafroc-csv Format"
context(contextStr)
test_that(contextStr, {
  
  #  data format:
  #  A string specifying the format of the data in the file.
  #  It can be "JAFROC" (the default), "MRMC" or "iMRMC".
  #  For "MRMC" the format is determined by the data file extension
  #  as specified in http://perception.radiology.uiowa.edu/,
  #  i.e., .csv or .txt or .lrc.
  #  For file extension .imrmc the format is described in https://code.google.com/p/imrmc/.
  #
  # dpc 6/29/19 comment
  # ROC is the simplest paradigm and data structure; yet 4 different formats; go
  # figure!
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
  temp <- DfReadDataFile(fileName, format = "MRMC")
  expect_equal(temp, ds)
  
})


contextStr <- "DfReadDataFile jafroc-lrc Format"
context(contextStr)
test_that(contextStr, {
  fileName <- system.file(
    "extdata", "RocData.lrc", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/lrc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, format = "MRMC")
  expect_equal(temp, ds)
  
})



contextStr <- "DfReadDataFile jafroc-iMRMC Format"
context(contextStr)
test_that(contextStr, {
  fileName <- system.file(
    "extdata", "RocData.imrmc", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/iMRMC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "iMRMC")
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, format = "iMRMC")
  expect_equal(temp, ds)

})



contextStr <- "DfReadDataFile jafroc-MRMC Format"
context(contextStr)
test_that(contextStr, {
  fileName <- system.file("extdata", "RocData.txt", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/txt", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, format = "MRMC")
  expect_equal(temp, ds)

})



contextStr <- "DfReadDataFile jafroc-oldExcelFormat"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "Roc.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/jafroc-oldExcelFormat", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  expect_equal(temp, ds)

})



contextStr <- "DfReadDataFile jafroc-newExcelFormat"
context(contextStr)
test_that(contextStr, {
  fileName <- system.file(
    "extdata", "Froc.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/jafroc-newExcelFormat", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  expect_equal(temp, ds)

})



contextStr <- "DfReadDataFile jafroc-renum"
context(contextStr)
test_that(contextStr, {
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


contextStr <- "DfReadDataFile bad toy ROC datasets"
context(contextStr)
test_that(contextStr, {
  
  ## The failed checks apply to the Truth Sheet only
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/bad/missing2CellsRow5and7.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/bad/missingEntireRow5.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/bad/missing1CellRow5.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/bad/non-integerCaseID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/bad/non-integerLesionID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/bad/incorrectWeights.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/bad/non-numeric-weight.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

 # fileName <- system.file(
 #   "extdata", "/toyFiles/ROC/duplicatedRow.xlsx", package = "RJafroc", mustWork = TRUE)
 # expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
 fileName <- system.file(
   "extdata", "/toyFiles/ROC/bad/incorrectSheetName.xlsx", package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
 fileName <- system.file("extdata", "toyFiles/ROC/bad/rocCrStrRdrsNonUnique.xlsx", 
                  package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
})


contextStr <- "DfReadDataFile toy FROC crossed datasets"
context(contextStr)
test_that(contextStr, {
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/frocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }

  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  expect_equal(temp, ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/FrocSpCVaryK1K2.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/FrocSpCVaryK1K2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }

  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  expect_equal(temp, ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCrStrRdrsTrts.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/frocCrStrRdrsTrts", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }

  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  expect_equal(temp, ds)
  
})
