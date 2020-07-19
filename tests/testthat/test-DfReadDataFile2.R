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
  
  # this checks a data file that uses strings for both readers and treatments, i.e., rocCrStrRdrsTrts.xlsx
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

contextStr <- "DfReadDataFile toy FROC split-plot A, C and crossed datasets"
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
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)

  # this checks a data file that uses strings for both readers and treatments, i.e., rocCrStrRdrsTrts.xlsx
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
    "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/frocSpC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  # fileName <- system.file(
  #   "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  # 
  # fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/frocSpA", ".rds")
  # if (!file.exists(fn)) {
  #   warning(paste0("File not found - generating new ",fn))
  #   temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  #   saveRDS(temp, file = fn)
  # }
  # 
  # ds <- readRDS(fn)
  # expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/bad/incorrectCaseIDsInTP.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
  
  y <- system.file("extdata", "toyFiles/FROC/bad/frocCrNonCharInReaderID.xlsx", 
                   package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(y, newExcelFileFormat = TRUE))
  
  y <- system.file("extdata", "toyFiles/FROC/bad/frocCrNonCharInModalityID.xlsx", 
                   package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(y, newExcelFileFormat = TRUE))
  
  
})
