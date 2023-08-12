library(tools)

contextStr <- "DfReadDataFile jafroc-csv Format"
context(contextStr)
test_that(contextStr, {
  
  #  data format:
  #  A string specifying the format of the data in the file.
  #  It can be "JAFROC" (the default), "MRMC" or "iMRMC".
  #  For "MRMC" the format is determined by the data file extension
  #  as specified in \url{https://perception.lab.uiowa.edu/},
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



contextStr <- "StSignificanceTestingCadVsRad: Issue T1-RRRC for ROC data #73"
context(contextStr)
test_that(contextStr, {
  # See issue 73 on GitHub website
  s1 <- dataset09
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/","issue73.imrmc")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(s1, fn, format="iMRMC")
  }
  s2 <- DfReadDataFile(fn, format="iMRMC")
  expect_equal(s1$ratings, s2$ratings) 
  expect_equal(s1$lesions, s2$lesions)
  expect_equal(s1$descriptions$type, s2$descriptions$type)
  expect_equal(s1$descriptions$truthTableStr, s2$descriptions$truthTableStr)
  expect_equal(unname(s1$descriptions$readerID), unname(s2$descriptions$readerID))
  expect_equal(unname(s1$descriptions$modalityID), unname(s2$descriptions$modalityID))
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



contextStr <- "DfReadDataFile: toy ROC factorial"
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
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  expect_equal(temp, ds)
  
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
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  expect_equal(temp, ds)
  
})

contextStr <- "DfReadDataFile: toy FROC factorial"
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
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  expect_equal(temp, ds)
  
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



contextStr <- "DfReadDataFile LROC dataset, 1 forced mark per case"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/LROC/lroc.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/lroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = TRUE)
  expect_equal(temp, ds)
  
  # must set lrocForcedMark flag to logical T/F with this LROC dataset  
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
  
})


contextStr <- "DfReadDataFile LROC dataset, 1 mark per case NOT enforced, missing a lesion containing case that was not marked"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/LROC/lroc1.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/lroc1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = FALSE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = FALSE)
  expect_equal(temp, ds)
  
  # cannot set lrocForcedMark flag with this dataset  
  expect_error(expect_warning(DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = TRUE)))
  # cannot set lrocForcedMark flag old format dataset  
  expect_error(expect_warning(DfReadDataFile(fileName, lrocForcedMark = TRUE)))
  
})


contextStr <- "DfReadDataFile LROC dataset, 1 mark per case NOT enforced, missing a lesion containing case and a normal case that were not marked"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/LROC/lroc2.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/lroc2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = FALSE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = FALSE)
  expect_equal(temp, ds)
  
  # cannot set lrocForcedMark flag with this dataset  
  expect_warning(expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = TRUE)))
  
  # cannot set LROC flag with FROC or ROC data  
  {
    fileName <- system.file(
      "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
    
    expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE, lrocForcedMark = TRUE))
  }
})
