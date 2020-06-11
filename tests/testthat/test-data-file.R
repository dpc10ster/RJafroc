test_that("DfCreateCorCbmDataset DfExtractCorCbmDataset", {

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfCreateCorCbmDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfCreateCorCbmDataset()
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfCreateCorCbmDataset(), ds)
  # end of test


  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfExtractCorCbmDataset-1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfExtractCorCbmDataset-2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3))
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3)), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfExtractCorCbmDataset-3", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2), ds)
  # end of test

})



test_that("DfFroc2Roc & DfReadLrocDataFile & DfLroc2Roc", {

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfFroc2Roc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfFroc2Roc(dataset05)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfFroc2Roc(dataset05), ds)
  
})
  


test_that("DfFroc2Roc & DfReadLrocDataFile & DfLroc2Roc", {
  
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadLrocDataFile", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadLrocDataFile()
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadLrocDataFile(), ds)
  
})



test_that("DfFroc2Roc & DfReadLrocDataFile & DfLroc2Rocs", {
  
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfLroc2Roc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfLroc2Roc(DfReadLrocDataFile())
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfLroc2Roc(DfReadLrocDataFile()), ds)

})





test_that("DfReadDataFile, non_JAFROC, see below", {

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

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile_csv", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(temp, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), ds)

})



test_that("DfReadDataFile, non_JAFROC, see below", {
  fileName <- system.file(
    "extdata", "RocData.lrc", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile_lrc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }

  ds1 <- readRDS(fn)
  ds2 <- DfReadDataFile(fileName, format = "MRMC")
  expect_equal(ds2, ds1)

})



test_that("DfReadDataFile, non_JAFROC, see below", {
  fileName <- system.file(
    "extdata", "RocData.imrmc", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-imrmc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "iMRMC")
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "iMRMC"), ds)

})



test_that("DfReadDataFile, non_JAFROC, see below", {
  fileName <- system.file("extdata", "RocData.txt", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile_txt", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), ds)

})



test_that("DfReadDataFile, JAFROC: it does ALL paradigms", {

  fileName <- system.file(
    "extdata", "RocData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-roc-jafroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }

  ds1 <- readRDS(fn)
  ds2 <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  
  expect_equal(ds1, ds2)
  
})



test_that("DfReadDataFile, JAFROC: it does ALL paradigms", {
    fileName <- system.file(
    "extdata", "FrocData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-froc-jafroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = FALSE), ds)

})



test_that("DfReadDataFile, JAFROC: it does ALL paradigms", {
  # check sequentialNames option
  fileName <- system.file(
    "extdata", "FrocData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-froc-jafroc-renum", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, sequentialNames = TRUE, newExcelFileFormat = FALSE)
    saveRDS(ds, file = fn)
  }

  ds1 <- readRDS(fn)
  ds2 <- DfReadDataFile(fileName, sequentialNames = TRUE, newExcelFileFormat = FALSE)
  expect_equal(ds2, ds1)

})



test_that("DfExtractDataset", {
  
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfExtractDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractDataset(dataset05, rdrs = c(1, 3))
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfExtractDataset(dataset05, rdrs = c(1, 3)), ds)

})



test_that("DfSaveDataFile", {

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfSaveDataFile.xlsx")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset05, fileName = fn, format = "JAFROC")
  }

  fn1 <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/myTemp.xlsx")
  DfSaveDataFile(dataset05, fileName = fn1, format = "JAFROC")
  for (i in 1:3) { # there are 3 worksheets in Excel file
    dfGood <- readWorkbook(fn, i) # check each sheet individually
    dfCurrent <- readWorkbook(fn1, i)    # do:
    expect_equivalent(dfGood, dfCurrent)# works!
  }
  unlink(fn1)

})



  
test_that("DfsaveDataFile", {
  
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfSaveDataFile-roi.xlsx")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(datasetROI, fileName = fn, format = "JAFROC")
  }

  fn1 <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfSaveDataFile-roi1.xlsx")
  DfSaveDataFile(datasetROI, fileName = fn1, format = "JAFROC")
  for (i in 1:3) { # there are 3 worksheets in Excel file
    dfGood <- readWorkbook(fn, i) # check each sheet individually
    dfCurrent <- readWorkbook(fn1, i)    # do:
    expect_equivalent(dfGood, dfCurrent)# works!
  }
  unlink(fn1)
  
})




test_that("DfsaveDataFile", {
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfSaveDataFile.imrmc") # sic file ext must be imrmc
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset02, fileName = fn, format = "iMRMC")
  }

  fn1 <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/myTemp.imrmc") # sic file ext must be imrmc
  DfSaveDataFile(dataset02, fileName = fn1, format = "iMRMC")
  xx <- file(fn, open = "rt")
  xx1 <- readLines(xx)
  yy <- file(fn1, open = "rt")
  yy1 <- readLines(yy)
  close(xx)
  close(yy)
  expect_equivalent(xx1, yy1)# works!
  unlink(fn1)

})




test_that("DfsaveDataFile", {
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfSaveDataFile.csv") # sic file ext must be csv
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset02, fileName = fn, format = "MRMC")
  }

  fn1 <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/myTemp.csv") # sic file ext must be csv
  DfSaveDataFile(dataset02, fileName = fn1, format = "MRMC")
  xx <- file(fn, open = "rt")
  xx1 <- readLines(xx)
  yy <- file(fn1, open = "rt")
  yy1 <- readLines(yy)
  close(xx)
  close(yy)
  expect_equivalent(xx1, yy1)# works!
  unlink(fn1)

})




test_that("DfsaveDataFile", {
  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfSaveDataFile.lrc") # sic file ext must be lrc
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset02, fileName = fn, format = "MRMC")
  }

  fn1 <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/myTemp.lrc") # sic file ext must be lrc
  DfSaveDataFile(dataset02, fileName = fn1, format = "MRMC")
  xx <- file(fn, open = "rt")
  xx1 <- readLines(xx)
  yy <- file(fn1, open = "rt")
  yy1 <- readLines(yy)
  close(xx)
  close(yy)
  expect_equivalent(xx1, yy1)# works!
  unlink(fn1)

})

