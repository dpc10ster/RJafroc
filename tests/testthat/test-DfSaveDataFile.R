contextStr <- "DfSaveDataFile1"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/DfSaveDataFile/dataset05.xlsx")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset05, fileName = fn, format = "JAFROC")
  }
  
  fn1 <- paste0(test_path(), "/goodValues361/DfSaveDataFile/myTemp.xlsx")
  DfSaveDataFile(dataset05, fileName = fn1, format = "JAFROC")
  for (i in 1:3) { # there are 3 worksheets in Excel file
    dfGood <- readWorkbook(fn, i) # check each sheet individually
    dfCurrent <- readWorkbook(fn1, i)    # do:
    expect_equivalent(dfGood, dfCurrent)# works!
  }
  unlink(fn1)
  
})




contextStr <- "DfSaveDataFile2"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/DfSaveDataFile/datasetROI.xlsx")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(datasetROI, fileName = fn, format = "JAFROC")
  }
  
  fn1 <- paste0(test_path(), "/goodValues361/DfSaveDataFile/DfSaveDataFile-roi1.xlsx")
  DfSaveDataFile(datasetROI, fileName = fn1, format = "JAFROC")
  for (i in 1:3) { # there are 3 worksheets in Excel file
    dfGood <- readWorkbook(fn, i) # check each sheet individually
    dfCurrent <- readWorkbook(fn1, i)    # do:
    expect_equivalent(dfGood, dfCurrent)# works!
  }
  unlink(fn1)
  
})



contextStr <- "DfSaveDataFile3"
context(contextStr)
test_that(contextStr, {
  fn <- paste0(test_path(), "/goodValues361/DfSaveDataFile/dataset02.imrmc") # sic file ext must be imrmc
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset02, fileName = fn, format = "iMRMC")
  }
  
  fn1 <- paste0(test_path(), "/goodValues361/DfSaveDataFile/myTemp.imrmc") # sic file ext must be imrmc
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




contextStr <- "DfSaveDataFile4"
context(contextStr)
test_that(contextStr, {
  fn <- paste0(test_path(), "/goodValues361/DfSaveDataFile/dataset02.csv") # sic file ext must be csv
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset02, fileName = fn, format = "MRMC")
  }
  
  fn1 <- paste0(test_path(), "/goodValues361/DfSaveDataFile/myTemp.csv") # sic file ext must be csv
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



contextStr <- "DfSaveDataFile5"
context(contextStr)
test_that(contextStr, {
  fn <- paste0(test_path(), "/goodValues361/DfSaveDataFile/dataset02.lrc") # sic file ext must be lrc
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    DfSaveDataFile(dataset02, fileName = fn, format = "MRMC")
  }
  
  fn1 <- paste0(test_path(), "/goodValues361/DfSaveDataFile/myTemp.lrc") # sic file ext must be lrc
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

