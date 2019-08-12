context("Data file saving and reading routines")

test_that("Df2RJafrocDataset", {

  # generate the ratings
  # a small ROC dataset
  set.seed(1)
  z1 <- rnorm(5)
  z2 <- rnorm(7)*1.5 + 2

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/Df2RJafrocDataset-1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- Df2RJafrocDataset(z1, z2)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(Df2RJafrocDataset(z1, z2), ds)
  # end of test

  # generate the ratings
  # a larger MRMC ROC dataset
  set.seed(1)
  I <- 2;J <- 3
  K1 <- 25;K2 <- 35
  z1 <- array(dim = c(I, J, K1))
  z2 <- array(dim = c(I, J, K2))
  mu <- 2;sigma <- 1.5
  for (i in 1:I) {
    for (j in 1:J) {
      z1[i,j,1:K1] <- rnorm(K1)
      z2[i,j,] <- rnorm(K2) * sigma + mu
    }
  }

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/Df2RJafrocDataset-2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- Df2RJafrocDataset(z1, z2)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(Df2RJafrocDataset(z1, z2), ds)
  # end of test

  # generate the ratings
  # a FROC dataset
  set.seed(1)
  I <- 2;J <- 3
  K1 <- 25;K2 <- 35
  mu <- 1;nuP <- 0.8;lambdaP <- 1;zeta1 <- 0
  lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
  nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
  Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
  z1 <- array(-Inf,dim = c(I,J,K1+K2,40))
  z2 <- array(-Inf,dim = c(I,J,K2,40))
  dimNL <- array(dim=c(I,J,2))
  dimLL <- array(dim=c(I,J,2))
  for (i in 1:I) {
    for (j in 1:J) {
      frocDataRaw <- SimulateFrocDataset(
        mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, lesionVector = Lk2)
      dimNL[i,j,] <- dim(drop(frocDataRaw$NL))
      dimLL[i,j,] <- dim(drop(frocDataRaw$LL))
      z1[i,j,,1:dimNL[i,j,2]] <- drop(frocDataRaw$NL) # drop the excess location indices
      z2[i,j,,1:dimLL[i,j,2]] <- drop(frocDataRaw$LL)
    }
  }
  z1 <- z1[,,,1:max(dimNL[,,2])]
  z2 <- z2[,,,1:max(dimLL[,,2])]

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/Df2RJafrocDataset-3", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- Df2RJafrocDataset(z1, z2, lesionVector = Lk2) # an FROC dataset
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(Df2RJafrocDataset(z1, z2, lesionVector = Lk2), ds) # an FROC dataset
  # end of test

})


test_that("DfBinDataset (ROC&AFROC)", {

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfBinDatasetROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfBinDataset(dataset05, opChType = "ROC") # JT FROC
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfBinDataset(dataset05, opChType = "ROC"), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfBinDatasetAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfBinDataset(dataset05, opChType = "AFROC")
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfBinDataset(dataset05, opChType = "AFROC"), ds)
  # end of test

})


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


test_that("DfFroc2Afroc & DfFroc2Roc & DfReadLrocDataFile & DfLroc2Roc & DfReadCrossedModalities", {

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfFroc2Roc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfFroc2Roc(dataset05)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfFroc2Roc(dataset05), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadLrocDataFile", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadLrocDataFile()
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadLrocDataFile(), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfLroc2Roc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfLroc2Roc(DfReadLrocDataFile())
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfLroc2Roc(DfReadLrocDataFile()), ds)
  # end of test

  crossedFileName <- system.file("extdata",
                                 "includedCrossedModalitiesData.xlsx",
                                 package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadCrossedModalities", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadCrossedModalities(crossedFileName)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadCrossedModalities(crossedFileName), ds)
  # end of test

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
    "extdata", "includedRocData.csv", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile_csv", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(temp, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), ds)
  # end of test

  fileName <- system.file(
    "extdata", "includedRocData.lrc", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile_lrc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), ds)
  # end of test

  fileName <- system.file(
    "extdata", "includedRocData.imrmc", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-imrmc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "iMRMC")
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "iMRMC"), ds)
  # end of test

  fileName <- system.file("extdata", "includedRocData.txt", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile_txt", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, format = "MRMC")
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), ds)
  # end of test

})


test_that("DfReadDataFile, JAFROC: it does ALL paradigms", {

  fileName <- system.file(
    "extdata", "includedRocData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-roc-jafroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName), ds)
  # end of test

  fileName <- system.file(
    "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-froc-jafroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName), ds)
  # end of test

  # check renumber option
  fileName <- system.file(
    "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-froc-jafroc-renum", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName, renumber = TRUE)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, renumber = TRUE), ds)
  # end of test

  fileName <- system.file(
    "extdata", "includedRoiData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfReadDataFile-jafroc-roi", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadDataFile(fileName)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName), ds)
  # end of test

  ds <- DfReadDataFile(fileName)
  expect_equal(ds$dataType, "ROI")
  # end of test

})


test_that("DfFroc2Afroc, DfExtractDataset", {

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfFroc2Afroc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfFroc2Afroc(dataset05)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfFroc2Afroc(dataset05), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues361/Df2RJafrocDataset/DfExtractDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfExtractDataset(dataset05, rdrs = c(1, 3))
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfExtractDataset(dataset05, rdrs = c(1, 3)), ds)
  # end of test

})


test_that("DfsaveDataFile", {

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
  # end of test


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
  # end of test

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
  # end of test


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
  # end test

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
  # end test
  
})

