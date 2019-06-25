test_that("Df2RJafrocDataset", {
  
  set.seed(1)
  z1 <- rnorm(5)
  z2 <- rnorm(7)*1.5 + 2
  # uncomment next to create dataset and then save it
  ## setwd(paste0(getwd(), "/tests/testthat"))
  # temp <- Df2RJafrocDataset(z1, z2)
  # save(temp, file = "goodValues/Df2RJafrocDataset.ds01")
  
  load("goodValues/Df2RJafrocDataset.ds01")
  expect_equal(Df2RJafrocDataset(z1, z2), temp) # an ROC dataset
  
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
  # uncomment next to create dataset and then save it
  # temp <- Df2RJafrocDataset(z1, z2)
  # save(temp, file = "goodValues/Df2RJafrocDataset.ds02")
  
  load("goodValues/Df2RJafrocDataset.ds02")
  expect_equal(Df2RJafrocDataset(z1, z2), temp)  # an ROC dataset
  
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
        mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, lesionNum = Lk2)
      dimNL[i,j,] <- dim(drop(frocDataRaw$NL))
      dimLL[i,j,] <- dim(drop(frocDataRaw$LL))
      z1[i,j,,1:dimNL[i,j,2]] <- drop(frocDataRaw$NL) # drop the excess location indices
      z2[i,j,,1:dimLL[i,j,2]] <- drop(frocDataRaw$LL)
    }
  }
  z1 <- z1[,,,1:max(dimNL[,,2])]
  z2 <- z2[,,,1:max(dimLL[,,2])]
  # uncomment next to create dataset and then save it
  # temp <- Df2RJafrocDataset(z1, z2, lesionNum = Lk2)
  # save(temp, file = "goodValues/Df2RJafrocDataset.ds03")
  
  load("goodValues/Df2RJafrocDataset.ds03")
  expect_equal(Df2RJafrocDataset(z1, z2, lesionNum = Lk2), temp) # an FROC dataset
  
})


test_that("DfBinDatasetROC", {
  
  dataset <- dataset05
  # uncomment next to create dataset and then save it
  # temp <- DfBinDataset(dataset, opChType = "ROC")
  # save(temp, file = "goodValues/DfBinDatasetROC.ds04")
  
  load("goodValues/DfBinDatasetROC.ds04")
  expect_equal(DfBinDataset(dataset, opChType = "ROC"), temp)
  
})


test_that("DfBinDatasetAFROC", {
  
  dataset <- dataset05 # JT FROC
  # uncomment next to create dataset and then save it
  # temp <- DfBinDataset(dataset, opChType = "AFROC")
  # save(temp, file = "goodValues/DfBinDatasetAFROC.ds05")
  
  load("goodValues/DfBinDatasetAFROC.ds05")
  expect_equal(DfBinDataset(dataset, opChType = "AFROC"), temp)
  
})


test_that("DfCreateCorCbmDataset", {
  
  # uncomment next to create dataset and then save it
  # temp <- DfCreateCorCbmDataset()
  # save(temp, file = "goodValues/DfCreateCorCbmDataset.ds06")
  
  load("goodValues/DfCreateCorCbmDataset.ds06")
  expect_equal(DfCreateCorCbmDataset(), temp)
  
})


test_that("DfExtractCorCbmDataset", {
  
  # uncomment next to create dataset and then save it
  # temp <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
  # save(temp, file = "goodValues/DfExtractCorCbmDataset.ds07")
  
  load("goodValues/DfExtractCorCbmDataset.ds07")
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), temp)
  
  # uncomment next to create dataset and then save it
  # temp <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
  # save(temp, file = "goodValues/DfExtractCorCbmDataset.ds08")
  
  load("goodValues/DfExtractCorCbmDataset.ds08")
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), temp)
  
  # uncomment next to create dataset and then save it
  # temp <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3))
  # save(temp, file = "goodValues/DfExtractCorCbmDataset.ds09")
  
  load("goodValues/DfExtractCorCbmDataset.ds09")
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3)), temp)
  
  # uncomment next to create dataset and then save it
  # temp <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2)
  # save(temp,file = "goodValues/DfExtractCorCbmDatasett2r1")
  
  load("goodValues/DfExtractCorCbmDatasett2r1")
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2), temp)
  
})


test_that("DfFroc2Afroc & DfFroc2Roc & DfReadLrocDataFile & DfLroc2Roc", {
  
  # uncomment next to create dataset and then save it
  # temp <- DfFroc2Afroc(dataset05)
  # save(temp, file = "goodValues/DfFroc2Afroc.ds10")
  
  load("goodValues/DfFroc2Afroc.ds10")
  expect_equal(DfFroc2Afroc(dataset05), temp)
  
  # uncomment next to create dataset and then save it
  # temp <- DfFroc2Roc(dataset05)
  # save(temp, file = "goodValues/DfFroc2Roc.ds11")
  
  load("goodValues/DfFroc2Roc.ds11")
  expect_equal(DfFroc2Roc(dataset05), temp)
  
  # uncomment next to create dataset and then save it
  # ds12 <- DfReadLrocDataFile()
  # save(ds12, file = "goodValues/DfReadLrocDataFile.ds12")
  
  load("goodValues/DfReadLrocDataFile.ds12")
  expect_equal(DfReadLrocDataFile(), ds12)

  # uncomment next to create dataset and then save it
  # temp <- DfLroc2Roc(ds12) # sic, temp1 is LROC dataset
  # save(temp, file = "goodValues/DfLroc2Roc.ds13")

  load("goodValues/DfLroc2Roc.ds13")
  expect_equal(DfLroc2Roc(ds12), temp)
  
})


test_that("DfReadCrossedModalities", {
  
  crossedFileName <- system.file("extdata",
                                 "includedCrossedModalitiesData.xlsx",
                                 package = "RJafroc", mustWork = TRUE)
  
  # uncomment next to create dataset and then save it
  # temp <- DfReadCrossedModalities(crossedFileName)
  # save(temp, file = "goodValues/DfReadCrossedModalities.ds14")
  
  load("goodValues/DfReadCrossedModalities.ds14")
  expect_equal(DfReadCrossedModalities(crossedFileName), temp)
  
})


test_that("DfReadDataFileMRMC", {
  
  fileName <- system.file("extdata", "includedRocData.csv", package = "RJafroc", mustWork = TRUE)
  
  # uncomment next to create dataset and then save it
  # temp <- DfReadDataFile(fileName, format = "MRMC")
  # save(temp, file = "goodValues/DfReadDataFileMRMC.ds15")
  
  load("goodValues/DfReadDataFileMRMC.ds15")
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), temp)
  
  fileName <- system.file(
    "extdata", "includedRocData.lrc", package = "RJafroc", mustWork = TRUE)
  
  # uncomment next to create dataset and then save it
  # temp <- DfReadDataFile(fileName, format = "MRMC")
  # save(temp, file = "goodValues/DfReadDataFileLRC.ds16")
  
  load("goodValues/DfReadDataFileLRC.ds16")
  expect_equal(DfReadDataFile(fileName, format = "MRMC"), temp)
  
  fileName <- system.file(
    "extdata", "includedRocData.imrmc", package = "RJafroc", mustWork = TRUE)
  
  # uncomment next to create dataset and then save it
  # temp <- DfReadDataFile(fileName, format = "iMRMC")
  # save(temp, file = "goodValues/DfReadDataFileiMRMC.ds17")
  
  load("goodValues/DfReadDataFileiMRMC.ds17")
  expect_equal(DfReadDataFile(fileName, format = "iMRMC"), temp)
  
})


test_that("DfReadDataFilesExcel", {
  
  fileName <- system.file(
    "extdata", "includedRocData.xlsx", package = "RJafroc", mustWork = TRUE)
  
  # uncomment next to create dataset and then save it
  temp <- DfReadDataFile(fileName)
  save(temp, file = "goodValues/DfReadDataFileXlsx.ds18")
  
  load("goodValues/DfReadDataFileXlsx.ds18")
  expect_equal(DfReadDataFile(fileName), temp)
  
  fileName <- system.file(
    "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)
  
  # uncomment next to create dataset and then save it
  # temp <- DfReadDataFile(fileName, renumber = TRUE)
  # save(temp, file = "goodValues/DfReadDataFileFrocXlsx.ds19")
  
  load("goodValues/DfReadDataFileFrocXlsx.ds19")
  expect_equal(DfReadDataFile(fileName, renumber = TRUE), temp)
  
})


test_that("DfReadDataFileROI", {
  
  fileName <- system.file(
    "extdata", "includedRoiData.xlsx", package = "RJafroc", mustWork = TRUE)
  
  # uncomment next to create dataset and then save it
  # temp <- DfReadDataFile(fileName)
  # save(temp, file = "goodValues/includedRoiData.ds20")
  
  load("goodValues/includedRoiData.ds20")
  expect_equal(DfReadDataFile(fileName), temp)
  
  ds <- DfReadDataFile(fileName)
  expect_equal(ds$dataType, "ROI")
  
})


test_that("DfExtractDataset", {
  
  dataset <- dataset05
  
  # uncomment next to create dataset and then save it
  # temp <- DfExtractDataset(dataset, rdrs = c(1, 3))
  # save(temp, file = "goodValues/DfExtractDataset.ds21")
  
  load("goodValues/DfExtractDataset.ds21")
  expect_equal(DfExtractDataset(dataset, rdrs = c(1, 3)), temp)
  
})


test_that("DfSaveDataFile", {
  
  dataset <- dataset05
  
  # special case
  # uncomment next to create dataset and then save it
  # DfSaveDataFile(dataset, fileName = "goodValues/dataset05.xlsx", format = "JAFROC")
  
  DfSaveDataFile(dataset, fileName = "tempValues/dataset05.xlsx", format = "JAFROC")
  for (i in 1:3) { # there are 3 worksheets in Excel file
    dfGood <- readWorkbook("goodValues/dataset05.xlsx", i) # check each sheet individually
    dfCurrent <- readWorkbook("tempValues/dataset05.xlsx", i)    # do:
    expect_equivalent(dfGood, dfCurrent)# works!
  }
  
  dataset <- datasetROI
  
  # special case
  # uncomment next to create dataset and then save it
  # DfSaveDataFile(dataset, fileName = "goodValues/datasetROI.xlsx", format = "JAFROC")
  
  DfSaveDataFile(dataset, fileName = "tempValues/datasetROI.xlsx", format = "JAFROC")
  for (i in 1:3) { # there are 3 worksheets in Excel file
    dfGood <- readWorkbook("goodValues/datasetROI.xlsx", i) # check each sheet individually
    dfCurrent <- readWorkbook("tempValues/datasetROI.xlsx", i)    # do:
    expect_equivalent(dfGood, dfCurrent)# works!
  }
  
  dataset <- dataset02 # ROC
  
  # uncomment lines to save dataset as Excel file in folder goodValues
  # DfSaveDataFile(dataset, fileName = "goodValues/dataset02.imrmc", format = "iMRMC")
  
  DfSaveDataFile(dataset, fileName = "tempValues//dataset02.imrmc", format = "iMRMC")
  xx <- file("goodValues/dataset02.imrmc", open = "rt")
  xx1 <- readLines(xx)
  yy <- file("tempValues//dataset02.imrmc", open = "rt")
  yy1 <- readLines(yy)
  close(xx)
  close(yy)
  expect_equivalent(xx1, yy1)# works!

  # uncomment lines to save dataset as Excel file in folder goodValues
  # DfSaveDataFile(dataset, fileName = "goodValues/dataset02.csv", format = "MRMC")
  
  DfSaveDataFile(dataset, fileName = "tempValues//dataset02.csv", format = "MRMC")
  xx <- file("goodValues/dataset02.csv", open = "rt")
  xx1 <- readLines(xx)
  yy <- file("tempValues//dataset02.csv", open = "rt")
  yy1 <- readLines(yy)
  close(xx)
  close(yy)
  expect_equivalent(xx1, yy1)# works!
  
  # uncomment lines to save dataset as Excel file in folder goodValues
  # DfSaveDataFile(dataset, fileName = "goodValues/dataset02.lrc", format = "MRMC")
  
  DfSaveDataFile(dataset, fileName = "tempValues//dataset02.lrc", format = "MRMC")
  xx <- file("goodValues/dataset02.lrc", open = "rt")
  xx1 <- readLines(xx)
  yy <- file("tempValues//dataset02.lrc", open = "rt")
  yy1 <- readLines(yy)
  close(xx)
  close(yy)
  expect_equivalent(xx1, yy1)# works!
  
})

