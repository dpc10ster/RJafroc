test_that("Df2RJafrocDataset", {
  
  set.seed(1)
  z1 <- rnorm(5)
  z2 <- rnorm(7)*1.5 + 2
  
  fn <- paste0(test_path(), "/goodValues/Df2RJafrocDataset.ds01")
  if (!file.exists(fn)) {
    ds <- Df2RJafrocDataset(z1, z2)
    save(ds, file = fn)
  }
  
  load(fn)
  expect_equal(Df2RJafrocDataset(z1, z2), ds) # a small ROC dataset
  # end of test
  
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
  
  fn <- paste0(test_path(), "/goodValues/Df2RJafrocDataset.ds02")
  if (!file.exists(fn)) {
    ds <- Df2RJafrocDataset(z1, z2)
    save(ds, file = fn)
  }
  
  load(fn)
  expect_equal(Df2RJafrocDataset(z1, z2), ds) # a bigger MRMC ROC dataset
  # end of test  
  
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
  
  fn <- paste0(test_path(), "/goodValues/Df2RJafrocDataset.ds03")
  if (!file.exists(fn)) {
    ds <- Df2RJafrocDataset(z1, z2, lesionNum = Lk2) # an FROC dataset
    save(ds, file = fn)
  }
  
  load(fn)
  expect_equal(Df2RJafrocDataset(z1, z2, lesionNum = Lk2), ds) # an FROC dataset
  # end of test
  
})


test_that("DfBinDataset (ROC&AFROC)", {

  dataset <- dataset05
  fn <- paste0(test_path(), "/goodValues/DfBinDatasetROC.ds01")
  if (!file.exists(fn)) {
    ds <- DfBinDataset(dataset, opChType = "ROC")
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfBinDataset(dataset, opChType = "ROC"), ds)
  # end of test

  dataset <- dataset05 # JT FROC
  fn <- paste0(test_path(), "/goodValues/DfBinDatasetAFROC.ds01")
  if (!file.exists(fn)) {
    ds <- DfBinDataset(dataset, opChType = "AFROC")
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfBinDataset(dataset, opChType = "AFROC"), ds)

})



test_that("DfCreateCorCbmDataset", {

  fn <- paste0(test_path(), "/goodValues/DfCreateCorCbmDataset.ds01")
  if (!file.exists(fn)) {
    ds <- DfCreateCorCbmDataset()
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfCreateCorCbmDataset(), ds)
  # end of test

})


test_that("DfExtractCorCbmDataset", {

  fn <- paste0(test_path(), "/goodValues/DfExtractCorCbmDataset.ds01")
  if (!file.exists(fn)) {
    ds <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues/DfExtractCorCbmDataset.ds02")
  if (!file.exists(fn)) {
    ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3))
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3)), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues/DfExtractCorCbmDataset.ds03")
  if (!file.exists(fn)) {
    ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2)
    save(ds, file = fn)
  }
  
  load(fn)
  expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2), ds)
  # end of test
  
})


test_that("DfFroc2Afroc & DfFroc2Roc & DfReadLrocDataFile & DfLroc2Roc", {

  fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
  if (!file.exists(fn)) {
    ds <- DfFroc2Afroc(dataset05)
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfFroc2Afroc(dataset05), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues/DfFroc2Roc.ds01")
  if (!file.exists(fn)) {
    ds <- DfFroc2Roc(dataset05)
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfFroc2Roc(dataset05), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues/DfReadLrocDataFile.ds01")
  if (!file.exists(fn)) {
    ds <- DfReadLrocDataFile()
    save(ds, file = fn)
  }

  load(fn)
  expect_equal(DfReadLrocDataFile(), ds)
  # end of test

  fn <- paste0(test_path(), "/goodValues/DfReadLrocDataFile.ds02")
  if (!file.exists(fn)) {
    dataset <- DfReadLrocDataFile()
    ds <- DfLroc2Roc(dataset)
    save(ds, file = fn)
  }

  dataset <- DfReadLrocDataFile()
  load(fn)
  expect_equal(DfLroc2Roc(dataset), ds)
  # end of test

})

# 
# test_that("DfReadCrossedModalities", {
#   
#   crossedFileName <- system.file("extdata",
#                                  "includedCrossedModalitiesData.xlsx",
#                                  package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfReadCrossedModalities.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfReadCrossedModalities(crossedFileName)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfReadCrossedModalities(crossedFileName), ds)
#   # end of test
#   
# })
# 
# 
# test_that("DfReadDataFileMRMC", {
#   
#   fileName <- system.file("extdata", "includedRocData.csv", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfReadDataFileMRMC.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   # temp <- DfReadDataFile(fileName, format = "MRMC")
#   # save(temp, file = "goodValues/DfReadDataFileMRMC.ds15")
#   
#   load("goodValues/DfReadDataFileMRMC.ds15")
#   expect_equal(DfReadDataFile(fileName, format = "MRMC"), temp)
#   
#   fileName <- system.file(
#     "extdata", "includedRocData.lrc", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   # temp <- DfReadDataFile(fileName, format = "MRMC")
#   # save(temp, file = "goodValues/DfReadDataFileLRC.ds16")
#   
#   load("goodValues/DfReadDataFileLRC.ds16")
#   expect_equal(DfReadDataFile(fileName, format = "MRMC"), temp)
#   
#   fileName <- system.file(
#     "extdata", "includedRocData.imrmc", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   # temp <- DfReadDataFile(fileName, format = "iMRMC")
#   # save(temp, file = "goodValues/DfReadDataFileiMRMC.ds17")
#   
#   load("goodValues/DfReadDataFileiMRMC.ds17")
#   expect_equal(DfReadDataFile(fileName, format = "iMRMC"), temp)
#   
# })
# 
# 
# test_that("DfReadDataFilesExcel", {
#   
#   fileName <- system.file(
#     "extdata", "includedRocData.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfReadDataFilesExcel.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   temp <- DfReadDataFile(fileName)
#   save(temp, file = "goodValues/DfReadDataFileXlsx.ds18")
#   
#   load("goodValues/DfReadDataFileXlsx.ds18")
#   expect_equal(DfReadDataFile(fileName), temp)
#   
#   fileName <- system.file(
#     "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   # temp <- DfReadDataFile(fileName, renumber = TRUE)
#   # save(temp, file = "goodValues/DfReadDataFileFrocXlsx.ds19")
#   
#   load("goodValues/DfReadDataFileFrocXlsx.ds19")
#   expect_equal(DfReadDataFile(fileName, renumber = TRUE), temp)
#   
# })
# 
# 
# test_that("DfReadDataFileROI", {
#   
#   fileName <- system.file(
#     "extdata", "includedRoiData.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   # temp <- DfReadDataFile(fileName)
#   # save(temp, file = "goodValues/includedRoiData.ds20")
#   
#   load("goodValues/includedRoiData.ds20")
#   expect_equal(DfReadDataFile(fileName), temp)
#   
#   ds <- DfReadDataFile(fileName)
#   expect_equal(ds$dataType, "ROI")
#   
# })
# 
# 
# test_that("DfExtractDataset", {
#   
#   dataset <- dataset05
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   # temp <- DfExtractDataset(dataset, rdrs = c(1, 3))
#   # save(temp, file = "goodValues/DfExtractDataset.ds21")
#   
#   load("goodValues/DfExtractDataset.ds21")
#   expect_equal(DfExtractDataset(dataset, rdrs = c(1, 3)), temp)
#   
# })
# 
# 
# test_that("DfSaveDataFile", {
#   
#   dataset <- dataset05
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # special case
#   # uncomment next to create dataset and then save it
#   # DfSaveDataFile(dataset, fileName = "goodValues/dataset05.xlsx", format = "JAFROC")
#   
#   DfSaveDataFile(dataset, fileName = "tempValues/dataset05.xlsx", format = "JAFROC")
#   for (i in 1:3) { # there are 3 worksheets in Excel file
#     dfGood <- readWorkbook("goodValues/dataset05.xlsx", i) # check each sheet individually
#     dfCurrent <- readWorkbook("tempValues/dataset05.xlsx", i)    # do:
#     expect_equivalent(dfGood, dfCurrent)# works!
#   }
#   
#   dataset <- datasetROI
#   
#   # special case
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then save it
#   # DfSaveDataFile(dataset, fileName = "goodValues/datasetROI.xlsx", format = "JAFROC")
#   
#   DfSaveDataFile(dataset, fileName = "tempValues/datasetROI.xlsx", format = "JAFROC")
#   for (i in 1:3) { # there are 3 worksheets in Excel file
#     dfGood <- readWorkbook("goodValues/datasetROI.xlsx", i) # check each sheet individually
#     dfCurrent <- readWorkbook("tempValues/datasetROI.xlsx", i)    # do:
#     expect_equivalent(dfGood, dfCurrent)# works!
#   }
#   
#   dataset <- dataset02 # ROC
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment lines to save dataset as Excel file in folder goodValues
#   # DfSaveDataFile(dataset, fileName = "goodValues/dataset02.imrmc", format = "iMRMC")
#   
#   DfSaveDataFile(dataset, fileName = "tempValues//dataset02.imrmc", format = "iMRMC")
#   xx <- file("goodValues/dataset02.imrmc", open = "rt")
#   xx1 <- readLines(xx)
#   yy <- file("tempValues//dataset02.imrmc", open = "rt")
#   yy1 <- readLines(yy)
#   close(xx)
#   close(yy)
#   expect_equivalent(xx1, yy1)# works!
# 
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment lines to save dataset as Excel file in folder goodValues
#   # DfSaveDataFile(dataset, fileName = "goodValues/dataset02.csv", format = "MRMC")
#   
#   DfSaveDataFile(dataset, fileName = "tempValues/dataset02.csv", format = "MRMC")
#   xx <- file("goodValues/dataset02.csv", open = "rt")
#   xx1 <- readLines(xx)
#   yy <- file("tempValues/dataset02.csv", open = "rt")
#   yy1 <- readLines(yy)
#   close(xx)
#   close(yy)
#   expect_equivalent(xx1, yy1)# works!
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afroc.ds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     save(ds, file = fn)
#   }
#   
#   load(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment lines to save dataset as Excel file in folder goodValues
#   # DfSaveDataFile(dataset, fileName = "goodValues/dataset02.lrc", format = "MRMC")
#   
#   DfSaveDataFile(dataset, fileName = "tempValues//dataset02.lrc", format = "MRMC")
#   xx <- file("goodValues/dataset02.lrc", open = "rt")
#   xx1 <- readLines(xx)
#   yy <- file("tempValues//dataset02.lrc", open = "rt")
#   yy1 <- readLines(yy)
#   close(xx)
#   close(yy)
#   expect_equivalent(xx1, yy1)# works!
#   
# })

