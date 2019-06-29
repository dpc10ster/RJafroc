test_that("Df2RJafrocDataset", {
  
  # generate the ratings  
  # a small ROC dataset
  set.seed(1)
  z1 <- rnorm(5)
  z2 <- rnorm(7)*1.5 + 2
  
  fn <- paste0(test_path(), "/goodValues/Df2RJafrocDatasetds01")
  if (!file.exists(fn)) {
    ds <- Df2RJafrocDataset(z1, z2)
    save(ds, file = fn)
    # saveRDS(ds, file = fn)
  }
  
  load(fn)
  # readRDS(fn)
  expect_equal(Df2RJafrocDataset(z1, z2), ds) 
  # end of test

})

  # # generate the ratings
  # # a larger MRMC ROC dataset
  # set.seed(1)
  # I <- 2;J <- 3
  # K1 <- 25;K2 <- 35
  # z1 <- array(dim = c(I, J, K1))
  # z2 <- array(dim = c(I, J, K2))
  # mu <- 2;sigma <- 1.5
  # for (i in 1:I) {
  #   for (j in 1:J) {
  #     z1[i,j,1:K1] <- rnorm(K1)
  #     z2[i,j,] <- rnorm(K2) * sigma + mu
  #   }
  # }
  # 
  # fn <- paste0(test_path(), "/goodValues/Df2RJafrocDatasetds02")
  # if (!file.exists(fn)) {
  #   ds <- Df2RJafrocDataset(z1, z2)
  #   saveRDS(ds, file = fn)
  # }
  # 
  # readRDS(fn)
  # expect_equal(Df2RJafrocDataset(z1, z2), ds) 
  # # end of test  
  # 
  # # generate the ratings  
  # # a FROC dataset
  # set.seed(1)
  # I <- 2;J <- 3
  # K1 <- 25;K2 <- 35
  # mu <- 1;nuP <- 0.8;lambdaP <- 1;zeta1 <- 0
  # lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
  # nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
  # Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
  # z1 <- array(-Inf,dim = c(I,J,K1+K2,40))
  # z2 <- array(-Inf,dim = c(I,J,K2,40))
  # dimNL <- array(dim=c(I,J,2))
  # dimLL <- array(dim=c(I,J,2))
  # for (i in 1:I) {
  #   for (j in 1:J) {
  #     frocDataRaw <- SimulateFrocDataset(
  #       mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, lesionNum = Lk2)
  #     dimNL[i,j,] <- dim(drop(frocDataRaw$NL))
  #     dimLL[i,j,] <- dim(drop(frocDataRaw$LL))
  #     z1[i,j,,1:dimNL[i,j,2]] <- drop(frocDataRaw$NL) # drop the excess location indices
  #     z2[i,j,,1:dimLL[i,j,2]] <- drop(frocDataRaw$LL)
  #   }
  # }
  # z1 <- z1[,,,1:max(dimNL[,,2])]
  # z2 <- z2[,,,1:max(dimLL[,,2])]
  # 
  # fn <- paste0(test_path(), "/goodValues/Df2RJafrocDatasetds03")
  # if (!file.exists(fn)) {
  #   ds <- Df2RJafrocDataset(z1, z2, lesionNum = Lk2) # an FROC dataset
  #   saveRDS(ds, file = fn)
  # }
  # 
  # readRDS(fn)
  # expect_equal(Df2RJafrocDataset(z1, z2, lesionNum = Lk2), ds) # an FROC dataset
  # # end of test
  # 


# test_that("DfBinDataset (ROC&AFROC)", {
#   
#   fn <- paste0(test_path(), "/goodValues/DfBinDatasetROCds01")
#   if (!file.exists(fn)) {
#     ds <- DfBinDataset(dataset05, opChType = "ROC")
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfBinDataset(dataset05, opChType = "ROC"), ds)
#   # end of test
#   
#   dataset <- dataset05 # JT FROC
#   fn <- paste0(test_path(), "/goodValues/DfBinDatasetAFROCds02")
#   if (!file.exists(fn)) {
#     ds <- DfBinDataset(dataset, opChType = "AFROC")
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfBinDataset(dataset, opChType = "AFROC"), ds)
#   
# })

# 
# 
# test_that("DfCreateCorCbmDataset", {
# 
#   fn <- paste0(test_path(), "/goodValues/DfCreateCorCbmDatasetds01")
#   if (!file.exists(fn)) {
#     ds <- DfCreateCorCbmDataset()
#     saveRDS(ds, file = fn)
#   }
# 
#   readRDS(fn)
#   expect_equal(DfCreateCorCbmDataset(), ds)
#   # end of test
# 
# })
# 
# 
# test_that("DfExtractCorCbmDataset", {
# 
#   fn <- paste0(test_path(), "/goodValues/DfExtractCorCbmDatasetds01")
#   if (!file.exists(fn)) {
#     ds <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
#     saveRDS(ds, file = fn)
#   }
# 
#   readRDS(fn)
#   expect_equal(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), ds)
#   # end of test
# 
#   fn <- paste0(test_path(), "/goodValues/DfExtractCorCbmDatasetds02")
#   if (!file.exists(fn)) {
#     ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3))
#     saveRDS(ds, file = fn)
#   }
# 
#   readRDS(fn)
#   expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3)), ds)
#   # end of test
# 
#   fn <- paste0(test_path(), "/goodValues/DfExtractCorCbmDatasetds03")
#   if (!file.exists(fn)) {
#     ds <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 2), ds)
#   # end of test
#   
# })
# 
# 
# test_that("DfFroc2Afroc & DfFroc2Roc & DfreadRDSLrocDataFile & DfLroc2Roc", {
# 
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
# 
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
# 
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Rocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Roc(dataset05)
#     saveRDS(ds, file = fn)
#   }
# 
#   readRDS(fn)
#   expect_equal(DfFroc2Roc(dataset05), ds)
#   # end of test
# 
#   fn <- paste0(test_path(), "/goodValues/DfreadRDSLrocDataFileds01")
#   if (!file.exists(fn)) {
#     ds <- DfreadRDSLrocDataFile()
#     saveRDS(ds, file = fn)
#   }
# 
#   readRDS(fn)
#   expect_equal(DfreadRDSLrocDataFile(), ds)
#   # end of test
# 
#   fn <- paste0(test_path(), "/goodValues/DfreadRDSLrocDataFileds02")
#   if (!file.exists(fn)) {
#     dataset <- DfreadRDSLrocDataFile()
#     ds <- DfLroc2Roc(dataset)
#     saveRDS(ds, file = fn)
#   }
# 
#   dataset <- DfreadRDSLrocDataFile()
#   readRDS(fn)
#   expect_equal(DfLroc2Roc(dataset), ds)
#   # end of test
# 
# })

# 
# test_that("DfreadRDSCrossedModalities", {
#   
#   crossedFileName <- system.file("extdata",
#                                  "includedCrossedModalitiesData.xlsx",
#                                  package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfreadRDSCrossedModalitiesds01")
#   if (!file.exists(fn)) {
#     ds <- DfreadRDSCrossedModalities(crossedFileName)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfreadRDSCrossedModalities(crossedFileName), ds)
#   # end of test
#   
# })
# 
# 
# test_that("DfreadRDSDataFileMRMC", {
#   
#   fileName <- system.file("extdata", "includedRocData.csv", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfreadRDSDataFileMRMCds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   # temp <- DfreadRDSDataFile(fileName, format = "MRMC")
#   # saveRDS(temp, file = "goodValues/DfreadRDSDataFileMRMCds15")
#   
#   readRDS("goodValues/DfreadRDSDataFileMRMCds15")
#   expect_equal(DfreadRDSDataFile(fileName, format = "MRMC"), temp)
#   
#   fileName <- system.file(
#     "extdata", "includedRocData.lrc", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   # temp <- DfreadRDSDataFile(fileName, format = "MRMC")
#   # saveRDS(temp, file = "goodValues/DfreadRDSDataFileLRCds16")
#   
#   readRDS("goodValues/DfreadRDSDataFileLRCds16")
#   expect_equal(DfreadRDSDataFile(fileName, format = "MRMC"), temp)
#   
#   fileName <- system.file(
#     "extdata", "includedRocData.imrmc", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   # temp <- DfreadRDSDataFile(fileName, format = "iMRMC")
#   # saveRDS(temp, file = "goodValues/DfreadRDSDataFileiMRMCds17")
#   
#   readRDS("goodValues/DfreadRDSDataFileiMRMCds17")
#   expect_equal(DfreadRDSDataFile(fileName, format = "iMRMC"), temp)
#   
# })
# 
# 
# test_that("DfreadRDSDataFilesExcel", {
#   
#   fileName <- system.file(
#     "extdata", "includedRocData.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfreadRDSDataFilesExcelds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   temp <- DfreadRDSDataFile(fileName)
#   saveRDS(temp, file = "goodValues/DfreadRDSDataFileXlsxds18")
#   
#   readRDS("goodValues/DfreadRDSDataFileXlsxds18")
#   expect_equal(DfreadRDSDataFile(fileName), temp)
#   
#   fileName <- system.file(
#     "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   # temp <- DfreadRDSDataFile(fileName, renumber = TRUE)
#   # saveRDS(temp, file = "goodValues/DfreadRDSDataFileFrocXlsxds19")
#   
#   readRDS("goodValues/DfreadRDSDataFileFrocXlsxds19")
#   expect_equal(DfreadRDSDataFile(fileName, renumber = TRUE), temp)
#   
# })
# 
# 
# test_that("DfreadRDSDataFileROI", {
#   
#   fileName <- system.file(
#     "extdata", "includedRoiData.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   # temp <- DfreadRDSDataFile(fileName)
#   # saveRDS(temp, file = "goodValues/includedRoiDatads20")
#   
#   readRDS("goodValues/includedRoiDatads20")
#   expect_equal(DfreadRDSDataFile(fileName), temp)
#   
#   ds <- DfreadRDSDataFile(fileName)
#   expect_equal(ds$dataType, "ROI")
#   
# })
# 
# 
# test_that("DfExtractDataset", {
#   
#   dataset <- dataset05
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   # temp <- DfExtractDataset(dataset, rdrs = c(1, 3))
#   # saveRDS(temp, file = "goodValues/DfExtractDatasetds21")
#   
#   readRDS("goodValues/DfExtractDatasetds21")
#   expect_equal(DfExtractDataset(dataset, rdrs = c(1, 3)), temp)
#   
# })
# 
# 
# test_that("DfsaveRDSDataFile", {
#   
#   dataset <- dataset05
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # special case
#   # uncomment next to create dataset and then saveRDS it
#   # DfsaveRDSDataFile(dataset, fileName = "goodValues/dataset05.xlsx", format = "JAFROC")
#   
#   DfsaveRDSDataFile(dataset, fileName = "tempValues/dataset05.xlsx", format = "JAFROC")
#   for (i in 1:3) { # there are 3 worksheets in Excel file
#     dfGood <- readRDSWorkbook("goodValues/dataset05.xlsx", i) # check each sheet individually
#     dfCurrent <- readRDSWorkbook("tempValues/dataset05.xlsx", i)    # do:
#     expect_equivalent(dfGood, dfCurrent)# works!
#   }
#   
#   dataset <- datasetROI
#   
#   # special case
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment next to create dataset and then saveRDS it
#   # DfsaveRDSDataFile(dataset, fileName = "goodValues/datasetROI.xlsx", format = "JAFROC")
#   
#   DfsaveRDSDataFile(dataset, fileName = "tempValues/datasetROI.xlsx", format = "JAFROC")
#   for (i in 1:3) { # there are 3 worksheets in Excel file
#     dfGood <- readRDSWorkbook("goodValues/datasetROI.xlsx", i) # check each sheet individually
#     dfCurrent <- readRDSWorkbook("tempValues/datasetROI.xlsx", i)    # do:
#     expect_equivalent(dfGood, dfCurrent)# works!
#   }
#   
#   dataset <- dataset02 # ROC
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment lines to saveRDS dataset as Excel file in folder goodValues
#   # DfsaveRDSDataFile(dataset, fileName = "goodValues/dataset02.imrmc", format = "iMRMC")
#   
#   DfsaveRDSDataFile(dataset, fileName = "tempValues//dataset02.imrmc", format = "iMRMC")
#   xx <- file("goodValues/dataset02.imrmc", open = "rt")
#   xx1 <- readRDSLines(xx)
#   yy <- file("tempValues//dataset02.imrmc", open = "rt")
#   yy1 <- readRDSLines(yy)
#   close(xx)
#   close(yy)
#   expect_equivalent(xx1, yy1)# works!
# 
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment lines to saveRDS dataset as Excel file in folder goodValues
#   # DfsaveRDSDataFile(dataset, fileName = "goodValues/dataset02.csv", format = "MRMC")
#   
#   DfsaveRDSDataFile(dataset, fileName = "tempValues/dataset02.csv", format = "MRMC")
#   xx <- file("goodValues/dataset02.csv", open = "rt")
#   xx1 <- readRDSLines(xx)
#   yy <- file("tempValues/dataset02.csv", open = "rt")
#   yy1 <- readRDSLines(yy)
#   close(xx)
#   close(yy)
#   expect_equivalent(xx1, yy1)# works!
#   
#   fn <- paste0(test_path(), "/goodValues/DfFroc2Afrocds01")
#   if (!file.exists(fn)) {
#     ds <- DfFroc2Afroc(dataset05)
#     saveRDS(ds, file = fn)
#   }
#   
#   readRDS(fn)
#   expect_equal(DfFroc2Afroc(dataset05), ds)
#   # end of test
#   
#   # uncomment lines to saveRDS dataset as Excel file in folder goodValues
#   # DfsaveRDSDataFile(dataset, fileName = "goodValues/dataset02.lrc", format = "MRMC")
#   
#   DfsaveRDSDataFile(dataset, fileName = "tempValues//dataset02.lrc", format = "MRMC")
#   xx <- file("goodValues/dataset02.lrc", open = "rt")
#   xx1 <- readRDSLines(xx)
#   yy <- file("tempValues//dataset02.lrc", open = "rt")
#   yy1 <- readRDSLines(yy)
#   close(xx)
#   close(yy)
#   expect_equivalent(xx1, yy1)# works!
#   
# })

