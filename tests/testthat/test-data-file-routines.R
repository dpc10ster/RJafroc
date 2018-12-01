context("Data file routines")

test_that("Df2RJafrocDataset", {
  set.seed(1)
  NL <- rnorm(5)
  LL <- rnorm(7)*1.5 + 2
  tmp <- tempfile()
  expect_known_output(
    Df2RJafrocDataset(NL, LL), 
    tmp, print = TRUE, update = TRUE)
})

test_that("Df2RJafrocDataset", {
  set.seed(1)
  mu <- 1;lambda <- 1;nu <- 1; zeta1 <- 0
  K1 <- 5;K2 <- 7
  Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
  frocDataRaw <- SimulateFrocDataset(
    mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, lesionNum = Lk2)
  NL <- drop(frocDataRaw$NL)
  LL <- drop(frocDataRaw$LL)
  tmp <- tempfile()
  expect_known_output(
    Df2RJafrocDataset(NL, LL, lesionNum = Lk2), 
    tmp, print = TRUE, update = TRUE)
})


test_that("Df2RJafrocDataset", {
  I <- 2;J <- 3;set.seed(1)
  K1 <- 25;K2 <- 35
  mu <- 1;nuP <- 0.8;lambdaP <- 1;zeta1 <- 0
  lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
  nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
  Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
  z1 <- array(-Inf,dim = c(I,J,K1+K2,40))
  z2 <- array(-Inf,dim = c(I,J,K2,40))
  dimNL <- array(dim=c(I,J,2)) 
  ## the last value (2) accommodates case and location indices
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
  tmp <- tempfile()
  expect_known_output(
    Df2RJafrocDataset(z1, z2, lesionNum = Lk2), 
    tmp, print = TRUE, update = TRUE)
})


test_that("DfBinDatasetROC", {
  tmp <- tempfile()
  expect_known_output(
    DfBinDataset(dataset05, opChType = "ROC"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfBinDatasetAFROC", {
  tmp <- tempfile()
  expect_known_output(
    DfBinDataset(dataset05, opChType = "AFROC"), 
    tmp, print = TRUE, update = TRUE)
})


test_that("DfCreateCorCbmDataset", {
  tmp <- tempfile()
  expect_known_output(
    DfCreateCorCbmDataset(), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfExtractCorCbmDataset", {
  tmp <- tempfile()
  expect_known_output(
    DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = c(1,3)), 
  tmp, print = TRUE, update = TRUE)
})


test_that("DfFroc2Afroc", {
  tmp <- tempfile()
  expect_known_output(
    DfFroc2Afroc(dataset05), 
    tmp, print = TRUE, update = TRUE)
})


test_that("DfFroc2Roc", {
  tmp <- tempfile()
  expect_known_output(
    DfFroc2Roc(dataset05), 
    tmp, print = TRUE, update = TRUE)
})


test_that("DfReadLrocDataFile", {
  tmp <- tempfile()
  expect_known_output(
    DfReadLrocDataFile(), 
    tmp, print = TRUE, update = TRUE)
})


test_that("DfLroc2Roc", {
  dataset <- DfReadLrocDataFile()
  tmp <- tempfile()
  expect_known_output(
    DfLroc2Roc(dataset), 
    tmp, print = TRUE, update = TRUE)
})


test_that("DfReadCrossedModalities", {
  crossedFileName <- system.file(
    "extdata", 
    "includedCrossedModalitiesData.xlsx", 
    package = "RJafroc", 
    mustWork = TRUE)
  tmp <- tempfile()
  expect_known_output(
    DfReadCrossedModalities(crossedFileName), 
    tmp, print = TRUE, update = TRUE)
})


test_that("DfReadDataFile MRMC", {
  tmp <- tempfile()
  fileName <- system.file(
    "extdata", "includedRocData.csv", package = "RJafroc", mustWork = TRUE)
  expect_known_output(
    DfReadDataFile(fileName, format = "MRMC"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  fileName <- system.file(
    "extdata", "includedRocData.lrc", package = "RJafroc", mustWork = TRUE)
  expect_known_output(
    DfReadDataFile(fileName, format = "MRMC"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  fileName <- system.file(
    "extdata", "includedRocData.imrmc", package = "RJafroc", mustWork = TRUE)
  expect_known_output(
    DfReadDataFile(fileName, format = "iMRMC"), 
    tmp, print = TRUE, update = TRUE)
})



test_that("DfReadDataFile JAFROC", {
  tmp <- tempfile()
  fileName <- system.file(
    "extdata", "includedRocData.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_known_output(
    DfReadDataFile(fileName), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  fileName <- system.file(
    "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)
   expect_known_output(
    DfReadDataFile(fileName, renumber = TRUE), 
    tmp, print = TRUE, update = TRUE)
  
})


test_that("DfExtractDataset", {
  tmp <- tempfile()
  expect_known_output(
    DfExtractDataset(dataset05, rdrs = c(1, 3)), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfSaveDataFile", {
  tmp <- tempfile()
  expect_known_output(
    DfSaveDataFile(dataset = dataset05, fileName = "rocData2.xlsx", format = "JAFROC"), 
    tmp, print = TRUE, update = TRUE)
})

test_that("DfSaveDataFile", {
  tmp <- tempfile()
  expect_known_output(
    DfSaveDataFile(dataset = dataset02, fileName = "rocData2.csv", format = "MRMC"), 
    tmp, print = TRUE, update = TRUE)
  tmp <- tempfile()
  expect_known_output(
    DfSaveDataFile(
      dataset = dataset02, fileName = "rocData2.lrc", format = "MRMC"), 
    tmp, print = TRUE, update = TRUE)
})




