# > devtools::test()
# Loading RJafroc
# Testing RJafroc
# ✔ |  OK F W S | Context
# ✔ |   1       | compare-3fits [6.5 s]
# ✔ |   2       | corcbm [1.2 s]
# ✔ |  33       | data-file [8.3 s]
# ✔ |  33       | data-file
# ✔ |  18       | Fitting routines [195.9 s]


contextStr <- "FitBinormalRoc - allow AUC less than 0.5"
context(contextStr)
test_that(contextStr, {
  # skip_on_travis()
  g1 <- c(2, 3, 5, 6)
  g2 <- c(0, 4, 7, 11)
  d <- Df2RJafrocDataset(g2, g1, InputIsCountsTable = TRUE)
  fit <- FitBinormalRoc(d)
  
  fn <- paste0(test_path(), "/goodValues361/Fitting/BinormalRoc01", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitBinormalRoc(d)[1:7]
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(FitBinormalRoc(d)[1:7], ret)
  # end of test
  
})


contextStr <- "FitBinormalRoc2"
context(contextStr)
test_that(contextStr, {
  # skip_on_travis()
  fn <- paste0(test_path(), "/goodValues361/Fitting/BinormalRoc02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitBinormalRoc(dataset02)[1:7]
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(FitBinormalRoc(dataset02)[1:7], ret)
  # end of test
  
})


contextStr <- "FitBinormalRoc3"
context(contextStr)
test_that(contextStr, {
  # skip_on_travis()
  fn <- paste0(test_path(), "/goodValues361/Fitting/BinormalRoc05", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitBinormalRoc(DfBinDataset(dataset05, desiredNumBins = 5, opChType = "ROC"))
    ret <- ret[1:7]
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  ret1 <- FitBinormalRoc(DfBinDataset(dataset05, desiredNumBins = 5, opChType = "ROC"))
  ret1 <- ret1[1:7]
  expect_equal(ret1, ret)
  # end of test
  
})


contextStr <- "FitCbmRoc"
context(contextStr)
test_that(contextStr, {
  # skip_on_travis()
  fn <- paste0(test_path(), "/goodValues361/Fitting/CbmRoc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitCbmRoc(dataset02)[1:7]
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(FitCbmRoc(dataset02)[1:7], ret)
  # end of test

})


contextStr <- "FitCorCbm"
context(contextStr)
test_that(contextStr, {
  skip_on_travis()
  skip_on_cran()
  skip_on_os("mac") # as it is very time consuming, and I am not changing the code in this part of the pkg
  
  fn <- paste0(test_path(), "/goodValues361/Fitting/CorCbmdataset05", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitCorCbm(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(4,7)))
    ret <- ret$fitCorCbmRet
    ret <- ret[1:10] # leave out covariance matrix and plots
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  ret1 <- FitCorCbm(DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(4,7)))
  ret1 <- ret1$fitCorCbmRet
  ret1 <- ret1[1:10] # leave out covariance matrix and plots
  expect_equal(ret1, ret)
  # end of test

})


contextStr <- "FitCorCbm123"
context(contextStr)
test_that(contextStr, {
  skip_on_travis()
  skip_on_cran()
  skip_on_os("mac") # as it is very time consuming, and I am not changing the code in this part of the pkg
  
  fn <- paste0(test_path(), "/goodValues361/Fitting/CorCbmdatasetBinned123", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitCorCbm(datasetBinned123)
    ret <- ret$fitCorCbmRet
    ret <- ret[1:10] # leave out covariance matrix and plots
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  ret1 <- FitCorCbm(datasetBinned123)
  ret1 <- ret1$fitCorCbmRet
  ret1 <- ret1[1:10] # leave out covariance matrix and plots
  expect_equal(ret1, ret)
  # end of test
  
})


contextStr <- "FitCorCbm124"
context(contextStr)
test_that(contextStr, {
  skip_on_travis()
  skip_on_cran()
  skip_on_os("mac") # as it is very time consuming, and I am not changing the code in this part of the pkg
  
  fn <- paste0(test_path(), "/goodValues361/Fitting/CorCbmdatasetBinned124", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitCorCbm(datasetBinned124)
    ret <- ret$fitCorCbmRet
    ret <- ret[1:10] # leave out covariance matrix and plots
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  ret1 <- FitCorCbm(datasetBinned124)
  ret1 <- ret1$fitCorCbmRet
  ret1 <- ret1[1:10] # leave out covariance matrix and plots
  expect_equal(ret1, ret)
  # end of test
  
})


contextStr <- "FitCorCbm125"
context(contextStr)
test_that(contextStr, {
  skip_on_travis()
  skip_on_cran()
  skip_on_os("mac") # as it is very time consuming, and I am not changing the code in this part of the pkg
  
  fn <- paste0(test_path(), "/goodValues361/Fitting/CorCbmdatasetBinned125", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitCorCbm(datasetBinned125)
    ret <- ret$fitCorCbmRet
    ret <- ret[1:10] # leave out covariance matrix and plots
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  ret1 <- FitCorCbm(datasetBinned125)
  ret1 <- ret1$fitCorCbmRet
  ret1 <- ret1[1:10] # leave out covariance matrix and plots
  expect_equal(ret1, ret)
  # end of test
  
})


contextStr <- "FitRsmRoc"
context(contextStr)
test_that(contextStr, {
  skip_on_cran()
  skip_on_travis()
  
  fn <- paste0(test_path(), "/goodValues361/Fitting/RsmRoc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitRsmRoc(dataset02, UtilLesionDistr(dataset02))[1:8]
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(FitRsmRoc(dataset02, UtilLesionDistr(dataset02))[1:8], ret, tolerance = 1e-6)
  # end of test

  fn <- paste0(test_path(), "/goodValues361/Fitting/RsmRocDegenerate", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- FitRsmRoc(datasetDegenerate, UtilLesionDistr(datasetDegenerate))[1:8]
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(FitRsmRoc(datasetDegenerate, UtilLesionDistr(datasetDegenerate))[1:8], ret, tolerance = 1e-6)
  # end of test
  
})
