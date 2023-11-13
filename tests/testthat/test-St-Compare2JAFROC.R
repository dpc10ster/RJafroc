contextStr <- "DBM varComp vs Windows JAFROC St HrAuc"
context(contextStr)
test_that(contextStr, {
  # this is the same dataset that Jason Cai found the error on
  fileName <- system.file("extdata", "Froc.xlsx",
                          package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/JAFROC/", "FrocData", "HrAuc", "DBM", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    # these values were obtained from JAFROC output file: 
    # inst/VarCompDiscrepancy/includedFrocData_Inferred_ROC.txt
    # in version 1.3.1
    varComp <- list(
      varR = 0.00182911,
      varC = 0.04347403,
      varTR = -0.0002401,
      varTC = 0.00076895,
      varRC = 0.02403484,
      varErr = 0.09253638
    )
    saveRDS(varComp, file = fn)
  }
  
  goodValues <- readRDS(fn)
  goodValues <- as.numeric(as.vector(as.matrix(goodValues)))
  
  # following tests the DBM branch of St
  currentValues <- St(ds, FOM = "HrAuc", method = "DBM")$ANOVA$VarCom
  currentValues <- as.vector(as.matrix(currentValues))
  for (i in 1: length(goodValues)){
    x <- as.numeric(goodValues[[i]])
    y <- as.numeric(currentValues[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
})



contextStr <- "OR varComp vs Windows JAFROC St HrAuc"
context(contextStr)
test_that(contextStr, {
  # this is the same dataset that Jason Cai found the error on
  fileName <- system.file("extdata", "Froc.xlsx",
                          package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/JAFROC/", "FrocData", "HrAuc", "OR", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    # these values were obtained from JAFROC output file:
    # inst/VarCompDiscrepancy/includedFrocData_Inferred_ROC.txt
    # in version 1.3.1
    varComp <- list(
      varR = 0.00182911,
      varTR = -0.000240099,
      Cov1 = 0.000337545,
      Cov2 = 0.000221216,
      Cov3 = 0.000217371,
      varErr = 0.000804072
    )
    saveRDS(varComp, file = fn)
  }
  
  goodValues <- readRDS(fn)
  goodValues <- as.numeric(as.vector(as.matrix(goodValues)))
  
  # following tests the OR branch of St
  currentValues <- St(ds, FOM = "HrAuc", method = "OR")$ANOVA$VarCom[,1]
  currentValues <- as.vector(as.matrix(currentValues))
  for (i in 1: length(goodValues)){
    x <- as.numeric(goodValues[[i]])
    y <- as.numeric(currentValues[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
})



contextStr <- "UtilMeanSquares vs Windows JAFROC: dataset05 MaxLLF"
context(contextStr)
test_that(contextStr, {

  ds <- dataset05
  fn <- paste0(test_path(), "/goodValues361/SigTest/JAFROC/", "dataset05", "MaxLLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    MS <- list(
      msT = 2.70037349,
      msR = 1.10846594,
      msC = 0.49907721,
      msTR = 0.02194054,
      msTC = 0.30908588,
      msRC = 0.08529043,
      msTRC = 0.05436326
    )
    saveRDS(MS, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilMeanSquares(dataset05, FOM = "MaxLLF")
  x2 <- x2[-(8:9)]
  
  currentValues <- as.vector(as.matrix(x2))
  for (i in 1: length(x1)){
    x <- as.numeric(x1[[i]])
    y <- as.numeric(x2[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
})



contextStr <- "UtilMeanSquares vs Windows JAFROC: dataset05 MaxNLF"
context(contextStr)
test_that(contextStr, {
  
  ds <- dataset05
  fn <- paste0(test_path(), "/goodValues361/SigTest/JAFROC/", "dataset05", "MaxNLF", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    MS <- list(
      msT = 1.69012351,
      msR = 9.13734535,
      msC = 1.53860759,
      msTR = 2.12067904,
      msTC = 0.54365865,
      msRC = 0.51171425,
      msTRC = 0.45716879
    )
    saveRDS(MS, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilMeanSquares(dataset05, FOM = "MaxNLF")
  x2 <- x2[-(8:9)]
  
  currentValues <- as.vector(as.matrix(x2))
  for (i in 1: length(x1)){
    x <- as.numeric(x1[[i]])
    y <- as.numeric(x2[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
})
