context("Check DBM varComp against JAFROC output")

test_that("Ensure that DBM varComp values match those from Windows JAFROC", {
  
  # this is the same dataset that Jason Cai found the error on
  fileName <- system.file("extdata", "FrocData.xlsx",
                          package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/JAFROC/", "FrocData", "HrAuc", "DBM", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    # these values were obtained from JAFROC output file: 
    # inst/VarCompDiscrepancy/includedFrocData_Inferred_ROC.txt
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
  
  # following tests the DBM branch of StSignificanceTesting
  ret <- StSignificanceTesting(ds, FOM = "HrAuc", method = "DBMH")
  currentValues <- as.list(ret$varComp)
  for (i in 1: length(goodValues)){
      x <- as.numeric(goodValues[[i]])
      y <- as.numeric(currentValues[[i]])
      expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
})

context("Check OR varComp against JAFROC output")

test_that("Ensure that OR varComp values match those from Windows JAFROC", {

  # this is the same dataset that Jason Cai found the error on
  fileName <- system.file("extdata", "FrocData.xlsx",
                          package = "RJafroc", mustWork = TRUE)
  ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)

  fn <- paste0(test_path(), "/goodValues361/SigTest/JAFROC/", "FrocData", "HrAuc", "OR", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    # these values were obtained from JAFROC output file:
    # inst/VarCompDiscrepancy/includedFrocData_Inferred_ROC.txt
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

  # following tests the OR branch of StSignificanceTesting
  ret <- StSignificanceTesting(ds, FOM = "HrAuc", method = "ORH")
  currentValues <- as.list(ret$varComp)
  for (i in 1: length(goodValues)){
    x <- as.numeric(goodValues[[i]])
    y <- as.numeric(currentValues[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
})

