context("StSignificanceTestingCadVsRadiologists")

test_that("1T-RRFC Wilcoxon", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "1T-RRFC", "Wilcoxon", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRFC")
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC PCL", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "1T-RRFC", "PCL", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.2)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC ALROC", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "1T-RRFC", "ALROC", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.2)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC Wilcoxon", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "1T-RRRC", "Wilcoxon", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRRC")
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC PCL", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "1T-RRRC", "PCL", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.2)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC ALROC", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "1T-RRRC", "ALROC", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.2)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC Wilcoxon", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "2T-RRRC", "Wilcoxon", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC", FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC", FPFValue = 0.2)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC PCL", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "2T-RRRC", "PCL", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.2)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC ALROC", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "2T-RRRC", "ALROC", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.2)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC FROC HrAuc", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "1T-RRFC", "HrAuc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRFC")
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC FROC wAFROC", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "1T-RRFC", "wAFROC", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRFC")
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC FROC HrAuc", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "1T-RRRC", "HrAuc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRRC")
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC FROC wAFROC", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "1T-RRRC", "wAFROC", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRRC")
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- GoodValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



