context("StSignificanceTestingCadVsRadiologists")

test_that("1T-RRFC Wilcoxon", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "Wilcoxon", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC PCL 0.05", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "PCL_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.05)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC PCL 0.2", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "PCL_20", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC PCL 1", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "PCL_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 1)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC ALROC 0.05", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "ALROC_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.05)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC ALROC 0.2", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "ALROC_20", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC ALROC 1", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "ALROC_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 1)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC Wilcoxon", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "Wilcoxon", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC PCL 0.05", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "PCL_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.05)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})




test_that("1T-RRRC PCL 0.2", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "PCL_20", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC PCL 1", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "PCL_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 1)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC ALROC 0.05", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "ALROC_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.05)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC ALROC 0.2", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "ALROC_20", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC ALROC 1", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "ALROC_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 1)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC Wilcoxon", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "Wilcoxon", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC")
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC PCL 0.05", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "PCL_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.05)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC PCL 0.2", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "PCL_20", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC PCL 1", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "PCL_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 1)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC ALROC 0.05", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "ALROC_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.05)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC ALROC 0.2", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "ALROC_20", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("2T-RRRC ALROC 1", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "ALROC_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 1)
  
  for (i in 1:length(GoodValues))
  {
    for (j in 1:length(GoodValues[[i]]))
    {
      good <- GoodValues[[i]][j]
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC FROC HrAuc", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRFC_", "HrAuc", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRFC FROC wAFROC", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRFC_", "wAFROC", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC FROC HrAuc", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRRC_", "HrAuc", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



test_that("1T-RRRC FROC wAFROC", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRRC_", "wAFROC", ".rds")
  
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
      current <- CurrentValues[[i]][j]
      expect_equal(good, current)  
    }
  }
  
})



