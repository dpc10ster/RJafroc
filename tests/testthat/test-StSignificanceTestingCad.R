contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC Wilcoxon"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "Wilcoxon", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRFC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC PCL 0.05"
context(contextStr)
test_that(contextStr, {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "PCL_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.05)
  
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


contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC PCL 0.2"
context(contextStr)
test_that(contextStr, {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "PCL_20", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "1T-RRFC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC PCL 1"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "PCL_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 1)
  
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


contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC ALROC 0.05"
context(contextStr)
test_that(contextStr, {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "ALROC_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.05)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC ALROC 0.2"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "ALROC_20", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "1T-RRFC")
  
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


contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC ALROC 1"
context(contextStr)
test_that(contextStr, {
  
  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRFC_", "ALROC_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 1)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC Wilcoxon"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "Wilcoxon", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRRC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC PCL 0.05"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "PCL_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.05)
  
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




contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC PCL 0.2"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "PCL_20", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "1T-RRRC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC PCL 1"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "PCL_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 1)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC ALROC 0.05"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "ALROC_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.05)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC ALROC 0.2"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "ALROC_20", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "1T-RRRC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC ALROC 1"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_1T-RRRC_", "ALROC_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 1)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 2T-RRRC Wilcoxon"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "Wilcoxon", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 2T-RRRC PCL 0.05"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "PCL_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.05)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 2T-RRRC PCL 0.2"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "PCL_20", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "2T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "2T-RRRC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 2T-RRRC PCL 1"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "PCL_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 1)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 2T-RRRC ALROC 0.05"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "ALROC_05", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.05)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.05)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 2T-RRRC ALROC 0.2"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "ALROC_20", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "2T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "2T-RRRC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 2T-RRRC ALROC 1"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadLroc", "_2T-RRRC_", "ALROC_10", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 1)
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 1)
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC FROC HrAuc"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRFC_", "HrAuc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRFC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRFC FROC wAFROC"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRFC_", "wAFROC", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRFC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRFC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC FROC HrAuc"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRRC_", "HrAuc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRRC")
  
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



contextStr <- "StSignificanceTestingCadVsRad: 1T-RRRC FROC wAFROC"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTestCad/",
               "datasetCadSimuFroc", "_1T-RRRC_", "wAFROC", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRRC")
    saveRDS(ret, file = fn)
  }
  
  GoodValues <- readRDS(fn) # good values
  CurrentValues <- StSignificanceTestingCadVsRad(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRRC")
  
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



