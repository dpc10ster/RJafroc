contextStr <- "DfFroc2Roc"
context(contextStr)
test_that(contextStr, {
  fn <- paste0(test_path(), "/goodValues361/DfXroc2Yroc/DfFroc2Roc", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfFroc2Roc(dataset05)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfFroc2Roc(dataset05), ds)
  
})
  

# contextStr <- "DfReadLrocDataFile"
# context(contextStr)
# test_that(contextStr, {
# 
#   fn <- paste0(test_path(), "/goodValues361/DfXroc2Yroc/DfReadLrocDataFile", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     ds <- DfReadLrocDataFile()
#     saveRDS(ds, file = fn)
#   }
# 
#   ds <- readRDS(fn)
#   expect_equal(DfReadLrocDataFile(), ds)
#   
# })



contextStr <- "DfLroc2Roc"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/DfXroc2Yroc/DfLroc2Roc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfLroc2Roc(datasetCadLroc)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfLroc2Roc(datasetCadLroc), ds)

})



contextStr <- "DfLroc2Froc"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/DfXroc2Yroc/DfLroc2Froc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfLroc2Froc(datasetCadLroc)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfLroc2Froc(datasetCadLroc), ds)
  
})



contextStr <- "DfFroc2Lroc"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/DfXroc2Yroc/DfFroc2Lroc", ".rds")
  
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfFroc2Lroc(datasetCadLroc)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfFroc2Lroc(datasetCadLroc), ds)
  
  lrocDataset <- DfFroc2Lroc(dataset05)
  frocHrAuc <- UtilFigureOfMerit(dataset05, FOM = "HrAuc")   
  lrocWilcoxonAuc <- UtilFigureOfMerit(lrocDataset, FOM = "Wilcoxon")
  expect_equal(frocHrAuc, lrocWilcoxonAuc)
  
})


