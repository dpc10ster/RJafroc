contextStr <- "UtilVarCompDBM: dataset02"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DbmDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilDBMVarComp(dataset, FOM = "Wilcoxon")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilDBMVarComp(dataset, FOM = "Wilcoxon")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarCompDBM: dataset05 wAFROC"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset05
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DbmDatasetwAFROC05", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilDBMVarComp(dataset, FOM = "wAFROC")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilDBMVarComp(dataset, FOM = "wAFROC")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: jackknife dataset02"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/JackknifeDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilOrVarCov(dataset, FOM = "Wilcoxon")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilOrVarCov(dataset, FOM = "Wilcoxon")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: bootstrap dataset02"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/bootstrapDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "bootstrap", 
                              nBoots = 2000, seed = 100)
    saveRDS(ds, file = fn)
  }
  
  # ???DPC???
  # good <- readRDS(fn)
  # current <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
  #                             covEstMethod = "bootstrap", 
  #                             nBoots = 2000, seed = 100)
  # 
  # expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: DeLong dataset02"
context(contextStr)
test_that(contextStr, {

  # ???DPC???
  # dataset <- dataset02
  # fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DeLongDataset02", ".rds")
  # if (!file.exists(fn)) {
  #   warning(paste0("File not found - generating new ",fn))
  #   ds <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
  #                             covEstMethod = "DeLong")
  #   saveRDS(ds, file = fn)
  # }
  # 
  # good <- readRDS(fn)
  # current <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
  #                                covEstMethod = "DeLong")
  # 
  # expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: jackknife dataset04"
context(contextStr)
test_that(contextStr, {

  dataset <- DfFroc2Roc(dataset04)
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/JackknifeDataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "jackknife")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "jackknife")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: bootstrap dataset04"
context(contextStr)
test_that(contextStr, {

  # ???DPC???
  # dataset <- DfFroc2Roc(dataset04)
  # fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/bootstrapDataset04", ".rds")
  # if (!file.exists(fn)) {
  #   warning(paste0("File not found - generating new ",fn))
  #   ds <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
  #                             covEstMethod = "bootstrap", 
  #                             nBoots = 2000, seed = 100)
  #   saveRDS(ds, file = fn)
  # }
  # 
  # good <- readRDS(fn)
  # current <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
  #                                covEstMethod = "bootstrap", 
  #                                nBoots = 2000, seed = 100)
  # 
  # expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: DeLong dataset04"
context(contextStr)
test_that(contextStr, {

  # ???DPC???
  # dataset <- DfFroc2Roc(dataset04)
  # fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DeLongDataset04", ".rds")
  # if (!file.exists(fn)) {
  #   warning(paste0("File not found - generating new ",fn))
  #    ds <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
  #                             covEstMethod = "DeLong")
  #   saveRDS(ds, file = fn)
  # }
  # 
  # good <- readRDS(fn)
  # current <- UtilOrVarCov(dataset, FOM = "Wilcoxon", 
  #                                covEstMethod = "DeLong")
  # 
  # expect_equal(good, current)
  
})



contextStr <- "UtilVarCompOR: toy crossed datasets"
context(contextStr)
test_that(contextStr, {
  
  # FACTRL
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/frocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- UtilOrVarCov(temp, FOM = "wAFROC")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- UtilOrVarCov(temp, FOM = "wAFROC")
  expect_equal(x1, x2)
  
})


