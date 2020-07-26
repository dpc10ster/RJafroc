contextStr <- "UtilVarCompDBM: dataset02"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DbmDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsDBM(dataset, FOM = "Wilcoxon")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsDBM(dataset, FOM = "Wilcoxon")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarCompDBM: dataset05 wAFROC"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset05
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DbmDatasetwAFROC05", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsDBM(dataset, FOM = "wAFROC")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsDBM(dataset, FOM = "wAFROC")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: jackknife dataset02"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/JackknifeDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "jackknife")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "jackknife")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: bootstrap dataset02"
context(contextStr)
test_that(contextStr, {
  
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/bootstrapDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "bootstrap", 
                              nBoots = 2000, seed = 100)
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "bootstrap", 
                              nBoots = 2000, seed = 100)
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: DeLong dataset02"
context(contextStr)
test_that(contextStr, {

  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DeLongDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "DeLong")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "DeLong")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: jackknife dataset04"
context(contextStr)
test_that(contextStr, {

  dataset <- DfFroc2Roc(dataset04)
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/JackknifeDataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "jackknife")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "jackknife")
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: bootstrap dataset04"
context(contextStr)
test_that(contextStr, {

  dataset <- DfFroc2Roc(dataset04)
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/bootstrapDataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "bootstrap", 
                              nBoots = 2000, seed = 100)
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "bootstrap", 
                                 nBoots = 2000, seed = 100)
  
  expect_equal(good, current)
  
})


contextStr <- "UtilVarComp: DeLong dataset04"
context(contextStr)
test_that(contextStr, {

  dataset <- DfFroc2Roc(dataset04)
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DeLongDataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
     ds <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "DeLong")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsORFactorial(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "DeLong")
  
  expect_equal(good, current)
  
})



