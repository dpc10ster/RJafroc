context("UtilVarCompDBM: dataset02")
test_that("DBM method", {
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


context("UtilVarCompDBM: dataset05 wAFROC")
test_that("DBM method wAFROC", {
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


context("UtilVarComp: jackknife dataset02")
test_that("jackknife method", {
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/JackknifeDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "jackknife")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "jackknife")
  
  expect_equal(good, current)
  
})


context("UtilVarComp: bootstrap dataset02")
test_that("bootstrap method", {
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/bootstrapDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "bootstrap", 
                              nBoots = 2000, seed = 100)
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "bootstrap", 
                              nBoots = 2000, seed = 100)
  
  expect_equal(good, current)
  
})


context("UtilVarComp: DeLong dataset02")
test_that("DeLong method", {
  dataset <- dataset02
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DeLongDataset02", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "DeLong")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "DeLong")
  
  expect_equal(good, current)
  
})


context("UtilVarComp: jackknife dataset04")
test_that("jackknife method", {
  dataset <- DfFroc2Roc(dataset04)
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/JackknifeDataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "jackknife")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "jackknife")
  
  expect_equal(good, current)
  
})


context("UtilVarComp: bootstrap dataset04")
test_that("bootstrap method", {
  dataset <- DfFroc2Roc(dataset04)
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/bootstrapDataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "bootstrap", 
                              nBoots = 2000, seed = 100)
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "bootstrap", 
                                 nBoots = 2000, seed = 100)
  
  expect_equal(good, current)
  
})


context("UtilVarComp: DeLong dataset04")
test_that("DeLong method", {
  dataset <- DfFroc2Roc(dataset04)
  fn <- paste0(test_path(), "/goodValues361/UtilVarComponents/DeLongDataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
     ds <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                              covEstMethod = "DeLong")
    saveRDS(ds, file = fn)
  }
  
  good <- readRDS(fn)
  current <- UtilVarComponentsOR(dataset, FOM = "Wilcoxon", 
                                 covEstMethod = "DeLong")
  
  expect_equal(good, current)
  
})



