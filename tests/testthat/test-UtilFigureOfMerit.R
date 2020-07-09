context("UtilFigureOfMerit FROC SPLIT-PLOT dataset, FOM = wAFROC")
test_that("FROC SPLIT-PLOT dataset, FOM = wAFROC", {
  
  dataset <- datasetFROCSp
  FOM = "wAFROC"
  
  fn <- paste0(test_path(), "/goodValues361/FOM/datasetFROCSpwAFROC-", FOM, ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilFigureOfMerit(dataset, FOM = FOM)
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn)
  ret2 <- UtilFigureOfMerit(dataset, FOM = FOM)
  
  expect_equal(ret1, ret2)
  
})


context("UtilFigureOfMerit ROC dataset dataset02: FOM = Wilcoxon")
test_that("ROC dataset dataset02: FOM = Wilcoxon", {
  
  dataset <- dataset02
  FOM = "Wilcoxon"
  
  fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritROC-", FOM, ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilFigureOfMerit(dataset, FOM = FOM)
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn)
  ret2 <- UtilFigureOfMerit(dataset, FOM = FOM)
  
  expect_equal(ret1, ret2)
  
})


context("UtilFigureOfMerit FROC dataset: all FOMs except ...")
test_that("FROC dataset: all FOMs except ...", {
  
  # cannot use Wilcoxon with FROC dataset
  # correct usage is HrAuc
  dataset <- dataset01
  expect_error(UtilFigureOfMerit(dataset, FOM = "Wilcoxon"))
  
  ## cycle through all FOMs possible with FROC data (except the excessive computation time ones)
  FOM_arr <- c("HrAuc","wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases","ExpTrnsfmSp",
               "HrSp", "HrSe")
  
  for (i in 1:length(FOM_arr)) {
    
    FOM  <- FOM_arr[i]
    
    fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritFROC-", FOM, ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      ret <- UtilFigureOfMerit(dataset, FOM = FOM)
      saveRDS(ret, file = fn)
    }
    
    ret1 <- readRDS(fn)
    ret2 <- UtilFigureOfMerit(dataset, FOM = FOM)
    
    for (i in 1:length(ret1)) {
      expect_equal(as.numeric(ret1[[i]]), as.numeric(ret1[[i]]))
    }
    
  }
  
})


context("UtilFigureOfMerit FROC data: excessive computation time FOMs")
test_that("FROC data: excessive computation time FOMs", {
  
  skip_on_cran()
  skip_on_travis()
  
  dataset <- dataset01 # FROC
  
  FOM_arr <- c("SongA2","SongA1")
  
  for (i in 1:length(FOM_arr)) {
    
    FOM  <- FOM_arr[i]
    
    fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritFROC-", FOM, ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      ret <- UtilFigureOfMerit(dataset, FOM = FOM)
      saveRDS(ret, file = fn)
    }
    
    ret1 <- readRDS(fn)
    ret2 <- UtilFigureOfMerit(dataset, FOM = FOM)
    
    for (i in 1:length(ret1)) {
      expect_equal(as.numeric(ret1[[i]]), as.numeric(ret1[[i]]))
    }
    
  }
  
})


context("UtilFigureOfMerit ROI paradigm")
test_that("ROI paradigm", {
  
  dataset <- datasetROI
  FOM <- "ROI"
  
  fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritROI", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilFigureOfMerit(dataset, FOM = FOM)
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn)
  ret2 <- UtilFigureOfMerit(dataset, FOM = FOM)
  
  for (i in 1:length(ret1)) {
    expect_equal(as.numeric(ret1[[i]]), as.numeric(ret1[[i]]))
  }
  
})


context("UtilFigureOfMerit LROC paradigm: FOM = Wilcoxon, ALROC")
test_that("LROC paradigm: FOM = Wilcoxon, ALROC", {
  
  dataset <- datasetCadLroc
  FOM_arr <- c("Wilcoxon", "ALROC", "PCL")
  
  for (i in 1:length(FOM_arr)) {
    
    FOM <- FOM_arr[i]
    
    fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritLROC-", FOM, ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      ret <- UtilFigureOfMerit(dataset, FOM = FOM, FPFValue = 0.2)
      saveRDS(ret, file = fn)
    }
    
    ret1 <- readRDS(fn)
    ret2 <- UtilFigureOfMerit(dataset, FOM = FOM, FPFValue = 0.2)
    
    for (i in 1:length(ret1)) {
      expect_equal(as.numeric(ret1[[i]]), as.numeric(ret1[[i]]))
    }
    
  }
  
})


context("UtilFigureOfMerit LROC paradigm: FOM = PCL@FPFValue")
test_that("LROC paradigm: FOM = PCL@FPFValue", {
  
  dataset <- datasetCadLroc
  FOM <- "PCL"
  
  fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritLROC-", FOM, ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilFigureOfMerit(dataset, FOM = FOM, FPFValue = 0.2)
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn)
  ret2 <- UtilFigureOfMerit(dataset, FOM = FOM, FPFValue = 0.2)
  
  for (i in 1:length(ret1)) {
    expect_equal(as.numeric(ret1[[i]]), as.numeric(ret1[[i]]))
  }
  
  
})

