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
  
  for (i in 1:dim(ret1)[1]) {
    for (j in 1:dim(ret1)[2])
    expect_equal(as.numeric(ret1[[i,j]]), as.numeric(ret2[[i,j]]))
  }

})



context("UtilFigureOfMerit FROC dataset: wAFROC1, dataset with no non-diseased cases")
test_that("FROC dataset: wAFROC1, dataset with no non-diseased cases", {
  
  fileName <- system.file("extdata", "/toyFiles/FROC/frocLocatClass.xlsx", package = "RJafroc", mustWork = TRUE)
  x <- DfReadDataFile(fileName = fileName, newExcelFileFormat = T)
  
  fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritFROC-", "wAFROC1a", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilFigureOfMerit(x, FOM = "wAFROC1")
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn)
  ret2 <- UtilFigureOfMerit(x, FOM = "wAFROC1")
  
  for (i in 1:length(ret1)) {
    expect_equal(as.numeric(ret1[[i]]), as.numeric(ret2[[i]]))
  }
  
  
})



context("UtilFigureOfMerit: wAFROC1, same FOM with new and old excel formats frocLocatClass.xlsx file")
test_that("UtilFigureOfMerit: wAFROC1, same FOM with new and old excel formatsfrocLocatClass.xlsx file", {
  
  fileName <- system.file("extdata", "/toyFiles/FROC/frocLocatClass.xlsx", package = "RJafroc", mustWork = TRUE)
  x1 <- DfReadDataFile(fileName = fileName, newExcelFileFormat = F)
  x2 <- DfReadDataFile(fileName = fileName, newExcelFileFormat = T)
  
  ret1 <-  UtilFigureOfMerit(x1, FOM = "wAFROC1")
  ret2 <-  UtilFigureOfMerit(x2, FOM = "wAFROC1")
  
  for (i in 1:length(ret1)) {
    expect_equal(as.numeric(ret1[[i]]), as.numeric(ret2[[i]]))
  }
  
  
})


context("UtilFigureOfMerit: wAFROC1, same FOM with new and old excel formats frocCr.xlsx file")
test_that("UtilFigureOfMerit: wAFROC1, same FOM with new and old excel formats frocCr.xlsx file", {
  
  fileName <- system.file("extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  x1 <- DfReadDataFile(fileName = fileName, newExcelFileFormat = F)
  x2 <- DfReadDataFile(fileName = fileName, newExcelFileFormat = T)
  
  ret1 <-  UtilFigureOfMerit(x1, FOM = "wAFROC1")
  ret2 <-  UtilFigureOfMerit(x2, FOM = "wAFROC1")
  
  for (i in 1:length(ret1)) {
    expect_equal(as.numeric(ret1[[i]]), as.numeric(ret2[[i]]))
  }
  
  
})

context("UtilFigureOfMerit FROC dataset: all FOMs except ...")
test_that("FROC dataset: all FOMs except ...", {
  
  # cannot use Wilcoxon with FROC dataset
  # correct usage is HrAuc
  dataset <- dataset01
  expect_error(UtilFigureOfMerit(dataset, FOM = "Wilcoxon"))
  
  ## cycle through all FOMs possible with FROC data (except the excessive computation time ones)
  FOM_arr <- c("HrAuc","wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "HrSp", "HrSe")
  
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


# context("UtilFigureOfMerit FROC data: excessive computation time FOMs")
# test_that("FROC data: excessive computation time FOMs", {
#   
#   skip_on_cran()
#  
#   dataset <- dataset01 # FROC
#   
#   FOM_arr <- c("SongA2","SongA1")
#   
#   for (i in 1:length(FOM_arr)) {
#     
#     FOM  <- FOM_arr[i]
#     
#     fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMeritFROC-", FOM, ".rds")
#     if (!file.exists(fn)) {
#       warning(paste0("File not found - generating new ",fn))
#       ret <- UtilFigureOfMerit(dataset, FOM = FOM)
#       saveRDS(ret, file = fn)
#     }
#     
#     ret1 <- readRDS(fn)
#     ret2 <- UtilFigureOfMerit(dataset, FOM = FOM)
#     
#     for (i in 1:length(ret1)) {
#       expect_equal(as.numeric(ret1[[i]]), as.numeric(ret1[[i]]))
#     }
#     
#   }
#   
# })


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

context("UtilFigureOfMerit FROC")
test_that("UtilFigureOfMerit FROC", {
  
  dataset <- datasetX
  FOM <- "wAFROC"
  
  fn <- paste0(test_path(), "/goodValues361/FOM/UtilFigureOfMerit-", FOM, ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilFigureOfMerit(dataset, FOM = FOM)
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn)
  ret2 <- UtilFigureOfMerit(dataset, FOM = FOM)
  
  for (i in 1:2) {
    expect_equal(ret1[[i]], ret2[[i]])
  }
  
})

