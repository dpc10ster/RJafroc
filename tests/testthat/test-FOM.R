## DPC note 6/23/19: updated incomplete applications of expect_known_output
context("UtilFigureOfMerit tests over all datasets")

test_that("ROC dataset dataset02: FOM = Wilcoxon", {
  dataset <- dataset02
  FOM = "Wilcoxon"
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilFigureOfMerit(dataset, FOM = FOM), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    UtilFigureOfMerit(dataset, FOM = FOM), 
    tmp, print = TRUE, update = TRUE)
  
})


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
    
    tmp <- tempfile()
    expect_warning(expect_known_output(
      UtilFigureOfMerit(dataset, FOM = FOM), 
      tmp, print = TRUE, update = TRUE),
      "Creating reference output")
    
    expect_known_output(
      UtilFigureOfMerit(dataset, FOM = FOM), 
      tmp, print = TRUE, update = TRUE)
    
  }
  
})


test_that("FROC data: excessive computation time FOMs", {
  
  skip_on_cran()
  skip_on_travis()
  
  dataset <- dataset01
  
  FOM_arr <- c("SongA2","SongA1") 
  
  for (i in 1:length(FOM_arr)) {
    
    FOM  <- FOM_arr[i]
    
    tmp <- tempfile()
    expect_warning(expect_known_output(
      UtilFigureOfMerit(dataset, FOM = FOM), 
      tmp, print = TRUE, update = TRUE),
      "Creating reference output")
    
    expect_known_output(
      UtilFigureOfMerit(dataset, FOM = FOM), 
      tmp, print = TRUE, update = TRUE)
    
  }
  
})


test_that("ROI paradigm", {
  
  dataset <- datasetROI
  FOM <- "ROI"
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilFigureOfMerit(dataset, FOM = FOM),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    UtilFigureOfMerit(dataset, FOM = FOM), 
    tmp, print = TRUE, update = TRUE)
  
})


test_that("LROC paradigm: FOM = Wilcoxon", {
  
  dataset <- datasetCadLroc
  FOM_arr <- c("Wilcoxon", "ALROC") 
  
  for (i in 1:length(FOM_arr)) {
    
    FOM <- FOM_arr[i]
    
    tmp <- tempfile()
    expect_warning(expect_known_output(
      UtilFigureOfMerit(dataset, FOM = FOM),
      tmp, print = TRUE, update = TRUE),
      "Creating reference output")
    
    expect_known_output(
      UtilFigureOfMerit(dataset, FOM = FOM),
      tmp, print = TRUE, update = TRUE)
    
  }
  
})


test_that("LROC paradigm: FOM = PCL@FPFValue", {
  
  dataset <- datasetCadLroc
  FOM <- "PCL" 
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    UtilFigureOfMerit(dataset, FOM = FOM, FPFValue = 0.2),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    UtilFigureOfMerit(dataset, FOM = FOM, FPFValue = 0.2), 
    tmp, print = TRUE, update = TRUE)
  
}) 

