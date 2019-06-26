context("Significance testing excluding CAD")

test_that("SignificanceTestingAllCombinations", {

  skip_on_travis()
  skip_on_cran()
  
# spent most time on this; dpc 06/26/19  
  dataset_arr <- list(dataset02, dataset05)
  FOM_arr <- c("Wilcoxon", "HrAuc", "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBMH", "ORH")
  options_arr <- c("RRRC", "FRRC", "RRFC")
  ## cycle through all representative datasets, FOMs, methods and options
  for (d in 1:length(dataset_arr)) {
    dataset <- dataset_arr[[d]]
    for (i in 1:length(FOM_arr)) {
      for (j in 1:length(method_arr)) {
        for (k in 1:length(options_arr)) {
          if ((dataset$dataType == "ROC") && (FOM_arr[i] != "Wilcoxon")) {
            
            # for ROC data, only Wilcoxon FOM is allowed
            expect_error(
              StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j]), option = options_arr[k])
            
          } else if ((dataset$dataType == "FROC") && (FOM_arr[i] == "Wilcoxon")) {
            
            # for FROC data, Wilcoxon FOM is NOT allowed
            expect_error(
              StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j]), option = options_arr[k])
            
          } else {
            
            tmp <- tempfile()
            expect_warning(expect_known_output(
              StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j], option = options_arr[k]),
              tmp, print = TRUE, update = TRUE),
              "Creating reference output")
            
            expect_known_output(
              StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j], option = options_arr[k]),
              tmp, print = TRUE, update = TRUE)
            
          }
          
        }  
      }
    }
  }
})

# TODO::DPC needs updating to meet quality of code above 6/26/19
test_that("StSignificanceTestingSingleFixedFactor", {
  
  tmp <- tempfile()
  singleFactorData <- DfExtractDataset(dataset02, 1, 1:4)
  expect_warning(expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "Wilcoxon"),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "Wilcoxon"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  singleFactorData <- DfExtractDataset(dataset05, 1, 1:4)
  expect_warning(expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE)
  
  tmp <- tempfile()
  singleFactorData <- DfExtractDataset(dataset05, 1:2, 4)
  expect_warning(expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "wAFROC"),
    tmp, print = TRUE, update = TRUE)
  
})


test_that("StSignificanceTestingCrossedModalities", {
  
  tmp <- tempfile()
  crossedFileName <- system.file(
    "extdata", "includedCrossedModalitiesData.xlsx", package = "RJafroc", mustWork = TRUE)
  crossedData <- DfReadCrossedModalities(crossedFileName)
  expect_warning(expect_known_output(
    StSignificanceTestingCrossedModalities(datasetCrossedModality, 1),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    StSignificanceTestingCrossedModalities(datasetCrossedModality, 1),
    tmp, print = TRUE, update = TRUE)
  
})

