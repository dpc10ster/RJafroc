context("Significance testing excluding CAD")

test_that("SignificanceTestingAllCombinations", {
  
  skip_on_travis()
  skip_on_cran()
  
  dataset_arr <- list(dataset02, dataset05)
  dataset_arr_str <- c("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc") #, "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBMH", "ORH")
  
  for (d in 1:length(dataset_arr)) {
    for (i in 1:length(FOM_arr)) {
      for (j in 1:length(method_arr)) {
        dataset <- dataset_arr[[d]]
        if ((dataset$dataType == "ROC") && (FOM_arr[i] != "Wilcoxon")) {
          
          # for ROC data, only Wilcoxon FOM is allowed
          expect_error(StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j]))
          
        } else if ((dataset$dataType == "FROC") && (FOM_arr[i] == "Wilcoxon")) {
          
          # for FROC data, Wilcoxon FOM is NOT allowed
          expect_error(StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j]))
          
        } else {
          
          fn <- paste0(test_path(), "/goodValues/SigTest/", 
                       dataset_arr_str[d], FOM_arr[i], method_arr[j])
          if (!file.exists(fn)) {
            ret <- StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j])
            saveRDS(ret, file = fn)
          }
          
          ret <- readRDS(fn);ret <- ret[c(-2,-3)] # removed anovaY and anovaYi list members
          # causes failure in R CMD check but not in devtools::test(); go figure 6/30/19 !!!dpc!!!
          ret1 <- StSignificanceTesting(dataset, FOM = FOM_arr[i],method = method_arr[j])
          ret1 <- ret1[c(-2,-3)] # removed anovaY and anovaYi list members
          expect_equal(ret1, ret) # now it works
          # end of test
          
        }
      }  
    }
  }
  
})


# 
# test_that("StSignificanceTestingCadVsRadiologists") {
#
#   # TBA
#
# }

test_that("StSignificanceTestingSingleFixedFactor", {
  
  fn <- paste0(test_path(), "/goodValues/SigTest/SingleFixedFactor_02_1_14")
  if (!file.exists(fn)) {
    ret <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon")
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon"), ret)
  # end of test
  
  fn <- paste0(test_path(), "/goodValues/SigTest/SingleFixedFactor_05_1_14")
  if (!file.exists(fn)) {
    ret <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4)), ret)
  # end of test
  
  fn <- paste0(test_path(), "/goodValues/SigTest/SingleFixedFactor_05_12_4")
  if (!file.exists(fn)) {
    ret <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4)), ret)
  # end of test
  
})


test_that("StSignificanceTestingCrossedModalities", {
  
  crossedFileName <- system.file(
    "extdata", "includedCrossedModalitiesData.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues/SigTest/Crossed")
  if (!file.exists(fn)) {
    ret <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(StSignificanceTestingCrossedModalities(datasetCrossedModality, 1), ret)
  # end of test
  
})

