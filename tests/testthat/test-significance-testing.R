context("Significance testing excluding CAD")

test_that("SignificanceTestingAllCombinations", {
  
  # skip_on_travis()
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
            warning(paste0("File not found - generating new ",fn))
            ret <- StSignificanceTesting(dataset, FOM = FOM_arr[i], method = method_arr[j])
            saveRDS(ret, file = fn)
          }
          
          ret <- readRDS(fn)
          # attributes(ret) <- NULL
          # ret <- ret[c(-2,-3)] # removed anovaY and anovaYi list members
          # causes failure in R CMD check but not in devtools::test(); go figure 6/30/19 !!!dpc!!!
          # causes failure in R CMD check but not in devtools::test(); go figure 7/12/19 !!!dpc!!!
          ret1 <- StSignificanceTesting(dataset, FOM = FOM_arr[i],method = method_arr[j])
          # attributes(ret1) <- NULL
          # ret1 <- ret1[c(-2,-3)] # removed anovaY and anovaYi list members
          expect_equivalent(ret1, ret, # could use expect_equivalent and then I don't have to set attributes to NULL
            info = paste0("Dataset = ",dataset_arr_str[[d]],", FOM = ",FOM_arr[i],", method = ",method_arr[j])
          )
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
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon")
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  ret1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon")
  expect_equal(ret1, ret)
  # end of test
  
  fn <- paste0(test_path(), "/goodValues/SigTest/SingleFixedFactor_05_1_14")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  ret1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4))
  expect_equal(ret1, ret)
  # end of test
  
  fn <- paste0(test_path(), "/goodValues/SigTest/SingleFixedFactor_05_12_4")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  ret1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4))
  expect_equal(ret1, ret)
  # end of test
  
})

#
# TODO: fix travis developer failure on this test as per saved log
# Probably need to set attributes explicitly
# Replacing expect_equal with expect_equivalent may fix this: DID NOT WORK
# Temporary fix: just comment out the test
# 7/12/19: added this back; passed on new version of R 3.6.1
test_that("StSignificanceTestingCrossedModalities", {

  crossedFileName <- system.file(
    "extdata", "includedCrossedModalitiesData.xlsx", package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues/SigTest/CrossedModalities")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  ret1 <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
  expect_equivalent(ret1, ret) # !!!dpc!!! 7/1/19
  # end of test

})

