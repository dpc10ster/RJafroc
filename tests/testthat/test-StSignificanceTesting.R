# this also tests the factorial dataset PseudoValues function
contextStr <- "StSignificanceTesting: AllCombinations"
context(contextStr)
test_that(contextStr, {
  
  skip_on_cran()

  dataset_arr <- c("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc", "wAFROC","AFROC", "wAFROC1","AFROC1",
               "MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBM", "OR")
  for (d in 1:length(dataset_arr)) {
      for (f in 1:length(FOM_arr)) {
        for (m in 1:length(method_arr)) {
        dataset <- get(dataset_arr[d])
        if ((dataset$descriptions$type == "ROC") && (FOM_arr[f] != "Wilcoxon")) {
          
          # for ROC data, only Wilcoxon FOM is allowed
          expect_error(StSignificanceTesting(dataset, FOM = FOM_arr[f], method = method_arr[m]))
          
        } else if ((dataset$descriptions$type == "FROC") && (FOM_arr[f] == "Wilcoxon")) {
          
          # for FROC data, Wilcoxon FOM is NOT allowed
          expect_error(StSignificanceTesting(dataset, FOM = FOM_arr[f], method = method_arr[m]))
          
        } else {
          
          fn <- paste0(test_path(), "/goodValues361/SigTest/", dataset_arr[d], FOM_arr[f], method_arr[m], ".rds")
          if (!file.exists(fn)) {
            warning(paste0("File not found - generating new ",fn))
            x1 <- StSignificanceTesting(dataset, FOM = FOM_arr[f], method = method_arr[m])
            saveRDS(x1, file = fn)
          }
          
          x1 <- readRDS(fn)
          x2 <- StSignificanceTesting(dataset, FOM = FOM_arr[f],method = method_arr[m])

          expect_equal(x1,x2)
        }
      }
    }
  }
  
})



contextStr <- "StSignificanceTestingCrossedModalities"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/SigTest/CrossedModalities", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    goodValues <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
    saveRDS(goodValues, file = fn)
  }

  goodValues <- readRDS(fn)
  currentValues <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)
  expect_equal(goodValues, currentValues)

})

