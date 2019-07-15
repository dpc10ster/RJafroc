context("UtilMeanSquares")

test_that("UtilMeanSquares", {
  
  dataset_arr <- list(dataset02, dataset05)
  dataset_arr_str <- list("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc", "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBMH", "ORH")
  # options_arr <- c("RRRC", "FRRC", "RRFC")
  ## cycle through all representative datasets, FOMs, methods and options
  for (d in 1:length(dataset_arr)) {
    dataset <- dataset_arr[[d]]
    for (i in 1:length(FOM_arr)) {
      for (j in 1:length(method_arr)) {
        # for (k in 1:length(options_arr)) {
        if ((dataset$dataType == "ROC") && (FOM_arr[i] != "Wilcoxon")) {
          
          # for ROC data, only Wilcoxon FOM is allowed
          expect_error(UtilMeanSquares(dataset, FOM = FOM_arr[i], method = method_arr[j]))
        } else if ((dataset$dataType == "FROC") && (FOM_arr[i] == "Wilcoxon")) {
          
          # for FROC data, Wilcoxon FOM is NOT allowed
          expect_error(UtilMeanSquares(dataset, FOM = FOM_arr[i], method = method_arr[j]))
          
        } else {
          
          fn <- paste0(test_path(), "/goodValues361/MeanSquares/", 
                       dataset_arr_str[d], FOM_arr[i],"-", method_arr[j], ".rds")
          if (!file.exists(fn)) {
            warning(paste0("File not found - generating new ",fn))
            ret <- UtilMeanSquares(dataset, FOM = FOM_arr[i], method = method_arr[j])
            saveRDS(ret, file = fn)
          }
          
          ret <- readRDS(fn)
          expect_equal(UtilMeanSquares(dataset, FOM = FOM_arr[i], method = method_arr[j]), ret,
            info = paste0("Dataset = ",dataset_arr_str[[d]],", FOM = ",FOM_arr[i],", method = ",method_arr[j])
          )
          # end of test
          
          #  }
        }  
      }
    }
  }
  
})



