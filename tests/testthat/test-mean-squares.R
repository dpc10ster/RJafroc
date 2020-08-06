contextStr <- "UtilMeanSquares"
context(contextStr)
test_that(contextStr, {

  dataset_arr <- c("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc", "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBM", "OR")
  # options_arr <- c("RRRC", "FRRC", "RRFC")
  ## cycle through all representative datasets, FOMs, methods and analysisOption
  for (d in 1:length(dataset_arr)) {
    dataset <- get(dataset_arr[d])
    for (f in 1:length(FOM_arr)) {
      for (m in 1:length(method_arr)) {
        # for (k in 1:length(options_arr)) {
        if ((dataset$descriptions$type == "ROC") && (FOM_arr[f] != "Wilcoxon")) {
          
          # for ROC data, only Wilcoxon FOM is allowed
          expect_error(UtilMeanSquares(dataset, FOM = FOM_arr[f], method = method_arr[m]),
                       info = paste0("Dataset = ",dataset_arr[d],", FOM = ",FOM_arr[f],", method = ",method_arr[m]))
        } else if ((dataset$descriptions$type == "FROC") && (FOM_arr[f] == "Wilcoxon")) {
          
          # for FROC data, Wilcoxon FOM is NOT allowed
          expect_error(UtilMeanSquares(dataset, FOM = FOM_arr[f], method = method_arr[m]),
                       info = paste0("Dataset = ",dataset_arr[d],", FOM = ",FOM_arr[f],", method = ",method_arr[m]))
          
        } else {
          
          fn <- paste0(test_path(), "/goodValues361/MeanSquares/", 
                       dataset_arr[d], FOM_arr[f],"-", method_arr[m], ".rds")
          if (!file.exists(fn)) {
            warning(paste0("File not found - generating new ",fn))
            x1 <- UtilMeanSquares(dataset, FOM = FOM_arr[f], method = method_arr[m])
            saveRDS(x1, file = fn)
          }
          
          x1 <- readRDS(fn)
          x2 <- UtilMeanSquares(dataset, FOM = FOM_arr[f], method = method_arr[m])
          expect_equal(x1, x2,
            info = paste0("Dataset = ",dataset_arr[d],", FOM = ",FOM_arr[f],", method = ",method_arr[m])
          )
        }  
      }
    }
  }
  
})
