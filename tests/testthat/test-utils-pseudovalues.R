context("utils:UtilPseudoValues")
test_that("UtilPseudoValues", {
  
  dataset <- dataset05
  # "Wilcoxon" will generate error, skipping "SongA1" and "SongA2"
  FOM_arr <- c("AFROC", "AFROC1", "wAFROC", "wAFROC1", "MaxNLF", "MaxLLF", "MaxNLFAllCases",
               "ExpTrnsfmSp", "HrSp", "HrSe")
  
  for (i in 1:length(FOM_arr)) {
    
    fn <- paste0(test_path(), "/goodValues361/Utils/PseudoValues", "-", FOM_arr[i], ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      ret <- UtilPseudoValues(dataset, FOM = FOM_arr[i])
      saveRDS(ret, file = fn)
    }
    
    ret <- readRDS(fn)
    expect_equal(UtilPseudoValues(dataset, FOM = FOM_arr[i]), ret, 
                 info = paste0("FOM = ", FOM_arr[i]))

  }
  
})


