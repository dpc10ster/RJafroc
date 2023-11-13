library(RJafroc)
library(testthat)

dataset <- dataset05
# "Wilcoxon" will generate error, skipping "SongA1" and "SongA2"
FOM_arr <- c("AFROC", "AFROC1", "wAFROC", "wAFROC1", "MaxNLF", "MaxLLF", "MaxNLFAllCases", "HrSp", "HrSe")

for (i in 1:length(FOM_arr)) {
  cat("i = ", i, ", FOM_arr[i] = ", FOM_arr[i], "\n")
  
  fn <- paste0(test_path(), "/goodValues361/Utils/PseudoValues", "-", FOM_arr[i], ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilPseudoValues(dataset, FOM = FOM_arr[i])
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn); ret1$caseTransitions <- NULL
  ret2 <- UtilPseudoValues(dataset, FOM = FOM_arr[i])
  expect_equal(ret1, ret2, info = paste0("FOM = ", FOM_arr[i]))
  
}

