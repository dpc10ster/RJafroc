context("utils:UtilMeanSquaresDBM")
test_that("UtilMeanSquaresDBM", {
  
  fn <- paste0(test_path(), "/goodValues361/Utils/PseudoValues", "-", "Wilcoxon", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilPseudoValues(dataset02, FOM = "Wilcoxon")
    saveRDS(ret, file = fn)
  }
  
  ret1 <- readRDS(fn);ret1$caseTransitions <- NULL
  ret2 <- UtilPseudoValues(dataset02, FOM = "Wilcoxon")
  expect_equal(ret2, ret1)
  # end of test
  
})




