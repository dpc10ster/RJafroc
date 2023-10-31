# context("StSignificanceTestingSingleFixedFactor")
# test_that("StSignificanceTestingSingleFixedFactor", {
# 
#   fn <- paste0(test_path(), "/goodValues361/SigTest/SingleFixedFactor_02_1_14", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     x1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon")
#     saveRDS(x1, file = fn)
#   }
# 
#   x1 <- readRDS(fn)
#   x2 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset02, 1, 1:4), FOM = "Wilcoxon")
# 
#   # CompareLists(x1,x2)
#   expect_equal(x1,x2)
# 
# })
# 
# 
# 
# 
# context("StSignificanceTestingSingleFixedFactor")
# test_that("StSignificanceTestingSingleFixedFactor", {
# 
#   skip_on_os("windows")
# 
#   fn <- paste0(test_path(), "/goodValues361/SigTest/SingleFixedFactor_05_1_14", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     x1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4), FOM = "wAFROC")
#     saveRDS(x1, file = fn)
#   }
# 
#   x1 <- readRDS(fn)
#   x2 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1, 1:4), FOM = "wAFROC")
# 
#   # CompareLists(x1,x2)
#   expect_equal(x1,x2)
# 
# })
# 
# 
# 
# test_that("StSignificanceTestingSingleFixedFactor", {
# 
#   fn <- paste0(test_path(), "/goodValues361/SigTest/SingleFixedFactor_05_12_4", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     x1 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4), FOM = "wAFROC")
#     saveRDS(x1, file = fn)
#   }
# 
#   x1 <- readRDS(fn)
#   x2 <- StSignificanceTestingSingleFixedFactor(DfExtractDataset(dataset05, 1:2, 4), FOM = "wAFROC")
# 
#   # CompareLists(x1,x2)
#   expect_equal(x1,x2)
# 
# })
