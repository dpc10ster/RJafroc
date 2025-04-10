# contextStr <- "StSP_A"
# context(contextStr)
# test_that(contextStr, {
#   
#   fileName <- system.file("extdata", "/toyFiles/ROC/rocSpA.xlsx", 
#                           package = "RJafroc", mustWork = TRUE)
#   dsSpA <- DfReadSP_A(fileName)
#   
#   fn <- paste0(test_path(), "/goodValues361/StSP/StSP_A", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     target <- StSP(dsSpA, FOM = "Wilcoxon")
#     saveRDS(target, file = fn)
#   }
#   
#   target <- readRDS(fn)
#   expect_equal(StSP(dsSpA, FOM = "Wilcoxon"), target)
# 
#   ret <- StSP(datasetFROCSpC, FOM = "wAFROC")
#   
# })
# 
# 
# contextStr <- "StSP_C"
# context(contextStr)
# test_that(contextStr, {
#   
#   fn <- paste0(test_path(), "/goodValues361/StSP/StSP_C", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     target <- StSP(datasetFROCSpC, FOM = "wAFROC")
#     saveRDS(target, file = fn)
#   }
#   
#   target <- readRDS(fn)
#   expect_equal(StSP(datasetFROCSpC, FOM = "wAFROC"), target)
#   
# })
# 
