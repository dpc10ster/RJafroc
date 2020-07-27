# contextStr <- "StSignificanceTesting-toy dataset frocSpC wAFROC"
# context(contextStr)
# test_that(contextStr, {
# 
#   fileName <- system.file(
#     "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues361/SigTest/frocSpC-wAFROC", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     x1 <- StSignificanceTesting(temp, FOM = "wAFROC", method = "OR")
#     saveRDS(x1, file = fn)
#   }
#   
#   x1 <- readRDS(fn)
#   x2 <- StSignificanceTesting(temp, FOM = "wAFROC", method = "OR")
#   
#   expect_equal(x1,x2)
#   
# })




contextStr <- "StSignificanceTesting-toy file frocSpA-wAFROC"
context(contextStr)
test_that(contextStr, {
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/frocSpA-wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- StSignificanceTesting(temp, FOM = "wAFROC", method = "OR")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- StSignificanceTesting(temp, FOM = "wAFROC", method = "OR")
  
  expect_equal(x1,x2)

})



# 
# contextStr <- "StSignificanceTesting real dataset FROC SpA-wAFROC"
# context(contextStr)
# test_that(contextStr, {
#   
#   fileName <- system.file(
#     "extdata", "/toyFiles/FROC/1T3Rvs4R.xlsx", package = "RJafroc", mustWork = TRUE)
#   
#   datasetFROCSpA <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
#   
#   fn <- paste0(test_path(), "/goodValues361/SigTest/1T3Rvs4R-SpA-wAFROC", ".rds")
#   if (!file.exists(fn)) {
#     warning(paste0("File not found - generating new ",fn))
#     x1 <- StSignificanceTesting(datasetFROCSpA, FOM = "wAFROC", method = "OR")
#     saveRDS(x1, file = fn)
#   }
#   
#   x1 <- readRDS(fn)
#   x2 <- StSignificanceTesting(datasetFROCSpA, FOM = "wAFROC", method = "OR")
#   
#   expect_equal(x1,x2)
#   
#   expect_error(StSignificanceTesting(datasetFROCSpA, FOM = "wAFROC", method = "DBM"))
#   expect_error(StSignificanceTesting(datasetFROCSpA, FOM = "wAFROC", method = "OR", covEstMethod = "bootstrap"))
#   expect_error(StSignificanceTesting(datasetFROCSpA, FOM = "wAFROC", method = "OR", covEstMethod = "DeLong"))
#   
# })
# 
