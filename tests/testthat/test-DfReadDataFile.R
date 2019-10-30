
test_that("DfReadDataFile and toy datasets", {
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/OK.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/OK.xlsx", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missing2CellsRow5and7.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missing1CellRow5.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-integerCaseID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-integerLesionID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/incorrectWeights.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/duplicatedRow.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-numeric-weight.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_warning(expect_error(DfReadDataFile(fileName)))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/incorrectSheetName.xlsx", package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName))
  
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/OK.xlsx", package = "RJafroc", mustWork = TRUE)
  dsOld <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  dsNew <- DfReadDataFile(fileName)
  expect_equal(dsOld, dsNew)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = FALSE, splitPlot = TRUE))
  
})

