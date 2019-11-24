context("The failed checks apply to the Truth Sheet only")
test_that("DfReadDataFile and toy datasets", {
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/OK.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/ROC_OK", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/SplitPlot.xlsx", package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadDataFile/ROC_SplitPlot", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
    saveRDS(temp, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadDataFile(fileName, newExcelFileFormat = TRUE), ds)
  ##expect_error(DfReadDataFile(fileName, newExcelFileFormat = FALSE))
  # instructive comment
  # reading a split plot ROC dataset with newExcelFileFormat = FALSE 
  # incorrectly yields an FROC dataset
  # dsx <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  # dsx <- dsx[-6] # delete the data type FROC
  # ds <- ds[-6]# delete the data type ROC
  # expect_equal(dsx, ds)
  
  ## The failed checks apply to the Truth Sheet only
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missing2CellsRow5and7.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missingEntireRow5.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
  
  fileName <- system.file(
    "extdata", "/toyFiles/ROC/missing1CellRow5.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-integerCaseID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-integerLesionID.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/incorrectWeights.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))

  fileName <- system.file(
    "extdata", "/toyFiles/ROC/non-numeric-weight.xlsx", package = "RJafroc", mustWork = TRUE)
  expect_warning(expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE)))

 fileName <- system.file(
   "extdata", "/toyFiles/ROC/duplicatedRow.xlsx", package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
 fileName <- system.file(
   "extdata", "/toyFiles/ROC/incorrectSheetName.xlsx", package = "RJafroc", mustWork = TRUE)
 expect_error(DfReadDataFile(fileName, newExcelFileFormat = TRUE))
 
 # fileName <- system.file(
  #   "extdata", "/toyFiles/FROC/OK.xlsx", package = "RJafroc", mustWork = TRUE)
  # dsOld <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  # dsNew <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
  # expect_equal(dsOld, dsNew)
  # expect_error(DfReadDataFile(fileName, newExcelFileFormat = FALSE, splitPlot = TRUE))
  
})

