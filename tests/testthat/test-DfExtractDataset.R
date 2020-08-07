contextStr <- "DfExtractDataset: toy crossed and split plot datasets"
context(contextStr)
test_that(contextStr, {
  
  # FACTRL
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/frocCr", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- DfExtractDataset(temp, rdrs = c(1, 3))
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- DfExtractDataset(temp, rdrs = c(1, 3))
  expect_equal(x1, x2)

  # SPLIT-PLOT-A
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpA.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/frocSpA", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- DfExtractDataset(temp, rdrs = c(1, 2, 3))
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- DfExtractDataset(temp, rdrs = c(1, 2, 3))
  expect_equal(x1, x2)

  # these need to be updated to reflect changes in frocSpA.xlsx  
  # t <- x1$descriptions$truthTableStr
  # for (j in 1:2) expect_equal(which(!is.na(t[1,j,,1])), 1:5)
  # expect_equal(which(!is.na(t[2,3,,1])), 1:5)
  # for (j in 1:2) expect_equal(which(!is.na(t[1,j,,2])), 6:10)
  # expect_equal(which(!is.na(t[2,3,,2])), 6:10)
  
  # SPLIT-PLOT-C
  # ############################################################################
  fileName <- system.file(
    "extdata", "/toyFiles/FROC/frocSpC.xlsx", package = "RJafroc", mustWork = TRUE)
  temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfExtractDataset/frocSpC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <-  DfExtractDataset(temp, rdrs = c(1, 3))
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <-  DfExtractDataset(temp, rdrs = c(1, 3))
  expect_equal(x1, x2)
  
  t <- x1$descriptions$truthTableStr
  for (i in 1:2) expect_equal(which(!is.na(t[i,1,,1])), 1:3)
  for (i in 1:2) expect_equal(which(!is.na(t[i,2,,1])), 7:9)
  
  for (i in 1:2) expect_equal(which(!is.na(t[i,1,,2])), 10:14)
  for (i in 1:2) expect_equal(which(!is.na(t[i,2,,2])), 20:24)
  
})

