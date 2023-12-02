context("DfBinDataset ROC, AFROC, etc.")
test_that("DfBinDataset (ROC, AFROC, etc.)", {
  Bins <- c(2, 4, 6)
  dataset <- c("dataset02", "dataset03", "dataset09")
  type <- c("ROC")
  for (b in 1:length(Bins)) {
    for (d in 1:length(dataset)) {
      for (t in 1:length(type)) {
        fn <- paste0(test_path(), "/goodValues361/DfBinDataset/", dataset[d], type[t], "-", Bins[b], ".rds")
        if (!file.exists(fn)) {
          warning(paste0("File not found - generating new ",fn))
          ds <- DfBinDataset(get(dataset[d]), desiredNumBins = Bins[b], opChType = type[t])
          saveRDS(ds, file = fn)
        }
        ds <- readRDS(fn)
        expect_equal(DfBinDataset(get(dataset[d]), desiredNumBins = Bins[b], opChType = type[t]), ds)
      }
    }
  }
  
  expect_error(DfBinDataset(dataset02, desiredNumBins = 1, opChType = "ROC"))
  expect_error(DfBinDataset(dataset04, desiredNumBins = 1, opChType = "ROC"))
  
  Bins <- c(1,2,4,5,6)
  dataset <- c("dataset01", "dataset04", "dataset05")
  type <- c("ROC", "FROC", "AFROC", "wAFROC")
  for (b in 1:length(Bins)) {
    for (d in 1:length(dataset)) {
      for (t in 1:length(type)) {
        if ((type[t] == "ROC") && (Bins[b] == 1)) next
        fn <- paste0(test_path(), "/goodValues361/DfBinDataset/", dataset[d], type[t], "-", Bins[b], ".rds")
        if (!file.exists(fn)) {
          warning(paste0("File not found - generating new ",fn))
          ds <- DfBinDataset(get(dataset[d]), desiredNumBins = Bins[b], opChType = type[t])
          saveRDS(ds, file = fn)
        }
        ds <- readRDS(fn)
        ds1 <- DfBinDataset(get(dataset[d]), desiredNumBins = Bins[b], opChType = type[t])
        expect_equal(ds$ratings,ds1$ratings)
        expect_equal(ds$lesions,ds1$lesions)
        expect_equal(ds,ds1)
      }
    }
  }
  
})





