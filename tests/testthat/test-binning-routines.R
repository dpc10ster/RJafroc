context("Binning routines")
tmp <- tempfile()
expect_known_output(
  DfBinDataset(dataset05, opChType = "ROC"), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile()
expect_known_output(
  DfBinDataset(dataset05, opChType = "AFROC"), 
  tmp, print = TRUE, update = TRUE)
