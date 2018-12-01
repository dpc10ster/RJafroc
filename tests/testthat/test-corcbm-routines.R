context("Corcbm routines")
tmp <- tempfile()
expect_known_output(
  DfCreateCorCbmDataset(), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile()
expect_known_output(
  DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3)), 
  tmp, print = TRUE, update = TRUE)

