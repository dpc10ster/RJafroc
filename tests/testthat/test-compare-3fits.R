context("Binning routines")
tmp <- tempfile()
expect_known_output(
  Compare3ProperRocFits(1,2,reAnalyze = TRUE), 
  tmp, print = TRUE, update = TRUE)

