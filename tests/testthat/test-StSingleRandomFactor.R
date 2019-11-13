context("StSignificanceTesting routines: single factor")


test_that("StSingleTreatmentRandomReader", {
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/SingleRandomFactor_02_1_14", ".rds") # fn is abbreviation for dataset02, 1, 1:4
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- StSingleTreatmentRandomReader(DfExtractDataset(dataset02, 1, 1:4), fomNh = 0.8,
                                                  FOM = "Wilcoxon")
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- StSingleTreatmentRandomReader(DfExtractDataset(dataset02, 1, 1:4), fomNh = 0.8,
                                                FOM = "Wilcoxon")
  
  expect_equal(x1, x2)
  
  ds <- DfExtractDataset(datasetCadLroc, rdrs = seq(2,10))
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/SingleRandomFactorLrocDataset", ".rds") # fn is abbreviation for dataset02, 1, 1:4
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    x1 <- StSingleTreatmentRandomReader(ds, fomNh = 0.8, FOM = "ALROC", FPFValue = 1)
    saveRDS(x1, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- StSingleTreatmentRandomReader(ds, fomNh = 0.8, FOM = "ALROC", FPFValue = 1)
  
  expect_equal(x1, x2)
  
})

