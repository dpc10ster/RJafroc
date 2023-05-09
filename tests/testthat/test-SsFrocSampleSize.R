contextStr <- "SsFrocSampleSize Sample Size FROC"
context(contextStr)
test_that(contextStr, {
 
  # skip_on_os("windows")
  skip_on_os("linux")
  skip_on_os("solaris")

  fn <- paste0(test_path(), "/goodValues361/SsPower/SsFrocSampleSize-dataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- SsFrocSampleSize(DfExtractDataset(dataset04, trts = c(1,2)), 
             effectSizeROC = 0.03, JPivot = 5, KPivot = 100, 
             lesDistr = c(0.69, 0.2, 0.11))
    saveRDS(ret, file = fn)
  }

  x1 <- readRDS(fn)
  x2 <- SsFrocSampleSize(DfExtractDataset(dataset04, trts = c(1,2)), 
        effectSizeROC = 0.03, JPivot = 5, KPivot = 100, 
        lesDistr = c(0.69, 0.2, 0.11))
  
  # expect_equal(x1,x2) 
  # this can fail on other operating systems
  # see Issue #70
  # as per Peter's suggestions
  expect_identical(names(x1), names(x2))
  expect_equivalent(x1$effectSizeROC, x2$effectSizeROC, tolerance=5e-3)
  expect_equivalent(x1$scaleFactor, x2$scaleFactor, tolerance=5e-3)
  expect_equivalent(x1$powerRoc, x2$powerRoc, tolerance=5e-3)
  expect_equivalent(x1$powerFroc, x2$powerFroc, tolerance=5e-3)

})



contextStr <- "SsFrocNhRsmModel Sample Size FROC"
context(contextStr)
test_that(contextStr, {
  
  # skip_on_os("windows")
  skip_on_os("linux")
  skip_on_os("solaris")
  
  fn <- paste0(test_path(), "/goodValues361/SsPower/SsFrocNhRsmModel-dataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- SsFrocNhRsmModel(DfExtractDataset(dataset04, trts = c(1,2)), 
                            lesDistr = c(0.69, 0.2, 0.11))
    saveRDS(ret, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <- SsFrocNhRsmModel(DfExtractDataset(dataset04, trts = c(1,2)), 
                         lesDistr = c(0.69, 0.2, 0.11))
  
  # expect_equal(x1,x2) 
  # this used to fail on other operating systems using SsFrocNhRsmModel()
  # see Issue #70
  # as per Peter's suggestions
  expect_identical(names(x1), names(x2))
  expect_equivalent(x1$mu, x2$mu, tolerance=5e-3)
  expect_equivalent(x1$lambda, x2$lambda, tolerance=5e-3)
  expect_equivalent(x1$nu, x2$nu, tolerance=5e-3)
  expect_equivalent(x1$ScaleFactor, x2$ScaleFactor, tolerance=5e-3)
  expect_equivalent(x1$R2, x2$R2, tolerance=1e-3)

  
})

