contextStr <- "Sample Size FROC"
context(contextStr)
test_that(contextStr, {
 
  # skip_on_os("windows") 
  skip_on_os("linux") 
  skip_on_os("solaris") 
  
  lesDistr <- c(0.7, 0.2, 0.1)
  frocNhData <- DfExtractDataset(dataset04, trts = c(1,2))
 
  fn <- paste0(test_path(), "/goodValues361/SsPower/FROC-dataset04", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- SsFrocNhRsmModel(frocNhData, lesDistr = lesDistr)
    saveRDS(ret, file = fn)
  }
  
  x1 <- readRDS(fn)
  x2 <-SsFrocNhRsmModel(frocNhData, lesDistr = lesDistr)
  ## expect_equal(x1,x2) # this fails on other operating systems
  # see Issue #70
  # as per Peter's suggestions
  expect_identical(names(x1), names(x2))
  expect_equivalent(x1$muMed, x2$muMed, tolerance=5e-4)
  expect_equivalent(x1$lambdaMed, x2$lambdaMed, tolerance=5e-3)
  expect_equivalent(x1$nuMed, x2$nuMed, tolerance=5e-4)
  expect_equivalent(x1$ScaleFactor, x2$ScaleFactor, tolerance=5e-5)
  expect_equivalent(x1$R2, x2$R2, tolerance=1e-7)

    
})




