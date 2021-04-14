contextStr <- "Sample Size FROC"
context(contextStr)
test_that(contextStr, {
  
 
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
  expect_equal(x1,x2)

})




