test_that("DfReadCrossedModalities", {
  
  crossedFileName <- system.file("extdata",
                                 "CrossedModalitiesData.xlsx",
                                 package = "RJafroc", mustWork = TRUE)
  
  fn <- paste0(test_path(), "/goodValues361/DfReadCrossedModalities/ReadJTXModData", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadCrossedModalities(crossedFileName)
    saveRDS(ds, file = fn)
  }
  
  ds <- readRDS(fn)
  expect_equal(DfReadCrossedModalities(crossedFileName), ds)
  # end of test
  
})
