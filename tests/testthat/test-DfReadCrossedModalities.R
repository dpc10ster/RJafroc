contextStr <- "DfReadXModalities"
context(contextStr)
test_that(contextStr, {
  crossedFileName <- system.file("extdata",
                                 "XModDataFile.xlsx",
                                 package = "RJafroc", mustWork = TRUE)

  fn <- paste0(test_path(), "/goodValues361/DfXMod/DfReadXModalities", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ds <- DfReadXModalities(crossedFileName)
    saveRDS(ds, file = fn)
  }

  ds <- readRDS(fn)
  expect_equal(DfReadXModalities(crossedFileName), ds)

})
