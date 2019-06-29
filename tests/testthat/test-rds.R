test_that("Check RDS", {
  
  skip_on_cran()
  skip_on_travis()
  # generate the ratings  
  # a small ROC dataset
  set.seed(1)
  z1 <- rnorm(5)
  z2 <- rnorm(7)*1.5 + 2
  
  fn <- paste0(test_path(), "/goodValues/Df2RJafrocDatasetds01.rds")
  if (!file.exists(fn)) {
    ds <- Df2RJafrocDataset(z1, z2)
    saveRDS(ds, file = fn)
  }
  
  readRDS(fn) # ds is not restored on second Run Tests; works on first only
  expect_equal(Df2RJafrocDataset(z1, z2), ds) 
  # end of test
  
})
