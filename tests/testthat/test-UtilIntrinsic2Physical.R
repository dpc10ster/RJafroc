context("utils:UtilIntrinsic2Physical")
test_that("UtilIntrinsic2Physical", {
  mu <- 2;lambda_i <- 20;nu_i <- 1.1512925
  
  fn <- paste0(test_path(), "/goodValues361/Utils/Intrinsic2PhysicalRSM", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilIntrinsic2RSM(mu, lambda_i, nu_i)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilIntrinsic2RSM(mu, lambda_i, nu_i), ret)
  # end of test
  
})



