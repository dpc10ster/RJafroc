context("Simulate data sets")

test_that("SimulateCorCbmDataset", {

  fn <- paste0(test_path(), "/goodValues361/SimulateDatasets/SimulateCorCbmDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- SimulateCorCbmDataset()
    saveRDS(ret, file = fn)
  }

  ret <- readRDS(fn)
  expect_equal(SimulateCorCbmDataset(), ret)
  # end of test

})


test_that("SimulateFrocDataset", {
  set.seed(1)
  K1 <- 5;K2 <- 7;
  maxLL <- 2;perCase <- floor(runif(K2, 1, maxLL + 1))
  mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- -1
  I <- 2; J <- 5

  fn <- paste0(test_path(), "/goodValues361/SimulateDatasets/SimulateFrocDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- SimulateFrocDataset(
      mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
      I = I, J = J, K1 = K1, K2 = K2, perCase = perCase)
    saveRDS(ret, file = fn)
  }
  
  set.seed(1)
  K1 <- 5;K2 <- 7;
  maxLL <- 2;perCase <- floor(runif(K2, 1, maxLL + 1))
  mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- -1
  ret <- readRDS(fn)
  expect_equal(SimulateFrocDataset(
    mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
    I = I, J = J, K1 = K1, K2 = K2, perCase = perCase), ret)
  # end of test

})


test_that("SimulateRocDataset", {
  set.seed(1)
  K1 <- 5;K2 <- 7;
  a <- 1.5;b <- 0.5

  fn <- paste0(test_path(), "/goodValues361/SimulateDatasets/SimulateRocDataset", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- SimulateRocDataset(K1 = K1, K2 = K2,a = a, b = b)
    saveRDS(ret, file = fn)
  }
  
  set.seed(1)
  ret <- readRDS(fn)
  expect_equal(SimulateRocDataset(K1 = K1, K2 = K2,a = a, b = b), ret)
  # end of test
  
})

