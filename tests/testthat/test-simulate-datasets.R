context("Simulate data sets")

test_that("SimulateCorCbmDataset", {
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SimulateCorCbmDataset(), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  expect_known_output(
    SimulateCorCbmDataset(), 
    tmp, print = TRUE, update = TRUE)
  
})

test_that("SimulateFrocDataset", {
  set.seed(1) 
  K1 <- 5;K2 <- 7;
  maxLL <- 2;lesionNum <- floor(runif(K2, 1, maxLL + 1))
  mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- -1
  I <- 2; J <- 5
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SimulateFrocDataset(
      mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
      I = I, J = J, K1 = K1, K2 = K2, lesionNum = lesionNum), 
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")
  
  set.seed(1) 
  K1 <- 5;K2 <- 7;
  maxLL <- 2;lesionNum <- floor(runif(K2, 1, maxLL + 1))
  mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- -1
  I <- 2; J <- 5
  expect_known_output(
    SimulateFrocDataset(
      mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
      I = I, J = J, K1 = K1, K2 = K2, lesionNum = lesionNum), 
    tmp, print = TRUE, update = TRUE)
  
  
})

test_that("SimulateRocDataset", {
  set.seed(1)
  K1 <- 5;K2 <- 7;
  a <- 1.5;b <- 0.5
  
  tmp <- tempfile()
  expect_warning(expect_known_output(
    SimulateRocDataset(K1 = K1, K2 = K2,a = a, b = b),
    tmp, print = TRUE, update = TRUE),
    "Creating reference output")

  set.seed(1)
  K1 <- 5;K2 <- 7;
  a <- 1.5;b <- 0.5
  expect_known_output(
    SimulateRocDataset(K1 = K1, K2 = K2,a = a, b = b),
    tmp, print = TRUE, update = TRUE)

})

