context("UtilAucBIN")
test_that("UtilAucBIN", {
  a <- 2;b <- 0.7
  
  fn <- paste0(test_path(), "/goodValues361/Utils/AucBinormal", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAucBIN(a,b)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilAucBIN(a,b), ret)
  # end of test
  
})




context("utils:UtilUtilAucCBM")
test_that("UtilAucCBM", {
  mu <- 2;alpha <- 0.8
  
  fn <- paste0(test_path(), "/goodValues361/Utils/AucCbm", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAucCBM(mu,alpha)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilAucCBM(mu,alpha), ret)
  # end of test
  
})



context("utils:UtilAucPROPROC")
test_that("UtilAucPROPROC", {
  c1 <- .2;da <- 1.5
  
  fn <- paste0(test_path(), "/goodValues361/Utils/AucPROPROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAucPROPROC(c1,da)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilAucPROPROC(c1,da), ret)
  # end of test
  
})



context("utils:UtilAnalyticalAucsRSM1")
test_that("UtilAnalyticalAucsRSM zeta1 neg Inf", {
  mu <- 1;lambda_i <- 1;nu_i <- 1
  x <- Util2Physical(mu, lambda_i, nu_i)
  lambda <- x$lambda
  nu <- x$nu
  lesDistr <- c(0.9, 0.1)
  
  fn <- paste0(test_path(), "/goodValues361/Utils/AucRSM1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = -Inf, lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = -Inf, lesDistr), ret)
  # end of test
  
})



context("utils:UtilAnalyticalAucsRSM2")
test_that("UtilAnalyticalAucsRSM zeta1 0", {
  mu <- 1;lambda_i <- 1;nu_i <- 1
  mu <- 1;lambda_i <- 1;nu_i <- 1
  x <- Util2Physical(mu, lambda_i, nu_i)
  lambda <- x$lambda
  nu <- x$nu
  lesDistr <- c(0.9, 0.1) 
  
  fn <- paste0(test_path(), "/goodValues361/Utils/AucRSM2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = 0, lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = 0, lesDistr), ret)
  # end of test
  
})




context("utils:UtilAnalyticalAucsRSMWeights")
test_that("UtilAnalyticalAucsRSM", {
  mu <- 1;lambda_i <- 1;nu_i <- 1
  mu <- 1;lambda_i <- 1;nu_i <- 1
  x <- Util2Physical(mu, lambda_i, nu_i)
  lambda <- x$lambda
  nu <- x$nu
  lesDistr <- c(0.9, 0.1) 
  relWeights <- c(0.05, 0.95)
  
  fn <- paste0(test_path(), "/goodValues361/Utils/AucRSMWeights", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = -Inf, lesDistr, relWeights)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = -Inf, lesDistr, relWeights), ret)
  # end of test
  
})



