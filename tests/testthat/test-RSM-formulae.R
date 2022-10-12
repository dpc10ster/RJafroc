contextStr <- "testing weights code with max 4 lesions per case: Cpp vs R"
context(contextStr)
test_that(contextStr, {
  mu <- 2
  nu <- 0.9
  lambda <- 1
  zeta1 <- -3
  maxLL <- 4
  lesDistr <-  c(0.1, 0.4, 0.4, 0.1) 
  relWeights <- c(0.5, 0.3, 0.1, 0.1)
  
  # see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
  fn <- paste0(test_path(), "/goodValues361/RSMformulae/weights-4-lesions", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAnalyticalAucsRSM(mu = mu, 
                                 lambda = lambda, 
                                 nu = nu, 
                                 zeta1 = zeta1, 
                                 lesDistr = lesDistr, 
                                 relWeights =  relWeights) # this uses Cpp code
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  ret1 <- UtilAnalyticalAucsRSM_R(mu = mu, 
                               lambda = lambda, 
                               nu = nu, 
                               zeta1 = zeta1, 
                               lesDistr = lesDistr, 
                               relWeights =  relWeights)
  expect_equal(ret1, ret)
  
})



contextStr <- "testing weights code with max 4 lesions per case, random values: Cpp vs R"
context(contextStr)
test_that(contextStr, {
  mu <- 2
  nu <- 0.9
  lambda <- 1
  zeta1 <- -3
  maxLL <- 4
  set.seed(100)
  lesDistr <-  runif(maxLL); lesDistr <- lesDistr/sum(lesDistr) 
  relWeights <- runif(maxLL); relWeights <- relWeights/sum(relWeights) 
  
  # see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
  fn <- paste0(test_path(), "/goodValues361/RSMformulae/weights-4-lesions-random", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAnalyticalAucsRSM(mu = mu, 
                                 lambda = lambda, 
                                 nu = nu, 
                                 zeta1 = zeta1, 
                                 lesDistr = lesDistr, 
                                 relWeights =  relWeights) # this uses Cpp code
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  ret1 <- UtilAnalyticalAucsRSM_R(mu = mu, 
                                lambda = lambda, 
                                nu = nu, 
                                zeta1 = zeta1, 
                                lesDistr = lesDistr, 
                                relWeights =  relWeights)
  expect_equal(ret1, ret)
  
})



contextStr <- "testing weights code with max 10 lesions per case, random values: Cpp vs R"
context(contextStr)
test_that(contextStr, {
  mu <- 2
  nu <- 0.9
  lambda <- 1
  zeta1 <- -3
  set.seed(101)
  maxLL <- 10
  lesDistr <-  runif(maxLL); lesDistr <- lesDistr/sum(lesDistr) 
  relWeights <- runif(maxLL); relWeights <- relWeights/sum(relWeights) 
  
  # see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
  fn <- paste0(test_path(), "/goodValues361/RSMformulae/weights-10-lesions-random", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- UtilAnalyticalAucsRSM(mu = mu, 
                                 lambda = lambda, 
                                 nu = nu, 
                                 zeta1 = zeta1, 
                                 lesDistr = lesDistr, 
                                 relWeights =  relWeights) # this uses Cpp code
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  ret1 <- UtilAnalyticalAucsRSM_R(mu = mu, 
                                lambda = lambda, 
                                nu = nu, 
                                zeta1 = zeta1, 
                                lesDistr = lesDistr, 
                                relWeights =  relWeights)
  expect_equal(ret1, ret)
  
})



contextStr <- "RSMformulae1 - implementations of RSM using Maple generated code"
context(contextStr)
test_that(contextStr, {
  
  z <- c(1.000000e+00, 2.330652e+02, 4.290645e-03, 3.829884e+01, 2.611045e-02, 1.379995e+01, 7.246402e-02, 6.737877e+00,
         1.484147e-01, 3.831936e+00, 2.609647e-01, 2.366126e+00, 4.226318e-01, 1.524567e+00, 6.559239e-01)
  
  mu <- 2;lambda <- 1;nu <- 0.4555825;lesDistr <- 1

  fn <- paste0(test_path(), "/goodValues361/RSMformulae/RSM_pdfD1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <-   RSM_pdfD(z, mu, lambda, nu, lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  expect_equal(RSM_pdfD(z, mu, lambda, nu, lesDistr), ret)

})


contextStr <- "RSMformulae2 - implementations of RSM using Maple generated code"
context(contextStr)
test_that(contextStr, {
  
  z <- c(1.000000e+00, 2.330652e+02, 4.290645e-03, 3.829884e+01, 2.611045e-02, 1.379995e+01, 7.246402e-02, 6.737877e+00,
         1.484147e-01, 3.831936e+00, 2.609647e-01, 2.366126e+00, 4.226318e-01, 1.524567e+00, 6.559239e-01)
  
  mu <- 2;lambda <- 1;nu <- 0.4555825;lesDistr <- c(0.1,0.9)

  fn <- paste0(test_path(), "/goodValues361/RSMformulae/RSM_pdfD2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <-   RSM_pdfD(z, mu, lambda, nu, lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  expect_equal(RSM_pdfD(z, mu, lambda, nu, lesDistr), ret)
  
})


contextStr <- "RSMformulae3 - implementations of RSM using Maple generated code"
context(contextStr)
test_that(contextStr, {
  
  z <- c(1.000000e+00, 2.330652e+02, 4.290645e-03, 3.829884e+01, 2.611045e-02, 1.379995e+01, 7.246402e-02, 6.737877e+00,
         1.484147e-01, 3.831936e+00, 2.609647e-01, 2.366126e+00, 4.226318e-01, 1.524567e+00, 6.559239e-01)
  
  lambda <- 1
  
  fn <- paste0(test_path(), "/goodValues361/RSMformulae/RSM_pdfN", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <-   RSM_pdfN(z, lambda)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  expect_equal(RSM_pdfN(z, lambda), ret)
  
})
