contextStr <- "RSMformulae1 - implementations of RSM using Maple generated code"
context(contextStr)
test_that(contextStr, {
  
  z <- c(1.000000e+00, 2.330652e+02, 4.290645e-03, 3.829884e+01, 2.611045e-02, 1.379995e+01, 7.246402e-02, 6.737877e+00,
         1.484147e-01, 3.831936e+00, 2.609647e-01, 2.366126e+00, 4.226318e-01, 1.524567e+00, 6.559239e-01)
  
  mu <- 2;lambdaP <- 1;nuP <- 0.4555825;lesDistr <- 1
  
  fn <- paste0(test_path(), "/goodValues361/RSMformulae/RSM_pdfD1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <-   RSM_pdfD(z, mu, lambdaP, nuP, lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  expect_equal(RSM_pdfD(z, mu, lambdaP, nuP, lesDistr), ret)

})


contextStr <- "RSMformulae2 - implementations of RSM using Maple generated code"
context(contextStr)
test_that(contextStr, {
  
  z <- c(1.000000e+00, 2.330652e+02, 4.290645e-03, 3.829884e+01, 2.611045e-02, 1.379995e+01, 7.246402e-02, 6.737877e+00,
         1.484147e-01, 3.831936e+00, 2.609647e-01, 2.366126e+00, 4.226318e-01, 1.524567e+00, 6.559239e-01)
  
  mu <- 2;lambdaP <- 1;nuP <- 0.4555825;lesDistr <- c(0.1,0.9)
  
  fn <- paste0(test_path(), "/goodValues361/RSMformulae/RSM_pdfD2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <-   RSM_pdfD(z, mu, lambdaP, nuP, lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  expect_equal(RSM_pdfD(z, mu, lambdaP, nuP, lesDistr), ret)
  
})


contextStr <- "RSMformulae3 - implementations of RSM using Maple generated code"
context(contextStr)
test_that(contextStr, {
  
  z <- c(1.000000e+00, 2.330652e+02, 4.290645e-03, 3.829884e+01, 2.611045e-02, 1.379995e+01, 7.246402e-02, 6.737877e+00,
         1.484147e-01, 3.831936e+00, 2.609647e-01, 2.366126e+00, 4.226318e-01, 1.524567e+00, 6.559239e-01)
  
  lambdaP <- 1
  
  fn <- paste0(test_path(), "/goodValues361/RSMformulae/RSM_pdfN", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <-   RSM_pdfN(z, lambdaP)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  
  expect_equal(RSM_pdfN(z, lambdaP), ret)
  
})
