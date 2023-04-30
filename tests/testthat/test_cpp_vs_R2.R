contextStr <- "Cpp vs R: RSM_wLLF_R for dataset11"
context(contextStr)
test_that(contextStr, {
  
  mu <- 2
  nu <- 0.9
  lambda <- 1
  zeta <- seq(from = -3, to = max(mu)+5, by = 0.2)

  ds <- dataset05
  lesDistr <- UtilLesDistr(ds)$Freq
  lesID <- UtilLesDistr(ds)$lesID
  W <- UtilLesWghtsDS(ds)
  
  for (i in 1:length(zeta)) {
  #for (i in 1:1) {
      ret1 <- RSM_wLLF  (zeta[i], mu = mu, nu = nu, f_L = lesDistr, W = W)
    ret2 <- RSM_wLLF_R(zeta[i], mu = mu, nu = nu, lesDistr = lesDistr, relWeights = 0)
    testthat::expect_equal(ret1, ret2)
  }
  
})
