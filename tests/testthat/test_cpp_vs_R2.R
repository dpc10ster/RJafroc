contextStr <- "Cpp vs R: RSM_wLLF_R for dataset11"
context(contextStr)
test_that(contextStr, {
  
  mu <- 2
  nu <- 0.9
  zeta <- seq(from = -3, to = max(mu)+5, by = 0.2)
  
  ds <- dataset06 # fail Magnus; dataset11 # fail Dobbins
  LD <- UtilLesDistr(ds)
  Freq <- LD$Freq
  lesID <- LD$lesID
  W <- UtilLesWghtsDS(ds)
  
  for (i in 1:length(zeta)) {
    ret1 <- RSM_wLLF_cpp  (zeta[i], mu = mu, nu = nu, f_L = Freq, W = W)
    ret2 <- RSM_wLLF_R(zeta[i], mu = mu, nu = nu, lesDistr = Freq, relWeights = 0)
    testthat::expect_equal(ret1, ret2)
  }
  
})
