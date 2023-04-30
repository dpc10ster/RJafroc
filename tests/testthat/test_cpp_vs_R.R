# this is correct but does not match cpp implentation
yROC_R <- function(zeta, mu, lambda, nu, lesDistr, lesID) {
  
  maxLL <-  max(lesID) # NOT length(lesDistr)
  
  x <-  0
  for (L in 1:length(lesID)){
    x = x + lesDistr[L] * (1 - nu * pnorm(mu - zeta))^lesID[L]
  }
  
  TPF <- 1 - exp(-lambda*pnorm(-zeta)) * x 
  
  return (TPF)
}


# this is incorrect but matches cpp implentation
yROC_R2 <- function(zeta, mu, lambda, nu, lesDistr, lesID) {
  
  maxLL <-  max(lesID) # NOT length(lesDistr)
  
  x <-  0
  for (L in 1:length(lesID)){
    x = x + lesDistr[L] * (1 - nu * pnorm(mu - zeta))^L
  }
  
  TPF <- 1 - exp(-lambda*pnorm(-zeta)) * x 
  
  return (TPF)
}


# W <-UtilLesWghtsLD(lesDistr, relWeights)
# wLLF <- 0
# for (L in 1:L_max){
#   wLLF_L <- 0
#   for (l_2 in 1:L){
#     # W has an extra column that must be skipped, hence W[L, l_2+1]
#     wLLF_L <- wLLF_L + W[L, l_2+1] * l_2 * dbinom(l_2, L, nu)
#   }
#   wLLF <- wLLF +  f_L[L] * wLLF_L
# }
# wLLF <- wLLF * pnorm(mu - zeta)



contextStr <- "Cpp vs R: yROC for dataset11"
context(contextStr)
test_that(contextStr, {
  
  mu <- 2
  nu <- 0.9
  lambda <- 1
  zeta <- seq(from = -3, to = max(mu)+5, by = 0.2)

  ds <- dataset11
  lesDistr <- UtilLesDistr(ds)$Freq
  lesID <- UtilLesDistr(ds)$lesID
  
  for (i in 1:length(zeta)) {
    ret1 <- yROC  (zeta[i], mu = mu, lambda = lambda, nu = nu, lesDistr = lesDistr)
    ret2 <- yROC_R2(zeta[i], mu = mu, lambda = lambda, nu = nu, lesDistr = lesDistr, lesID = lesID)
    testthat::expect_equal(ret1, ret2)
  }
  
})


