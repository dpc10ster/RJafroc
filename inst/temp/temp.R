library(RJafroc)
# does not work 4/30/23
mu <- 2
nu <- 0.9
lambda <- 1
zeta <- seq(from = -3, to = max(mu)+5, by = 0.2)
maxLL <- 4
lesDistr <-  c(0.1, 0.4, 0.4, 0.1) 
relWeights <- c(0.5, 0.3, 0.1, 0.1)

for (i in zeta) {
  ret1 <- yROC_cpp  (zeta[i], mu = mu, lambda = lambda, nu = nu, lesDistr = lesDistr)
  ret2 <- yROC_R(zeta[i], mu = mu, lambda = lambda, nu = nu, lesDistr = lesDistr)
  expect_equal(ret1, ret)
}



