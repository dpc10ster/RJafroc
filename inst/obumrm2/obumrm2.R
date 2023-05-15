library(RJafroc)


ret <- UtilPseudoValues(RJafroc::dataset02, FOM = "Wilcoxon")

I <- dim(ret$jkFomValues)[1]
J <- dim(ret$jkFomValues)[2]
K <- dim(ret$jkFomValues)[3]

COV <- array(dim = c(I, I, J, J))

for (i in 1:I) {
  for (ip in 1:I) {
    for (j in 1:J) {
      for (jp in 1:J) {
        COV[i, ip, j, jp] <- cov(ret$jkFomValues[i, j, ], ret$jkFomValues[ip, jp, ])
      }
    }
  }
}

COV <- COV * (K - 1)^2/K  # see paper by Efron and Stein 




