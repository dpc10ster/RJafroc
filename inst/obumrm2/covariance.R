covariance <- function(ds, FOM) 
{
  
  ret <- UtilPseudoValues(ds, FOM)
  
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
  
  # chk matrix is symmetric, no output if so
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          if((ip != i) && (jp != j)) {
            if (COV[i, ip, j, jp] != COV[ip, i, jp, j]) cat("i, ip, j, jp = ", i, ip, j, jp, "\n")
          }
        }
      }
    }
  }
  
  return(COV)
}
