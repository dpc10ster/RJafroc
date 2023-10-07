FOM2VarCov <- function(resampleFOM, varInflFactor, flag) {
  
  if ((length(dim(resampleFOM)) == 3) && (flag == "IJ")) {
    
    ret <- FOMijk2VC(resampleFOM, varInflFactor)
    
  } else if ((length(dim(resampleFOM)) == 2) && (flag == "I")) {
    
    ret <- FOMik2VC(resampleFOM, varInflFactor)
    
  } else if ((length(dim(resampleFOM)) == 2) && (flag == "J")) {
    
    ret <- FOMjk2VC(resampleFOM, varInflFactor)
    
  }   
  
  return(ret)
  
}



FOMijk2VC <- function(resampleFOMijk, varInflFactor) {
  
  I <- dim(resampleFOMijk)[1]
  J <- dim(resampleFOMijk)[2]
  K <- dim(resampleFOMijk)[3]
  
  covariances <- array(dim = c(I, I, J, J))
  
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          covariances[i, ip, j, jp] <- cov(resampleFOMijk[i, j, ], resampleFOMijk[ip, jp, ])
        }
      }
    }
  }
  
  if (varInflFactor)  {
    covariances <- covariances * (K - 1)^2/K  # see paper by Efron and Stein 
  }
  
  Var <- 0
  count <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      Var <- Var + covariances[i, i, j, j]
      count <- count + 1
    }
  }
  if (count > 0) Var <- Var/count else Var <- 0
  
  Cov1 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        if (ip != i) {
          Cov1 <- Cov1 + covariances[i, ip, j, j]
          count <- count + 1
        }
      }
    }
  }
  if (count > 0) Cov1 <- Cov1/count else Cov1 <- 0
  
  Cov2 <- 0
  count <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (jp in 1:J) {
        if (j != jp) {
          Cov2 <- Cov2 + covariances[i, i, j, jp]
          count <- count + 1
        }
      }
    }
  }
  if (count > 0) Cov2 <- Cov2/count else Cov2 <- 0
  
  Cov3 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      if (i != ip) {
        for (j in 1:J) {
          for (jp in 1:J) {
            if (j != jp) {
              Cov3 <- Cov3 + covariances[i, ip, j, jp]
              count <- count + 1
            }
          }
        }
      }
    }
  }
  if (count > 0) Cov3 <- Cov3/count else Cov3 <- 0
  
  return(list(
    Var = Var,
    Cov1 = Cov1,
    Cov2 = Cov2,
    Cov3 = Cov3
  ))
  
}


FOMik2VC <- function(FOMik, varInflFactor) {
  
  I <- dim(FOMik)[1]
  K <- dim(FOMik)[2]
  
  covariances <- array(dim = c(I, I))
  
  for (i in 1:I) {
    for (ip in 1:I) {
      covariances[i, ip] <- cov(FOMik[i, ], FOMik[ip, ])
    }
  }
  
  if (varInflFactor)  {
    covariances <- covariances * (K - 1)^2/K  # see paper by Efron and Stein 
  }
  
  Var <- 0
  count <- 0
  for (i in 1:I) {
    Var <- Var + covariances[i, i]
    count <- count + 1
  }
  if (count > 0) Var <- Var/count else Var <- 0
  
  Cov1 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      if (ip != i) {
        Cov1 <- Cov1 + covariances[i, ip]
        count <- count + 1
      }
    }
  }
  if (count > 0) Cov1 <- Cov1/count else Cov1 <- 0
  
  return(list(
    Var = Var,
    Cov1 = Cov1
  ))
  
}


FOMjk2VC <- function(FOMjk, varInflFactor) {
  
  J <- dim(FOMjk)[1]
  K <- dim(FOMjk)[2]
  
  covariances <- array(dim = c(J, J))
  
  for (j in 1:J) {
    for (jp in 1:J) {
      covariances[j, jp] <- cov(FOMjk[j, ], FOMjk[jp, ])
    }
  }
  
  if (varInflFactor)  {
    covariances <- covariances * (K - 1)^2/K  # see paper by Efron and Stein 
  }
  
  Var <- 0
  count <- 0
  for (j in 1:J) {
    Var <- Var + covariances[j, j]
    count <- count + 1
  }
  if (count > 0) Var <- Var/count else Var <- 0
  
  Cov2 <- 0
  count <- 0
  for (j in 1:J) {
    for (jp in 1:J) {
      if (j != jp) {
        Cov2 <- Cov2 + covariances[j, jp]
        count <- count + 1
      }
    }
  }
  if (count > 0) Cov2 <- Cov2/count else Cov2 <- 0
  
  
  return(list(
    Var = Var,
    Cov2 = Cov2
  ))
  
}


