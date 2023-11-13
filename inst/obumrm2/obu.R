obu <- function (ds1, ds2, FOM) {
  
  I_1 <- dim(ds1$ratings$NL)[1]
  I_2 <- dim(ds2$ratings$NL)[1]
  
  if ((I_1 !=2) && (I_1 != I_2)) stop("The two treatments must be identical.")
  
  I <- I_1
  
  J_1 <- dim(ds1$ratings$NL)[2]
  J_2 <- dim(ds2$ratings$NL)[2]
  
  J <- J_1 + J_2
  
  COV1 <- covariance(ds1, FOM = FOM)
  COV2<- covariance(ds2, FOM = FOM)
  
  COV <- array(0, dim = c(I, I, J, J))
  
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J_1) {
        for (jp in 1:J_1) {
          COV[i, ip, j, jp] <- COV1[i, ip, j, jp]
        }
      }
    }
  }
  
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J_2) {
        for (jp in 1:J_2) {
          COV[i, ip, j+J_1, jp+J_1] <- COV2[i, ip, j, jp]
        }
      }
    }
  }
  
  accur1 <- UtilFigureOfMerit(ds1, FOM = FOM)
  accur2 <- UtilFigureOfMerit(ds2, FOM = FOM)
  
  accur <- array(0, dim = c(I,J))
  
  accur[,1:J_1] <- accur1
  accur[,(J_1+1):J] <- accur2
  
  #	ESTIMATE THE MEAN FOR EACH MODALITY
  mean_t <- array(dim = I)
  for (i in 1:I) {
    sum <- 0
    for (j in 1:J) {
      sum <- sum + accur[i,j]
    }
    mean_t[i] <- sum/J
  }
  
  #	ESTIMATE THE MEAN FOR EACH READER
  mean_r <- array(dim = J)
  for (j in 1:J) {
    sum <- 0
    for (i in 1:I) {
      sum <- sum + accur[i,j]
    }
    mean_r[j] <- sum/I
  }
  
  #	ESTIMATE THE OVERALL MEAN
  
  sum <- 0
  for (i in 1:I) {
    sum <- sum + mean_t[i]
  }
  mean_o <- sum/I 
  
  # ESTIMATE Cov1
  sum <- 0
  sum2 <- 0
  minus <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      if (ip > i) {
        for (j in 1:J) {
          if((COV[i,i,j,j] == 0) || (COV[ip,ip,j,j] == 0)) {
            minus <- minus+1
            next
          }
          sum <- sum + COV[i,ip,j,j]/sqrt(COV[i,i,j,j]*COV[ip,ip,j,j])
          sum2 <- sum2 + COV[i,ip,j,j]
        }
      }
    }
  }
  r1 <- sum/(I*J*(I-1)/2) # not used and does not agree with RJafroc
  covr1 <- sum2/((I*J*(I-1)-minus)/2) # agrees with RJafroc
  
  #	ESTIMATE R_2 and Cov2
  sum <- 0
  sum2 <- 0
  count <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (jp in 1:J) {
        if (jp > j) {
          sum <- sum + COV[i,i,j,jp]/sqrt(COV[i,i,j,j]*COV[i,i,jp,jp])
          sum2 <- sum2 + COV[i,i,j,jp]
          count <- count + 1
        }
      }
    }
  }
  r2 <- sum / count  # not used
  covr2 <- sum2 / count # agrees with RJafroc
  
  
  #	ESTIMATE R_3 and Cov3
  sum <- 0
  sum2 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          if((ip != i) && (jp != j)) {
            sum <- sum + COV[i,ip,j,jp]/sqrt(COV[i,i,j,j]*COV[ip,ip,jp,jp])
            sum2 <- sum2 + COV[i,ip,j,jp]
            count <- count + 1
          }
        }
      }
    }
  }
  r3 <- sum/count  # not used
  covr3 <- sum2/count # agrees with RJafroc
  
  # Code for sum of squares etc
  correct <- 0
  totalss <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      correct <- correct + accur[i,j]
      totalss <- totalss + accur[i,j]*accur[i,j]
    }
  }
  
  correct <-  correct*correct/(I*J)
  totalss <- totalss - correct
  
  trtsum <- array(0, dim = I)
  readersum <- array(0, dim = J)
  
  for (i in 1:I) {
    for (j in 1:J) {
      trtsum[i] <- trtsum[i] + accur[i,j]
      readersum[j] <- readersum[j] + accur[i,j]
    }
  }
  
  trtsss <- 0
  for (i in 1:I) {
    trtsss <- trtsss + trtsum[i] * trtsum[i]
  }
  trtsss <- trtsss/J - correct
  
  readss <- 0
  for (j in 1:J) {
    readss <- readss + readersum[j]*readersum[j]
  }
  readss <- readss/I - correct
  
  ss_inter <- totalss - trtsss - readss
  ms_inter <- ss_inter/((I-1)*(J-1))
  
  #	CALCULATE THE DENOMINATOR OF FSTAR (equation 3.13) (Random-Reader)
  sum <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      sum <- sum + (accur[i,j] - mean_t[i] - mean_r[j] + mean_o)**2
    }
  }
  sum <- sum/((I-1)*(J-1))
  den_f <- sum + J*(covr2-covr3)
  
  #	CALCULATE THE NUMERATOR OF FSTAR (Random-Reader)
  sum <- 0
  for (i in 1:I) {
    sum <- sum + (mean_t[i] - mean_o)**2
  }
  num_f <- J*sum/(I-1)
  
  #	CALCULATE FSTAR 
  fstar <- num_f/den_f
  
  #	Revisions made on April 7, 2005 to accommodate the DFs proposed
  #	by Steve Hillis at ENAR 2005.
  #	GET THE P-VALUE OF FSTAR (Random-Reader)
  dfn <- I-1
  if((covr2-covr3) > 0) {
    dfd <- ((ms_inter + J*(covr2-covr3))**2)/((ms_inter**2)/((I-1)*(J-1)))
  }
  
  if((covr2-covr3) <= 0) {
    dfd <- ((ms_inter)**2)/((ms_inter**2)/((I-1)*(J-1)))
  }
  
  p_fstar_H <- 1 - pf(fstar, dfn, dfd) 
  
  return(p_fstar_H)
}
