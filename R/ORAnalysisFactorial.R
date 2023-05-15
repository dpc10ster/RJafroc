ORAnalysisFactorial <- function(dataset, FOM, FPFValue, alpha = 0.05, covEstMethod = "jackknife", 
                                nBoots = 200, analysisOption = "ALL")  
{
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  
  # `as.matrix` is NOT absolutely necessary as `mean()` function is not used
  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  ret <- UtilVarComponentsOR(dataset, FOM, FPFValue, covEstMethod, nBoots)
  
  TRanova <- ret$TRanova
  VarCom <-  ret$VarCom
  IndividualTrt <- ret$IndividualTrt
  IndividualRdr <- ret$IndividualRdr
  
  ANOVA <- list()
  ANOVA$TRanova <- TRanova
  ANOVA$VarCom <- VarCom
  ANOVA$IndividualTrt <- IndividualTrt
  ANOVA$IndividualRdr <- IndividualRdr
  
  trtMeans <- rowMeans(foms)
  trtMeans <- as.data.frame(trtMeans)
  colnames(trtMeans) <- "Estimate"
  
  trtMeanDiffs <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
      diffTRName[ii] <- paste0("trt", modalityID[i], sep = "-", "trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("Estimate" = trtMeanDiffs,
                             row.names = diffTRName,
                             stringsAsFactors = FALSE)
  
  FOMs <- list(
    foms = foms,
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs
  )
  
  if (analysisOption == "RRRC") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
  
} 


# 5/30/20 returning zeroes instead of NAs; simplifies handling of SPLIT-PLOT-C dataseets
FOMijk2VarCov <- function(resampleFOMijk, varInflFactor) {
  
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
  I <- dim(covariances)[1]
  J <- dim(covariances)[3]
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
    Cov3 = Cov3,
    CovMatrix = covariances
  ))
  
}



FOMijk2VarCovSpA <- function(resampleFOMijk, varInflFactor) {
  
  I <- dim(resampleFOMijk)[1]
  J <- dim(resampleFOMijk)[2]
  K <- dim(resampleFOMijk)[3]
  
  covariances <- array(dim = c(I, I, J, J))
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          if (any(is.na(resampleFOMijk[i, j, ])) || any(is.na(resampleFOMijk[ip, jp, ]))) next
          covariances[i, ip, j, jp] <- cov(resampleFOMijk[i, j, ], resampleFOMijk[ip, jp, ])
        }
      }
    }
  }
  
  # See dropbox/RjafrocMaintenance/SpAMethods/Cov2Cov3Str.xlsx for hand calculations
  Var_i <- rep(0,I)
  count_i <- rep(0,I)
  for (i in 1:I) {
    rdr_i <- which(!is.na(resampleFOMijk[i,,1]))
    for (j in 1:length(rdr_i)) {
      if (is.na(covariances[i, i, rdr_i[j], rdr_i[j]])) next
      Var_i[i] <- Var_i[i] + covariances[i, i, rdr_i[j], rdr_i[j]]
      count_i[i] <- count_i[i] + 1
    }
    if (count_i[i] > 0) Var_i[i] <- Var_i[i]/count_i[i] else Var_i[i] <- 0
  }
  
  Cov2_i <- rep(0,I)
  count_i <- rep(0,I)
  for (i in 1:I) {
    rdr_i <- which(!is.na(resampleFOMijk[i,,1]))
    for (j in 1:length(rdr_i)) {
      for (jp in 1:length(rdr_i)) {
        if (rdr_i[j] != rdr_i[jp]) {
          if (is.na(covariances[i, i, rdr_i[j], rdr_i[jp]])) next
          Cov2_i[i] <- Cov2_i[i] + covariances[i, i, rdr_i[j], rdr_i[jp]]
          count_i[i] <- count_i[i] + 1
        }
      }
    }
    if (count_i[i] > 0) Cov2_i[i] <- Cov2_i[i]/count_i[i] else Cov2_i[i] <- 0
  }
  
  Cov3_i <- rep(0,I)
  count_i <- rep(0,I)
  for (i in 1:I) {
    for (ip in 1:I) {
      rdr_i <- which(!is.na(resampleFOMijk[i,,1]))
      rdr_ip <- which(!is.na(resampleFOMijk[ip,,1]))
      if (i != ip) {
        for (j in 1:length(rdr_i)) {
          for (jp in 1:length(rdr_ip)) {
            if (rdr_i[j] != rdr_ip[jp]) {
              if (is.na(covariances[i, ip, rdr_i[j], rdr_ip[jp]])) next
              Cov3_i[i] <- Cov3_i[i] + covariances[i, ip, rdr_i[j], rdr_ip[jp]]
              count_i[i] <- count_i[i] + 1
            }
          }
        }
      }
    }
    if (count_i[i] > 0) Cov3_i[i] <- Cov3_i[i]/count_i[i] else Cov3_i[i] <- 0
  }
  
  if (varInflFactor)  {
    Var_i <-  Var_i * (K - 1)^2/K  # see paper by Efron and Stein 
    Cov2_i  <-  Cov2_i * (K - 1)^2/K
    Cov3_i <-  Cov3_i  * (K - 1)^2/K
  }
  
  return(list(Var_i = Var_i,
              Cov2_i = Cov2_i,
              Cov3_i = Cov3_i
  ))
  
}



varComponentsJackknifeFactorial <- function(dataset, FOM, FPFValue) {
  if (dataset$descriptions$design != "FCTRL") stop("This functions requires a factorial dataset")  
  
  K <- length(dataset$ratings$NL[1,1,,1])
  
  ret <- UtilPseudoValues(dataset, FOM, FPFValue)
  CovTemp <- FOMijk2VarCov(ret$jkFomValues, varInflFactor = TRUE)
  Cov <- list(
    Var = CovTemp$Var,
    Cov1 = CovTemp$Cov1,
    Cov2 = CovTemp$Cov2,
    Cov3 = CovTemp$Cov3
  )
  
  return(Cov)
  
}



#' @importFrom stats runif
varComponentsBootstrapFactorial <- function(dataset, FOM, FPFValue, nBoots, seed) 
{
  if (dataset$descriptions$design != "FCTRL") stop("This functions requires a factorial dataset")  
  
  set.seed(seed) ## added 4/28/20, to test reproducibility with RJafrocBook code
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  perCase <- dataset$lesions$perCase
  IDs <- dataset$lesions$IDs
  weights <- dataset$lesions$weights
  
  I <- length(NL[,1,1,1])
  J <- length(NL[1,,1,1])
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- (K - K2)
  maxNL <- length(NL[1,1,1,])
  maxLL <- length(LL[1,1,1,])
  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    stop("This needs fixing")
    fomBsArray <- array(dim = c(I, J, nBoots))
    for (b in 1:nBoots) {
      kBs <- ceiling(runif(K1) * K1)
      for (i in 1:I) {
        for (j in 1:J) {
          NLbs <- NL[i, j, kBs, ]
          LLbs <- LL[i, j, , ]
          dim(NLbs) <- c(K1, maxNL)
          dim(LLbs) <- c(K2, maxLL)
          fomBsArray[i, j, b] <- MyFom_ij(NLbs, LLbs, 
                                          perCase, IDs, 
                                          weights, maxNL, 
                                          maxLL, K1, K2, 
                                          FOM, FPFValue)
        }
      }
    }
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    stop("This needs fixing")
    fomBsArray <- array(dim = c(I, J, nBoots))
    for (b in 1:nBoots) {
      kBs <- ceiling(runif(K2) * K2)
      for (i in 1:I) {
        for (j in 1:J) {
          NLbs <- NL[i, j, c(1:K1, (kBs + K1)), ]
          LLbs <- LL[i, j, kBs, ]
          dim(NLbs) <- c(K, maxNL)
          dim(LLbs) <- c(K2, maxLL)
          lesionIDBs <- IDs[kBs, ]
          dim(lesionIDBs) <- c(K2, maxLL)
          lesionWeightBs <- weights[kBs, ]
          dim(lesionWeightBs) <- c(K2, maxLL)
          fomBsArray[i, j, b] <- MyFom_ij(NLbs, LLbs, 
                                          perCase[kBs], lesionIDBs, 
                                          lesionWeightBs, maxNL, maxLL, 
                                          K1, K2, FOM, FPFValue)
        }
      }
    }
  } else { # original code had errors; see Fadi RRRC code; Aug 9, 2017 !!dpc!!!
    ## however, following code needs checking
    ##stop("this code needs checking; contact Dr. Chakraborty with dataset and code that lands here; 8/9/2017")
    fomBsArray <- array(dim = c(I, J, nBoots))
    for (b in 1:nBoots) {
      k1bs <- ceiling(runif(K1) * K1)
      k2bs <- ceiling(runif(K2) * K2)
      for (i in 1:I) {
        for (j in 1:J) {
          NLbs <- NL[i, j, c(k1bs, k2bs + K1), ]
          lesionVectorbs <- perCase[k2bs]            
          LLbs <- LL[i, j, k2bs,1:max(lesionVectorbs)] 
          dim(NLbs) <- c(K, maxNL)
          dim(LLbs) <- c(K2, max(lesionVectorbs))  
          lesionIDBs <- IDs[k2bs, ]
          dim(lesionIDBs) <- c(K2, maxLL)
          lesionWeightBs <- weights[k2bs, ]
          dim(lesionWeightBs) <- c(K2, maxLL)
          fomBsArray[i, j, b] <- MyFom_ij(NLbs, LLbs, lesionVectorbs, lesionIDBs, 
                                          lesionWeightBs, maxNL, maxLL, K1, K2, FOM, FPFValue)
        }
      }
    }
  }
  
  Cov <- FOMijk2VarCov(fomBsArray, varInflFactor = FALSE)
  Var <- Cov$Var
  Cov1 <- Cov$Cov1
  Cov2 <- Cov$Cov2
  Cov3 <- Cov$Cov3
  
  return(list(
    Var = Var, 
    Cov1 = Cov1, 
    Cov2 = Cov2, 
    Cov3 = Cov3
  ))
  
}



varComponentsDeLongFactorial <- function(dataset, FOM)
{
  if (dataset$descriptions$design != "FCTRL") stop("This functions requires a factorial dataset")  
  
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  perCase <- dataset$lesions$perCase
  
  I <- length(NL[,1,1,1])
  J <- length(NL[1,,1,1])
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- (K - K2)
  maxNL <- length(NL[1,1,1,])
  maxLL <- length(LL[1,1,1,])
  # if ((maxLL != 1) || (maxLL != 1)) stop("dataset error in varComponentsDeLongFactorial")
  
  fomArray <- UtilFigureOfMerit(dataset, FOM)
  
  if (!FOM %in% c("Wilcoxon", "HrAuc", "ROI")) 
    stop("DeLong\"s method can only be used for trapezoidal figures of merit.")
  
  if (FOM == "ROI") {
    kI01 <- which(apply((NL[1, 1, , ] != UNINITIALIZED), 1, any))
    numKI01 <- rowSums((NL[1, 1, , ] != UNINITIALIZED))
    I01 <- length(kI01)
    I10 <- K2
    N <- sum((NL[1, 1, , ] != UNINITIALIZED))
    M <- sum(perCase)
    V01 <- array(dim = c(I, J, I01, maxNL))
    V10 <- array(dim = c(I, J, I10, maxLL))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:I10) {
          for (el in 1:perCase[k]) {
            V10[i, j, k, el] <- (sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) < LL[i, j, k, el]) 
                                 + 0.5 * sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) == LL[i, j, k, el]))/N
          }
        }
        for (k in 1:I01) {
          for (el in 1:maxNL) {
            if (NL[i, j, kI01[k], el] == UNINITIALIZED) 
              next
            V01[i, j, k, el] <- (sum(NL[i, j, kI01[k], el] < as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])) 
                                 + 0.5 * sum(NL[i, j, kI01[k], el] == as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])))/M
          }
        }
      }
    }
    s10 <- array(0, dim = c(I, I, J, J))
    s01 <- array(0, dim = c(I, I, J, J))
    s11 <- array(0, dim = c(I, I, J, J))
    for (i in 1:I) {
      for (ip in 1:I) {
        for (j in 1:J) {
          for (jp in 1:J) {
            for (k in 1:I10) {
              s10[i, ip, j, jp] <- (s10[i, ip, j, jp]
                                    + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])])
                                       - perCase[k] * fomArray[i, j])
                                    * (sum(V10[ip, jp, k, !is.na(V10[ip, jp, k, ])]) 
                                       - perCase[k] * fomArray[ip, jp]))
            }
            for (k in 1:I01) {
              s01[i, ip, j, jp] <- (s01[i, ip, j, jp] 
                                    + (sum(V01[i, j, k, !is.na(V01[i, j, k, ])]) 
                                       - numKI01[kI01[k]] * fomArray[i, j]) 
                                    * (sum(V01[ip, jp, k, !is.na(V01[ip, jp, k, ])]) 
                                       - numKI01[kI01[k]] * fomArray[ip, jp]))
            }
            allAbn <- 0
            for (k in 1:K2) {
              if (all(NL[ip, jp, k + K1, ] == UNINITIALIZED)) {
                allAbn <- allAbn + 1
                next
              }                  
              s11[i, ip, j, jp] <- (s11[i, ip, j, jp] 
                                    + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])]) 
                                       - perCase[k] * fomArray[i, j]) 
                                    * (sum(V01[ip, jp, k + K1 - allAbn, !is.na(V01[ip, jp, k + K1 - allAbn, ])]) 
                                       - numKI01[K1 + k] * fomArray[ip, jp]))
            }
          }
        }
      }
    }
    s10 <- s10 * I10/(I10 - 1)/M
    s01 <- s01 * I01/(I01 - 1)/N
    s11 <- s11 * K/(K - 1)
    S <- array(0, dim = c(I, I, J, J))
    for (i in 1:I) {
      for (ip in 1:I) {
        for (j in 1:J) {
          for (jp in 1:J) {
            S[i, ip, j, jp] <- s10[i, ip, j, jp]/M + s01[i, ip, j, jp]/N + s11[i, ip, j, jp]/(M * N) + s11[ip, i, jp, j]/(M * N)
          }
        }
      }
    }
  } else {
    # ROC
    V10 <- array(dim = c(I, J, K2))
    V01 <- array(dim = c(I, J, K1))
    for (i in 1:I) {
      for (j in 1:J) {
        nl <- NL[i, j, 1:K1, ]
        ll <- cbind(NL[i, j, (K1 + 1):K, ], LL[i, j, , ])
        dim(nl) <- c(K1, maxNL)
        dim(ll) <- c(K2, maxNL + maxLL)
        fp <- apply(nl, 1, max)
        tp <- apply(ll, 1, max)
        for (k in 1:K2) {
          V10[i, j, k] <- (sum(fp < tp[k]) + 0.5 * sum(fp == tp[k]))/K1
        }
        for (k in 1:K1) {
          V01[i, j, k] <- (sum(fp[k] < tp) + 0.5 * sum(fp[k] == tp))/K2
        }
      }
    }
    s10 <- array(dim = c(I, I, J, J))
    s01 <- array(dim = c(I, I, J, J))
    for (i in 1:I) {
      for (ip in 1:I) {
        for (j in 1:J) {
          for (jp in 1:J) {
            s10[i, ip, j, jp] <- sum((V10[i, j, ] - fomArray[i, j]) * (V10[ip, jp, ] - fomArray[ip, jp]))/(K2 - 1)
            s01[i, ip, j, jp] <- sum((V01[i, j, ] - fomArray[i, j]) * (V01[ip, jp, ] - fomArray[ip, jp]))/(K1 - 1)
          }
        }
      }
    }
    S <- s10/K2 + s01/K1
  }
  
  covariances <- S
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
    Cov3 = Cov3))
}

