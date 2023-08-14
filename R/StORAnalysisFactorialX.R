StORAnalysisFactorialX <- function(dsX, avgIndx, FOM, analysisOption, alpha)  
{
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID1 <- dsX$descriptions$modalityID1
  modalityID2 <- dsX$descriptions$modalityID2
  
  I1 <- length(modalityID1)
  I2 <- length(modalityID2)
  
  # `as.matrix` is NOT absolutely necessary as `mean()` function is not used
  foms <- UtilFigureOfMeritX(dsX, FOM)
  fomArray1 <- data.matrix(foms$avg1) # averaged over 1st modality
  fomArray2 <- data.matrix(foms$avg2) # averaged over 2nd modality
  
  ret <- UtilVarComponentsORX(dsX, FOM)
  
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
    RRRC <- ORSummaryRRRC(dsX, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- ORSummaryFRRC(dsX, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- ORSummaryRRFC(dsX, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- ORSummaryRRRC(dsX, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- ORSummaryFRRC(dsX, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- ORSummaryRRFC(dsX, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
  
} 


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



varComponentsJackknifeFactorial <- function(ds, FOM, FPFValue) {
  if (ds$descriptions$design != "FCTRL") stop("This functions requires a factorial dataset")  
  
  K <- length(ds$ratings$NL[1,1,,1])
  
  ret <- UtilPseudoValues(ds, FOM, FPFValue)
  CovTemp <- FOMijk2VarCov(ret$jkFomValues, varInflFactor = TRUE)
  Cov <- list(
    Var = CovTemp$Var,
    Cov1 = CovTemp$Cov1,
    Cov2 = CovTemp$Cov2,
    Cov3 = CovTemp$Cov3
  )
  
  return(Cov)
  
}


