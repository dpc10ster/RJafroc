# Old Code from 0.0.1 (the first version on CRAN)
# The error in p-value (noted by Lucy A) has been corrected below
# Otherwise the code is identical to XZ code
DBMHAnalysis <- function(dataset, FOM, alpha, option) 
{
  NL <- dataset$NL
  LL <- dataset$LL
  lesionVector <- dataset$lesionVector
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  if (!option %in% c("RRRC", "FRRC", "RRFC", "ALL")){
    errMsg <- sprintf("%s is not an available option.", option)
    stop(errMsg)
  }    
  
  if (I < 2) {
    stop("The analysis requires at least 2 modalities; consult Dr. Chakraborty for single-modality analysis")
  }
  
  fomArray <- UtilFigureOfMerit(dataset, FOM)
  trMeans <- rowMeans(fomArray)
  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    jkFOMArray <- array(dim = c(I, J, K1))
    pseudoValues <- array(dim = c(I, J, K1))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K1) {
          nl <- NL[i, j, -k, ]
          ll <- LL[i, j, , ]
          dim(nl) <- c(K - 1, maxNL)
          dim(ll) <- c(K2, max(lesionVector))
          jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, FOM)
          pseudoValues[i, j, k] <- fomArray[i, j] * K1 - jkFOMArray[i, j, k] * (K1 - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    jkFOMArray <- array(dim = c(I, J, K2))
    pseudoValues <- array(dim = c(I, J, K2))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K2) {
          nl <- NL[i, j, -(k + K1), ]
          ll <- LL[i, j, -k, ]
          dim(nl) <- c(K - 1, maxNL)
          dim(ll) <- c(K2 - 1, max(lesionVector))
          jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector[-k], lesionID[-k, ], lesionWeight[-k, ], maxNL, FOM)
          pseudoValues[i, j, k] <- fomArray[i, j] * K2 - jkFOMArray[i, j, k] * (K2 - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  } else {
    jkFOMArray <- array(dim = c(I, J, K))
    pseudoValues <- array(dim = c(I, J, K))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K) {
          if (k <= K1) {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(lesionVector))
            jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, FOM)
          } else {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, -(k - K1), ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(lesionVector))
            jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector[-(k - K1)], lesionID[-(k - K1), ], lesionWeight[-(k - K1), ], maxNL, FOM)
          }
          pseudoValues[i, j, k] <- fomArray[i, j] * K - jkFOMArray[i, j, k] * (K - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  }
  K <- length(pseudoValues[1, 1, ])
  
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(pseudoValues[i, , ]) - mean(pseudoValues))^2
  }
  msT <- msT * K * J/(I - 1)
  
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(pseudoValues[, j, ]) - mean(pseudoValues))^2
  }
  msR <- msR * K * I/(J - 1)
  
  
  msC <- 0
  for (k in 1:K) {
    msC <- msC + (mean(pseudoValues[, , k]) - mean(pseudoValues))^2
  }
  msC <- msC * I * J/(K - 1)
  
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, j, ]) + mean(pseudoValues))^2
    }
  }
  msTR <- msTR * K/((I - 1) * (J - 1))
  
  
  msTC <- 0
  for (i in 1:I) {
    for (k in 1:K) {
      msTC <- msTC + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
    }
  }
  msTC <- msTC * J/((I - 1) * (K - 1))
  
  
  msRC <- 0
  for (j in 1:J) {
    for (k in 1:K) {
      msRC <- msRC + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
    }
  }
  msRC <- msRC * I/((J - 1) * (K - 1))
  
  msTRC <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K) {
        msTRC <- msTRC + (pseudoValues[i, j, k] - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) - mean(pseudoValues[, j, k]) + 
                            mean(pseudoValues[i, , ]) + mean(pseudoValues[, j, ]) + mean(pseudoValues[, , k]) - mean(pseudoValues))^2
      }
    }
  }
  msTRC <- msTRC/((I - 1) * (J - 1) * (K - 1))
  
  varR <- (msR - msTR - msRC + msTRC)/(I * K)
  varC <- (msC - msTC - msRC + msTRC)/(I * J)
  varTR <- (msTR - msTRC)/K
  varTC <- (msTC - msTRC)/J
  varRC <- (msRC - msTRC)/I
  varErr <- msTRC
  varComp <- c(varR, varC, varTR, varTC, varRC, varErr)
  varCompName <- c("Var(R)", "Var(C)", "Var(T*R)", "Var(T*C)", "Var(R*C)", "Var(Error)")
  varComp <- data.frame(varComp, row.names = varCompName)
  
  msArray <- c(msT, msR, msC, msTR, msTC, msRC, msTRC)
  dfArray <- c(I - 1, J - 1, K - 1, (I - 1) * (J - 1), (I - 1) * (K - 1), (J - 1) * (K - 1), (I - 1) * (J - 1) * (K - 1))
  ssArray <- msArray * dfArray
  msArray <- c(msArray, NA)
  dfArray <- c(dfArray, sum(dfArray))
  ssArray <- c(ssArray, sum(ssArray))
  sourceArray <- c("T", "R", "C", "TR", "TC", "RC", "TRC", "Total")
  anovaY <- data.frame(Source = sourceArray, SS = ssArray, DF = dfArray, MS = msArray)
  
  msRSingle <- array(0, dim = c(I))
  msCSingle <- array(0, dim = c(I))
  msRCSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    for (j in 1:J) {
      msRSingle[i] <- msRSingle[i] + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]))^2
    }
    msRSingle[i] <- msRSingle[i] * K/(J - 1)
    
    
    for (k in 1:K) {
      msCSingle[i] <- msCSingle[i] + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]))^2
    }
    msCSingle[i] <- msCSingle[i] * J/(K - 1)
    
    for (j in 1:J) {
      for (k in 1:K) {
        msRCSingle[i] <- msRCSingle[i] + (mean(pseudoValues[i, j, k]) - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) + mean(pseudoValues[i, , ]))^2
      }
    }
    msRCSingle[i] <- msRCSingle[i]/((J - 1) * (K - 1))
  }
  sourceArraySingle <- c("R", "C", "RC")
  dfArraySingle <- c(J - 1, K - 1, (J - 1) * (K - 1))
  msArraySingle <- t(cbind(msRSingle, msCSingle, msRCSingle))
  msSingleTable <- data.frame(sourceArraySingle, dfArraySingle, msArraySingle, row.names = NULL)
  colnames(msSingleTable) <- c("Source", "DF", modalityID)
  
  diffTRMeans <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRMeans[ii] <- trMeans[i] - trMeans[ip]
      diffTRName[ii] <- paste(modalityID[i], modalityID[ip], sep = "-")
      ii <- ii + 1
    }
  }
  
  msNum <- msT
  
  if (option %in% c("RRRC", "ALL")) {
    # ************ RRRC ****************
    if (J > 1) {
      msDenRRRC <- msTR + max(msTC - msTRC, 0)
      fRRRC <- msNum/msDenRRRC
      ddfRRRC <- msDenRRRC^2/(msTR^2/((I - 1) * (J - 1)))
      pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
      stdErrRRRC <- sqrt(2 * msDenRRRC/J/K)
      tStat <- vector()
      tPr <- vector()
      CIRRRC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRRC
        tPr[i] <- 2 * pt(tStat[i], ddfRRRC)
        CIRRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRRC) * stdErrRRRC, diffTRMeans[i] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
        
      }
      ciDiffTrtRRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRRC, choose(I, 2)), DF = rep(ddfRRRC, choose(I, 2)), t = tStat, p = tPr, CI = CIRRRC)
      colnames(ciDiffTrtRRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
      
      dfSingleRRRC <- array(dim = I)
      msDenSingleRRRC <- array(dim = I)
      stdErrSingleRRRC <- array(dim = I)
      CISingleRRRC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRRC[i] <- msRSingle[i] + max(msCSingle[i] - msRCSingle[i], 0)
        dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
        stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J/K)
        CISingleRRRC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trMeans[i] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
      }
      ciAvgRdrEachTrtRRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRRC, DF = dfSingleRRRC, CI = CISingleRRRC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRRC) <- c("Treatment", "Area", "StdErr", "DF", "CI Lower", "CI Upper")
    } else {
      fRRRC <- NA
      ddfRRRC <- NA
      pRRRC <- NA
      ciDiffTrtRRRC <- NA
      ciAvgRdrEachTrtRRRC <- NA
    }
    if (option == "RRRC")
      return(list(fomArray = fomArray, anovaY = anovaY, anovaYi = msSingleTable, varComp = varComp, 
                  fRRRC = fRRRC, ddfRRRC = ddfRRRC, pRRRC = pRRRC, ciDiffTrtRRRC = ciDiffTrtRRRC, ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC))
  }
  
  if (option %in% c("FRRC", "ALL")) {
    # ************ FRRC ****************
    msDenFRRC <- msTC
    fFRRC <- msNum/msDenFRRC
    ddfFRRC <- (I - 1) * (K - 1)
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J/K)
    tStat <- vector()
    tPr <- vector()
    CIFRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrFRRC
      tPr[i] <- 2 * pt(tStat[i], ddfFRRC)
      CIFRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfFRRC) * stdErrFRRC, diffTRMeans[i] + qt(alpha/2, ddfFRRC) * stdErrFRRC))
    }
    ciDiffTrtFRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrFRRC, choose(I, 2)), DF = rep(ddfFRRC, choose(I, 2)), t = tStat, p = tPr, CI = CIFRRC)
    colnames(ciDiffTrtFRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
    
    dfSingleFRRC <- array(dim = I)
    msDenSingleFRRC <- array(dim = I)
    stdErrSingleFRRC <- array(dim = I)
    CISingleFRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleFRRC[i] <- msCSingle[i]
      dfSingleFRRC[i] <- (K - 1)
      stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J/K)
      CISingleFRRC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i], trMeans[i] + qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i]))
    }
    ciAvgRdrEachTrtFRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleFRRC, DF = dfSingleFRRC, CI = CISingleFRRC, row.names = NULL)
    colnames(ciAvgRdrEachTrtFRRC) <- c("Treatment", "Area", "StdErr", "DF", "CI Lower", "CI Upper")
    
    ssTFRRC <- array(0, dim = c(J))
    ssCFRRC <- array(0, dim = c(J))
    ssTCFRRC <- array(0, dim = c(J))
    for (j in 1:J) {
      for (i in 1:I) {
        ssTFRRC[j] <- ssTFRRC[j] + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[, j, ]))^2
      }
      ssTFRRC[j] <- ssTFRRC[j] * K
      
      for (k in 1:K) {
        ssCFRRC[j] <- ssCFRRC[j] + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]))^2
      }
      ssCFRRC[j] <- ssCFRRC[j] * I
      
      for (i in 1:I) {
        for (k in 1:K) {
          ssTCFRRC[j] <- ssTCFRRC[j] + (mean(pseudoValues[i, j, k]) - mean(pseudoValues[i, j, ]) - mean(pseudoValues[, j, k]) + mean(pseudoValues[, j, ]))^2
        }
      }
    }
    sourceArrayFRRC <- c("T", "C", "TC")
    dfArrayFRRC <- c(I - 1, K - 1, (I - 1) * (K - 1))
    ssArrayFRRC <- t(cbind(ssTFRRC, ssCFRRC, ssTCFRRC))
    ssTableFRRC <- data.frame(sourceArrayFRRC, dfArrayFRRC, ssArrayFRRC, row.names = NULL)
    colnames(ssTableFRRC) <- c("Source", "DF", readerID)
    
    msArrayFRRC <- ssArrayFRRC
    for (n in 1:3) msArrayFRRC[n, ] <- ssArrayFRRC[n, ]/dfArrayFRRC[n]
    msTableFRRC <- data.frame(sourceArrayFRRC, dfArrayFRRC, msArrayFRRC, row.names = NULL)
    colnames(msTableFRRC) <- c("Source", "DF", readerID)
    
    diffTRMeansFRRC <- array(dim = c(J, choose(I, 2)))
    for (j in 1:J) {
      ii <- 1
      for (i in 1:I) {
        if (i == I) 
          break
        for (ip in (i + 1):I) {
          diffTRMeansFRRC[j, ii] <- fomArray[i, j] - fomArray[ip, j]
          ii <- ii + 1
        }
      }
    }
    diffTRMeansFRRC <- as.vector(t(diffTRMeansFRRC))
    stdErrFRRC <- sqrt(2 * msArrayFRRC[3, ]/K)
    stdErrFRRC <- rep(stdErrFRRC, choose(I, 2))
    dim(stdErrFRRC) <- c(J, choose(I, 2))
    stdErrFRRC <- as.vector(t(stdErrFRRC))
    readerNames <- rep(readerID, choose(I, 2))
    dim(readerNames) <- c(J, choose(I, 2))
    readerNames <- as.vector(t(readerNames))
    trNames <- rep(diffTRName, J)
    dfReaderFRRC <- rep(K - 1, length(stdErrFRRC))
    CIReaderFRRC <- array(dim = c(length(stdErrFRRC), 2))
    tStat <- vector()
    tPr <- vector()
    for (n in 1:length(stdErrFRRC)) {
      tStat[n] <- diffTRMeansFRRC[n]/stdErrFRRC[n]
      tPr[n] <- 2 * pt(tStat[n], dfReaderFRRC[n])
      CIReaderFRRC[n, ] <- sort(c(diffTRMeansFRRC[n] - qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n], diffTRMeansFRRC[n] + qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n]))
    }
    ciDiffTrtEachRdr <- data.frame(Reader = readerNames, Treatment = trNames, Estimate = diffTRMeansFRRC, StdErr = stdErrFRRC, DF = dfReaderFRRC, t = tStat, p = tPr, CI = CIReaderFRRC)
    colnames(ciDiffTrtEachRdr) <- c("Reader", "Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
    if (option == "FRRC")
      return(list(fomArray = fomArray, anovaY = anovaY, anovaYi = msSingleTable, varComp = varComp, 
                  fFRRC = fFRRC, ddfFRRC = ddfFRRC, pFRRC = pFRRC, ciDiffTrtFRRC = ciDiffTrtFRRC, ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
                  ssAnovaEachRdr = ssTableFRRC, msAnovaEachRdr = msTableFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr))
  }
  
  if (option %in% c("RRFC", "ALL")) {
    # ************ RRFC ****************
    if (J > 1) {
      msDenRRFC <- msTR
      fRRFC <- msNum/msDenRRFC
      ddfRRFC <- ((I - 1) * (J - 1))
      pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
      stdErrRRFC <- sqrt(2 * msDenRRFC/J/K)
      tStat <- vector()
      tPr <- vector()
      CIRRFC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRFC
        tPr[i] <- 2 * pt(tStat[i], ddfRRFC)
        CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
      }
      ciDiffTrtRRFC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRFC, choose(I, 2)), DF = rep(ddfRRFC, choose(I, 2)), t = tStat, p = tPr, CI = CIRRFC)
      colnames(ciDiffTrtRRFC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
      
      dfSingleRRFC <- array(dim = I)
      msDenSingleRRFC <- array(dim = I)
      stdErrSingleRRFC <- array(dim = I)
      CISingleRRFC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRFC[i] <- msRSingle[i]
        dfSingleRRFC[i] <- (J - 1)
        stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J/K)
        CISingleRRFC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
      }
      ciAvgRdrEachTrtRRFC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRFC, DF = dfSingleRRFC, CI = CISingleRRFC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRFC) <- c("Treatment", "Area", "StdErr", "DF", "CI Lower", "CI Upper")
    } else {
      fRRFC <- NA
      ddfRRFC <- NA
      pRRFC <- NA
      ciDiffTrtRRFC <- NA
      ciAvgRdrEachTrtRRFC <- NA
    }
    if (option == "RRFC")
      return(list(fomArray = fomArray, anovaY = anovaY, anovaYi = msSingleTable, varComp = varComp, 
                  fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
  }
  
  return(list(fomArray = fomArray, anovaY = anovaY, anovaYi = msSingleTable, varComp = varComp, 
              fRRRC = fRRRC, ddfRRRC = ddfRRRC, pRRRC = pRRRC, ciDiffTrtRRRC = ciDiffTrtRRRC, ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC, 
              fFRRC = fFRRC, ddfFRRC = ddfFRRC, pFRRC = pFRRC, ciDiffTrtFRRC = ciDiffTrtFRRC, ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
              ssAnovaEachRdr = ssTableFRRC, msAnovaEachRdr = msTableFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr, 
              fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
} 


# Old Code from 0.0.1 (the first version on CRAN)
# The error in p-value (noted by Lucy A) has been corrected below
# Otherwise the code is identical to XZ code
ORHAnalysis <- function(dataset, FOM, alpha, covEstMethod, nBoots, option) 
{
  NL <- dataset$NL
  LL <- dataset$LL
  lesionVector <- dataset$lesionVector
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, max(lesionVector))
  
  if (!option %in% c("RRRC", "FRRC", "RRFC", "ALL")){
    errMsg <- sprintf("%s is not an available option.", option)
    stop(errMsg)
  }    
  
  if (I < 2) {
    stop("The analysis requires at least 2 modalities")
  }
  
  if (!covEstMethod %in% c("Jackknife", "Bootstrap", "DeLong")) {
    errMsg <- paste0(covEstMethod, " is not an allowed covariance estimation method.")
    stop(errMsg)
  }
  
  fomArray <- UtilFigureOfMerit(dataset, FOM)
  trMeans <- rowMeans(fomArray)
  fomMean <- mean(fomArray)
  
  ret <- EstimateVarCov(fomArray, NL, LL, lesionVector, lesionID, lesionWeight, maxNL, FOM, covEstMethod, nBoots)
  var <- ret$var
  cov1 <- ret$cov1
  cov2 <- ret$cov2
  cov3 <- ret$cov3
  
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(fomArray[i, ]) - fomMean)^2
  }
  msT <- J * msT/(I - 1)
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(fomArray[, j]) - fomMean)^2
  }
  msR <- I * msR/(J - 1)
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (fomArray[i, j] - mean(fomArray[i, ]) - mean(fomArray[, j]) + fomMean)^2
    }
  }
  msTR <- msTR/((J - 1) * (I - 1))
  
  varTR <- msTR - var + cov1 + max(cov2 - cov3, 0)
  varR <- (msR - var - (I - 1) * cov1 + cov2 + (I - 1) * cov3 - varTR)/I
  varCovArray <- c(varR, varTR, cov1, cov2, cov3, var)
  nameArray <- c("Var(R)", "Var(T*R)", "COV1", "COV2", "COV3", "Var(Error)")
  varComp <- data.frame(varCov = varCovArray, row.names = nameArray)
  
  varSingle <- vector(length = I)
  cov2Single <- vector(length = I)
  for (i in 1:I) {
    fomSingle <- fomArray[i, ]
    nl <- NL[i, , , ]
    ll <- LL[i, , , ]
    dim(fomSingle) <- c(1, J)
    dim(nl) <- c(1, J, K, maxNL)
    dim(ll) <- c(1, J, K2, max(lesionVector))
    ret <- EstimateVarCov(fomSingle, nl, ll, lesionVector, lesionID, lesionWeight, maxNL, FOM, covEstMethod, nBoots)
    varSingle[i] <- ret$var
    if (J > 1) {
      cov2Single[i] <- ret$cov2
    } else {
      cov2Single[i] <- 0
    }
  }
  
  varEchRder <- vector(length = J)
  cov1EchRder <- vector(length = J)
  for (j in 1:J) {
    fomSingle <- fomArray[, j]
    nl <- NL[, j, , ]
    ll <- LL[, j, , ]
    dim(fomSingle) <- c(I, 1)
    dim(nl) <- c(I, 1, K, maxNL)
    dim(ll) <- c(I, 1, K2, max(lesionVector))
    ret <- EstimateVarCov(fomSingle, nl, ll, lesionVector, lesionID, lesionWeight, maxNL, FOM, covEstMethod, nBoots)
    varEchRder[j] <- ret$var
    cov1EchRder[j] <- ret$cov1
  }
  
  msRSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    msRSingle[i] <- sum((fomArray[i, ] - trMeans[i])^2)/(J - 1)
  }
  
  diffTRMeans <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRMeans[ii] <- trMeans[i] - trMeans[ip]
      diffTRName[ii] <- paste(modalityID[i], modalityID[ip], sep = "-")
      ii <- ii + 1
    }
  }
  
  msNum <- msT
  
  # ************ RRRC ****************
  if (option %in% c("RRRC", "ALL")) {
    if (J > 1) {
      msDenRRRC <- msTR + max(J * (cov2 - cov3), 0)
      fRRRC <- msNum/msDenRRRC
      ddfRRRC <- msDenRRRC^2/(msTR^2/((I - 1) * (J - 1)))
      pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
      stdErrRRRC <- sqrt(2 * msDenRRRC/J)
      tStat <- vector()
      PrGTt <- vector()
      CIRRRC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRRC
        PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRRC, lower.tail = FALSE) # critical correction, noted by user Lucy D'Agostino McGowan
        # PrGTt[i] <- 2 * pt(tStat[i], ddfRRRC) # this was the mistake in original version of CRAN code
        CIRRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRRC) * stdErrRRRC, diffTRMeans[i] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
      }
      ciDiffTrtRRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRRC, choose(I, 2)), DF = rep(ddfRRRC, choose(I, 2)), t = tStat, p = PrGTt, CI = CIRRRC)
      colnames(ciDiffTrtRRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
      
      dfSingleRRRC <- array(dim = I)
      msDenSingleRRRC <- array(dim = I)
      stdErrSingleRRRC <- array(dim = I)
      CISingleRRRC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRRC[i] <- msRSingle[i] + max(J * cov2Single[i], 0)
        dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
        stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J)
        CISingleRRRC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trMeans[i] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
      }
      ciAvgRdrEachTrtRRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRRC, DF = dfSingleRRRC, CI = CISingleRRRC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRRC) <- c("Treatment", "Area", "StdErr", "DF", "CI Lower", "CI Upper")
    } else {
      fRRRC <- NA
      ddfRRRC <- NA
      pRRRC <- NA
      ciDiffTrtRRRC <- NA
      ciAvgRdrEachTrtRRRC <- NA
    }
    if (option == "RRRC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fRRRC = fRRRC, ddfRRRC = ddfRRRC, pRRRC = pRRRC, ciDiffTrtRRRC = ciDiffTrtRRRC, ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC))
    }
  }
  
  # ************ FRRC ****************
  if (option %in% c("FRRC", "ALL")) {
    if (J > 1) {
      msDenFRRC <- var - cov1 + (J - 1) * (cov2 - cov3)
    } else {
      msDenFRRC <- var - cov1
    }
    fFRRC <- msNum/msDenFRRC
    ddfFRRC <- Inf
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J)
    tStat <- vector()
    PrGTt <- vector()
    CIFRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrFRRC
      PrGTt[i] <- 2 * pt(tStat[i], ddfFRRC)
      CIFRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfFRRC) * stdErrFRRC, diffTRMeans[i] + qt(alpha/2, ddfFRRC) * stdErrFRRC))
    }
    ciDiffTrtFRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrFRRC, choose(I, 2)), DF = rep(ddfFRRC, choose(I, 2)), t = tStat, p = PrGTt, CI = CIFRRC)
    colnames(ciDiffTrtFRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
    
    dfSingleFRRC <- array(dim = I)
    msDenSingleFRRC <- array(dim = I)
    stdErrSingleFRRC <- array(dim = I)
    CISingleFRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleFRRC[i] <- varSingle[i] + (J - 1) * cov2Single[i]
      dfSingleFRRC[i] <- Inf
      stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J)
      CISingleFRRC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i], trMeans[i] + qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i]))
    }
    ciAvgRdrEachTrtFRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleFRRC, DF = dfSingleFRRC, CI = CISingleFRRC, row.names = NULL)
    colnames(ciAvgRdrEachTrtFRRC) <- c("Treatment", "Area", "StdErr", "DF", "CI Lower", "CI Upper")
    
    diffTRMeansFRRC <- array(dim = c(J, choose(I, 2)))
    for (j in 1:J) {
      ii <- 1
      for (i in 1:I) {
        if (i == I) 
          break
        for (ip in (i + 1):I) {
          diffTRMeansFRRC[j, ii] <- fomArray[i, j] - fomArray[ip, j]
          ii <- ii + 1
        }
      }
    }
    
    diffTRMeansFRRC <- as.vector(t(diffTRMeansFRRC))
    stdErrFRRC <- sqrt(2 * (varEchRder - cov1EchRder))
    stdErrFRRC <- rep(stdErrFRRC, choose(I, 2))
    dim(stdErrFRRC) <- c(J, choose(I, 2))
    stdErrFRRC <- as.vector(t(stdErrFRRC))
    readerNames <- rep(readerID, choose(I, 2))
    dim(readerNames) <- c(J, choose(I, 2))
    readerNames <- as.vector(t(readerNames))
    trNames <- rep(diffTRName, J)
    dfReaderFRRC <- rep(Inf, length(stdErrFRRC))
    CIReaderFRRC <- array(dim = c(length(stdErrFRRC), 2))
    tStat <- vector()
    PrGTt <- vector()
    for (n in 1:length(stdErrFRRC)) {
      tStat[n] <- diffTRMeansFRRC[n]/stdErrFRRC[n]
      PrGTt[n] <- 2 * pt(tStat[n], dfReaderFRRC[n])
      CIReaderFRRC[n, ] <- sort(c(diffTRMeansFRRC[n] - qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n], diffTRMeansFRRC[n] + qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n]))
    }
    ciDiffTrtEachRdr <- data.frame(Reader = readerNames, Treatment = trNames, Estimate = diffTRMeansFRRC, StdErr = stdErrFRRC, DF = dfReaderFRRC, t = tStat, p = PrGTt, CI = CIReaderFRRC)
    colnames(ciDiffTrtEachRdr) <- c("Reader", "Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
    
    varCovEachRdr <- data.frame(readerID, varEchRder, cov1EchRder)
    colnames(varCovEachRdr) <- c("Reader", "Var", "Cov1")
    if (option == "FRRC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fFRRC = fFRRC, ddfFRRC = ddfFRRC, pFRRC = pFRRC, ciDiffTrtFRRC = ciDiffTrtFRRC, ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr, varCovEachRdr = varCovEachRdr
      ))
    }
  }
  
  # ************ RRFC ****************
  if (option %in% c("RRFC", "ALL")) {
    if (J > 1) {
      msDenRRFC <- msTR
      fRRFC <- msNum/msDenRRFC
      ddfRRFC <- ((I - 1) * (J - 1))
      pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
      stdErrRRFC <- sqrt(2 * msDenRRFC/J)
      tStat <- vector()
      PrGTt <- vector()
      CIRRFC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRFC
        PrGTt[i] <- 2 * pt(tStat[i], ddfRRFC)
        CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
      }
      ciDiffTrtRRFC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRFC, choose(I, 2)), DF = rep(ddfRRFC, choose(I, 2)), t = tStat, p = PrGTt, CI = CIRRFC)
      colnames(ciDiffTrtRRFC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "Pr > t", "CI Lower", "CI Upper")
      
      dfSingleRRFC <- array(dim = I)
      msDenSingleRRFC <- array(dim = I)
      stdErrSingleRRFC <- array(dim = I)
      CISingleRRFC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRFC[i] <- msRSingle[i]
        dfSingleRRFC[i] <- (J - 1)
        stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J)
        CISingleRRFC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
      }
      ciAvgRdrEachTrtRRFC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRFC, DF = dfSingleRRFC, CI = CISingleRRFC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRFC) <- c("Treatment", "Area", "StdErr", "DF", "CI Lower", "CI Upper")
    } else {
      fRRFC <- NA
      ddfRRFC <- NA
      pRRFC <- NA
      ciDiffTrtRRFC <- NA
      ciAvgRdrEachTrtRRFC <- NA
    }
    if (option == "RRFC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
    }
  }
  
  return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
              fRRRC = fRRRC, ddfRRRC = ddfRRRC, pRRRC = pRRRC, ciDiffTrtRRRC = ciDiffTrtRRRC, ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC, 
              fFRRC = fFRRC, ddfFRRC = ddfFRRC, pFRRC = pFRRC, ciDiffTrtFRRC = ciDiffTrtFRRC, ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr, varCovEachRdr = varCovEachRdr, 
              fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
} 


# this is used with StOldCode.R

EstimateVarCov <- function(fomArray, NL, LL, lesionVector, lesionID, lesionWeight, maxNL, fom, covEstMethod, nBoots) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  
  K1 <- K - K2
  if (covEstMethod == "Jackknife") {
    if (fom %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
      jkFOMArray <- array(dim = c(I, J, K1))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K1) {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(lesionVector))
            jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, fom)
          }
        }
      }
    } else if (fom %in% c("MaxLLF", "HrSe")) {
      jkFOMArray <- array(dim = c(I, J, K2))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K2) {
            nl <- NL[i, j, -(k + K1), ]
            ll <- LL[i, j, -k, ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(lesionVector))
            jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector[-k], lesionID[-k, ], lesionWeight[-k, ], maxNL, fom)
          }
        }
      }
    } else {
      jkFOMArray <- array(dim = c(I, J, K))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K) {
            if (k <= K1) {
              nl <- NL[i, j, -k, ]
              ll <- LL[i, j, , ]
              dim(nl) <- c(K - 1, maxNL)
              dim(ll) <- c(K2, max(lesionVector))
              jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, fom)
            } else {
              nl <- NL[i, j, -k, ]
              ll <- LL[i, j, -(k - K1), ]
              dim(nl) <- c(K - 1, maxNL)
              dim(ll) <- c(K2 - 1, max(lesionVector))
              jkFOMArray[i, j, k] <- MyFOMOldCode(nl, ll, lesionVector[-(k - K1)], lesionID[-(k - K1), ], lesionWeight[-(k - K1), ], maxNL, fom)
            }
          }
        }
      }
    }
    K <- length(jkFOMArray[1, 1, ])
    Cov <- ResamplingEstimateVarCovs(jkFOMArray)
    var <- Cov$var * (K - 1)^2/K  # see paper by Efron and Stein
    cov1 <- Cov$cov1 * (K - 1)^2/K
    cov2 <- Cov$cov2 * (K - 1)^2/K
    cov3 <- Cov$cov3 * (K - 1)^2/K
  }
  
  if (covEstMethod == "Bootstrap") {
    if (fom %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
      fomBsArray <- array(dim = c(I, J, nBoots))
      for (b in 1:nBoots) {
        kBs <- ceiling(runif(K1) * K1)
        for (i in 1:I) {
          for (j in 1:J) {
            nlBs <- NL[i, j, kBs, ]
            llBs <- LL[i, j, , ]
            dim(nlBs) <- c(K1, maxNL)
            dim(llBs) <- c(K2, max(lesionVector))
            fomBsArray[i, j, b] <- MyFOMOldCode(nlBs, llBs, lesionVector, lesionID, lesionWeight, maxNL, fom)
          }
        }
      }
    } else if (fom %in% c("MaxLLF", "HrSe")) {
      fomBsArray <- array(dim = c(I, J, nBoots))
      for (b in 1:nBoots) {
        kBs <- ceiling(runif(K2) * K2)
        for (i in 1:I) {
          for (j in 1:J) {
            nlBs <- NL[i, j, c(1:K1, (kBs + K1)), ]
            llBs <- LL[i, j, kBs, ]
            dim(nlBs) <- c(K, maxNL)
            dim(llBs) <- c(K2, max(lesionVector))
            fomBsArray[i, j, b] <- MyFOMOldCode(nlBs, llBs, lesionVector[kBs], lesionID[kBs, ], lesionWeight[kBs, ], maxNL, fom)
          }
        }
      }
    } else {
      fomBsArray <- array(dim = c(I, J, nBoots))
      for (b in 1:nBoots) {
        kBs1 <- ceiling(runif(K1) * K1)
        kBs2 <- ceiling(runif(K2) * K2)
        for (i in 1:I) {
          for (j in 1:J) {
            nlBs <- NL[i, j, c(kBs1, kBs2 + K1), ]
            llBs <- LL[i, j, kBs2, ]
            dim(nlBs) <- c(K, maxNL)
            dim(llBs) <- c(K2, max(lesionVector))
            fomBsArray[i, j, b] <- MyFOMOldCode(nlBs, llBs, lesionVector[kBs2], lesionID[kBs2, ], lesionWeight[kBs2, ], maxNL, fom)
          }
        }
      }
    }
    Cov <- ResamplingEstimateVarCovs(fomBsArray)
    var <- Cov$var
    cov1 <- Cov$cov1
    cov2 <- Cov$cov2
    cov3 <- Cov$cov3
  }
  
  if (covEstMethod == "DeLong") {
    if (!fom %in% c("Wilcoxon", "HrAuc", "ROI")) 
      stop("DeLong\"s method can only be used for trapezoidal figures of merit.")
    
    if (fom == "ROI") {
      kI01 <- which(apply((NL[1, 1, , ] != UNINITIALIZED), 1, any))
      numKI01 <- rowSums((NL[1, 1, , ] != UNINITIALIZED))
      I01 <- length(kI01)
      I10 <- K2
      N <- sum((NL[1, 1, , ] != UNINITIALIZED))
      M <- sum(lesionVector)
      V01 <- array(dim = c(I, J, I01, maxNL))
      V10 <- array(dim = c(I, J, I10, max(lesionVector)))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:I10) {
            for (el in 1:lesionVector[k]) {
              V10[i, j, k, el] <- (sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) < LL[i, j, k, el]) + 0.5 * sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) == LL[i, j, k, el]))/N
            }
          }
          for (k in kI01) {
            for (el in 1:maxNL) {
              if (NL[i, j, k, el] == UNINITIALIZED) 
                next
              V01[i, j, k, el] <- (sum(NL[i, j, k, el] < as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])) + 0.5 * sum(NL[i, j, k, el] == as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])))/M
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
                s10[i, ip, j, jp] <- (s10[i, ip, j, jp] + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])]) - lesionVector[k] * fomArray[i, j]) * (sum(V10[ip, jp, k, !is.na(V10[ip, jp, k, ])]) - lesionVector[k] * fomArray[ip, 
                                                                                                                                                                                                                       jp]))
              }
              for (k in kI01) {
                s01[i, ip, j, jp] <- (s01[i, ip, j, jp] + (sum(V01[i, j, k, !is.na(V01[i, j, k, ])]) - numKI01[k] * fomArray[i, j]) * (sum(V01[ip, jp, k, !is.na(V01[ip, jp, k, ])]) - numKI01[k] * fomArray[ip, 
                                                                                                                                                                                                             jp]))
              }
              for (k in 1:K2) {
                if (all(NL[ip, jp, k + K1, ] == UNINITIALIZED)) 
                  next
                s11[i, ip, j, jp] <- (s11[i, ip, j, jp] + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])]) - lesionVector[k] * fomArray[i, j]) * (sum(V01[ip, jp, k + K1, !is.na(V01[ip, jp, k + K1, ])]) - numKI01[K1 + 
                                                                                                                                                                                                                k] * fomArray[ip, jp]))
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
      # ROI
      V10 <- array(dim = c(I, J, K2))
      V01 <- array(dim = c(I, J, K1))
      for (i in 1:I) {
        for (j in 1:J) {
          nl <- NL[i, j, 1:K1, ]
          ll <- cbind(NL[i, j, (K1 + 1):K, ], LL[i, j, , ])
          dim(nl) <- c(K1, maxNL)
          dim(ll) <- c(K2, maxNL + max(lesionVector))
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
    Cov <- ResamplingEstimateVarCovs(S)
    var <- Cov$var
    cov1 <- Cov$cov1
    cov2 <- Cov$cov2
    cov3 <- Cov$cov3
  }
  
  return(list(var = var, cov1 = cov1, cov2 = cov2, cov3 = cov3))
} 


# this function is needed ONLY with OldCode
MyFOMOldCode <- function(nl, ll, lesionNum, lesionID, lesionWeight, maxNL, fom) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  K <- length(nl[, 1])
  K2 <- length(ll[, 1])
  K1 <- K - K2
  
  FOM <- NA
  if (fom == "Wilcoxon") {
    truth <- c(rep(0, K1), rep(1, K2))
    ratings <- c(nl[1:K1], ll)
    FOM <- CalculateTrapezoidalArea(ratings, truth)
  } else if (fom == "HrAuc") {
    truth <- c(rep(0, K1), rep(1, K2))
    ratings <- array(dim = c(K1 + K2))
    
    for (k in 1:K1) {
      ratings[k] <- max(nl[k, ])
    }
    
    
    for (k in (K1 + 1):(K1 + K2)) {
      ratings[k] <- max(c(nl[k, ], ll[k - K1, ]))
    }
    
    FOM <- CalculateTrapezoidalArea(ratings, truth)
  } else if (fom == "HrSe") {
    tpCount <- 0
    for (k in 1:K2) {
      tempMax <- UNINITIALIZED
      for (el in 1:lesionNum[k]) tempMax <- max(tempMax, ll[k, el])
      
      for (el in 1:maxNL) tempMax <- max(tempMax, nl[k + K1, el])
      
      if (tempMax > UNINITIALIZED) 
        tpCount <- tpCount + 1
    }
    FOM <- tpCount/K2
  } else if (fom == "HrSp") {
    fpCount <- 0
    for (k in 1:K1) {
      tempMax <- UNINITIALIZED
      for (el in 1:maxNL) tempMax <- max(tempMax, nl[k, el])
      if (tempMax > UNINITIALIZED) 
        fpCount <- fpCount + 1
    }
    FOM <- 1 - fpCount/K1
  } else if (fom == "SongA1") {
    x <- array(UNINITIALIZED, dim = c(K))
    y1 <- array(UNINITIALIZED, dim = c(K2))
    y2 <- array(UNINITIALIZED, dim = c(K2))
    
    kInd <- which(apply(nl != UNINITIALIZED, 1, any))
    for (k in kInd){      
      x[ k ] <- mean(nl[ k, ( nl[ k, ] != UNINITIALIZED )])
    }
    y1 <- x[(K1 + 1):K]
    x <- x[1:K1]
    px0 <- sum(x == UNINITIALIZED)    
    
    kInd <- which(apply(ll != UNINITIALIZED, 1, any))
    for (k in kInd){      
      y2[ k ] <- mean(ll[ k, ( ll[ k, ] != UNINITIALIZED )])
    }
    y <- apply(cbind(y1, y2), 1, max)
    py0 <- sum(y == UNINITIALIZED)
    
    FOM <- 0
    x <- x[x != UNINITIALIZED]
    y <- y[y != UNINITIALIZED]
    for ( k1 in 1 : length(x)){
      FOM <- FOM + sum(x[k1] < y) + 0.5 * sum(x[k1] == y)
    }
    FOM <- FOM / (K1 * K2) + (px0 / K1) * ( 1 - 0.5 * py0 / K2)
  } else if (fom == "SongA2") {
    n0 <- apply(nl[1:K1, ] != UNINITIALIZED, 1, sum)
    n1 <- apply(cbind(nl[(K1 + 1):K, ], ll) != UNINITIALIZED, 1, sum)   
    k1Ind <- which(n0 > 0)
    k2Ind <- which(n1 > 0)
    px0 <- K1 - length(k1Ind)
    py0 <- K2 - length(k2Ind)
    v1 <- 0.5
    tw <- 0
    for (k1 in k1Ind) {
      for (k2 in k2Ind) {          
        v2 <- 0
        x <- nl[k1, ][nl[k1, ] != UNINITIALIZED]
        y1 <- nl[K1 + k2, ][nl[K1 + k2, ] != UNINITIALIZED]
        y2 <- ll[k2, ][ll[k2, ] != UNINITIALIZED]
        for (l in 1:length(x)){
          v2 <- v2 + sum(x[l] < y1) + 0.5 * sum(x[l] == y1) + sum(x[l] < y2) + 0.5 * sum(x[l] == y2)
        }          
        v2 <- v2/(n0[k1] * n1[k2])
        tw <- tw + (v1 < v2) + 0.5 * (v1 == v2)
      }
    }
    FOM <- tw/(K1 * K2) + px0/K1 * (1 - 0.5 * py0/K2)
  } else if (fom == "FOM_AFROC1") {
    numLesTotal <- sum(lesionNum)
    les <- as.vector(ll[lesionID != UNINITIALIZED])
    fp <- array(dim = c(K))
    for (k in 1:K) {
      fp[k] <- max(nl[k, ])
    }
    ratings <- c(fp, les)
    truth <- c(rep(0, K), rep(1, numLesTotal))
    FOM <- CalculateTrapezoidalArea(ratings, truth)
  } else if (fom == "JAFROC") {
    numLesTotal <- sum(lesionNum)
    les <- as.vector(ll[lesionID != UNINITIALIZED])
    fp <- array(dim = c(K1))
    for (k in 1:K1) {
      fp[k] <- max(nl[k, ])
    }
    ratings <- c(fp, les)
    truth <- c(rep(0, K1), rep(1, length(les)))
    FOM <- CalculateTrapezoidalArea(ratings, truth)
  } else if (fom == "FOM_wAFROC1") {
    numLesTotal <- sum(lesionNum)
    les <- as.vector(ll[lesionID != UNINITIALIZED])
    fp <- array(dim = c(K))
    for (k in 1:K) {
      fp[k] <- max(nl[k, ])
    }
    ratings <- c(fp, les)
    truth <- c(rep(0, K), rep(1, numLesTotal))
    weights <- lesionWeight[lesionWeight != UNINITIALIZED]
    FOM <- CalculateTrapezoidalAreaWeighted(ratings, truth, weights, K2)
  } else if (fom == "FOM_wAFROC") {
    numLesTotal <- sum(lesionNum)
    les <- as.vector(ll[lesionID != UNINITIALIZED])
    fp <- array(dim = c(K1))
    for (k in 1:K1) {
      fp[k] <- max(nl[k, ])
    }
    ratings <- c(fp, les)
    truth <- c(rep(0, K1), rep(1, numLesTotal))
    weights <- lesionWeight[lesionWeight != UNINITIALIZED]
    FOM <- CalculateTrapezoidalAreaWeighted(ratings, truth, weights, K2)
  } else if (fom == "MaxLLF") {
    numMarksTotal <- sum(ll != UNINITIALIZED)
    numLesTotal <- sum(lesionNum)
    FOM <- numMarksTotal/numLesTotal
  } else if (fom == "MaxNLF") {
    numMarksTotal <- sum(nl[1:K1, ] != UNINITIALIZED)
    FOM <- numMarksTotal/K1
  } else if (fom == "MaxNLFAllCases") {
    numMarksTotal <- sum(nl != UNINITIALIZED)
    FOM <- numMarksTotal/K
  } else if (fom == "ExpTrnsfmSp") {
    numMarksTotal <- sum(nl[1:K1, ] != UNINITIALIZED)
    FOM <- exp(-numMarksTotal/K1)
  } else if (fom == "ROI") {
    nn <- 0
    ns <- sum(lesionNum)
    tw <- 0
    for (k in 1:K) {
      for (el in 1:maxNL) {
        if (nl[k, el] == UNINITIALIZED) 
          next
        nn <- nn + 1
        tw <- tw + sum(nl[k, el] < as.vector(ll)) + 0.5 * sum(nl[k, el] == as.vector(ll))
      }
    }
    FOM <- tw/(nn * ns)
  } else {
    errMsg <- paste0(fom, " is not an available figure of merit.")
    stop(errMsg)
  }
  return(FOM)
} 




CalculateTrapezoidalArea <- function(rocRatings, truth) {
  K2 <- sum(truth)
  K1 <- length(truth) - K2
  
  K1Indx <- (1:length(truth))[!as.logical(truth)]  #index of incorrect cases; or normal cases
  K2Indx <- (1:length(truth))[as.logical(truth)]  #index of correct cases; or abnormal cases
  
  S <- 0
  for (k in K1Indx) {
    S <- S + sum(rocRatings[k] < rocRatings[K2Indx]) + 0.5 * sum(rocRatings[k] == rocRatings[K2Indx])
  }
  
  S <- S/(K1 * K2)
  
  return(S)
} 




CalculateTrapezoidalAreaWeighted <- function(rocRatings, truth, weights, numAbn) {
  K2 <- sum(truth)
  K1 <- length(truth) - K2
  
  K1Indx <- (1:length(truth))[!as.logical(truth)]  #index of incorrect cases; or normal cases
  K2Indx <- (1:length(truth))[as.logical(truth)]  #index of correct cases; or abnormal cases
  
  S <- 0
  for (k in K1Indx) {
    S <- S + sum((rocRatings[k] < rocRatings[K2Indx]) * weights) + 0.5 * sum((rocRatings[k] == rocRatings[K2Indx]) * weights)
  }
  
  S <- S/(K1 * numAbn)
  
  return(S)
} 


