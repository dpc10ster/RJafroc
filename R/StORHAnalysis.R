StORHAnalysis <- function(dataset, FOM, FPFValue, alpha = 0.05, covEstMethod = "Jackknife", 
                          nBoots = 200, option = "ALL")  
{
  
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  
  if (!covEstMethod %in% c("Jackknife", "Bootstrap", "DeLong")) {
    errMsg <- paste0(covEstMethod, " is not an allowed covariance estimation method for ORH analysis.")
    stop(errMsg)
  }
  
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  trtMeans <- rowMeans(fomArray)
  
  ret <- UtilVarComponentsOR(dataset, FOM, FPFValue, covEstMethod, nBoots)
  varComp <-  ret$varComp
  meanSquares <- ret$meanSquares
  
  cov1 <- varComp$cov1
  cov2 <- varComp$cov2
  cov3 <- varComp$cov3
  var <- varComp$var
  
  varEachTrt <- vector(length = I)
  cov2EachTrt <- vector(length = I)
  for (i in 1:I) {
    fomSingle <- fomArray[i, ]
    dim(fomSingle) <- c(1, J)
    dsi <- DfExtractDataset(dataset, trts = i)
    ret <- gpfEstimateVarCov(dsi, FOM, FPFValue, nBoots, covEstMethod)
    varEachTrt[i] <- ret$var
    cov2EachTrt[i] <- ret$cov2
  }
  
  varEachRdr <- vector(length = J)
  cov1EachRdr <- vector(length = J)
  for (j in 1:J) {
    fomSingle <- fomArray[, j]
    dim(fomSingle) <- c(I, 1)
    dsj <- DfExtractDataset(dataset, rdrs = j)
    ret <- gpfEstimateVarCov(dsj, FOM, FPFValue, nBoots, covEstMethod)
    varEachRdr[j] <- ret$var
    cov1EachRdr[j] <- ret$cov1
  }
  
  msRSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    msRSingle[i] <- sum((fomArray[i, ] - trtMeans[i])^2)/(J - 1)
  }
  
  diffTRMeans <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRMeans[ii] <- trtMeans[i] - trtMeans[ip]
      diffTRName[ii] <- paste0("Trt", modalityID[i], sep = "-", "Trt", modalityID[ip]) #  # !sic
      ii <- ii + 1
    }
  }
  
  # ************ RRRC ****************
  # ************ RRRC ****************
  # ************ RRRC ****************
  if (option %in% c("RRRC", "ALL")) {
    msDenRRRC <- meanSquares$msTR + max(J * (cov2 - cov3), 0)
    fRRRC <- meanSquares$msT/msDenRRRC
    ddfRRRC <- msDenRRRC^2/((meanSquares$msTR)^2/((I - 1) * (J - 1)))
    pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
    FTestStatsRRRC <- data.frame(fRRRC = fRRRC,
                                 ndfRRRC = (I-1),
                                 ddfRRRC = ddfRRRC,
                                 pRRRC = pRRRC)
    stdErrRRRC <- sqrt(2 * msDenRRRC/J)
    tStat <- vector()
    PrGTt <- vector()
    CIRRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrRRRC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRRC, lower.tail = FALSE) # critical correction, noted by user Lucy D'Agostino McGowan
      ci <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRRC) * stdErrRRRC, diffTRMeans[i] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
      if (length(ci) == 0){
        CIRRRC[i, ] <- c(NA, NA)
      }else{
        CIRRRC[i, ] <- ci
      }
    }
    ciDiffTrtRRRC <- data.frame(Treatment = diffTRName, 
                                Estimate = diffTRMeans, 
                                StdErr = rep(stdErrRRRC, choose(I, 2)), 
                                DF = rep(ddfRRRC, choose(I, 2)), 
                                t = tStat, 
                                PrGTt = PrGTt, 
                                CILower = CIRRRC[,1],
                                CIUpper = CIRRRC[,2])
    
    dfSingleRRRC <- array(dim = I)
    msDenSingleRRRC <- array(dim = I)
    stdErrSingleRRRC <- array(dim = I)
    CISingleRRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleRRRC[i] <- msRSingle[i] + max(J * cov2EachTrt[i], 0)
      dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
      stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J)
      ci <- sort(c(trtMeans[i] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trtMeans[i] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
      if (length(ci) == 0){
        CISingleRRRC[i, ] <- c(NA, NA)
      }else{
        CISingleRRRC[i, ] <- ci
      }
      
    }
    ciAvgRdrEachTrtRRRC <- data.frame(Treatment = paste("Trt", modalityID, sep = ""), 
                                      Area = trtMeans, 
                                      StdErr = as.vector(stdErrSingleRRRC), 
                                      DF = as.vector(dfSingleRRRC), 
                                      CILower = CISingleRRRC[,1], 
                                      CIUpper = CISingleRRRC[,2], 
                                      row.names = NULL)
    
    if (option == "RRRC"){
      return(list(
        fomArray = fomArray, 
        meanSquares = meanSquares, 
        varComp = varComp,
        FTestStatsRRRC = FTestStatsRRRC, 
        ciDiffTrtRRRC = ciDiffTrtRRRC, 
        ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC
      ))
    }
  }
  
  # ************ FRRC ****************
  # ************ FRRC ****************
  # ************ FRRC ****************
  if (option %in% c("FRRC", "ALL")) {
    msDenFRRC <- var - cov1 + (J - 1) * (cov2 - cov3)
    # if (J > 1) {
    #   msDenFRRC <- var - cov1 + (J - 1) * (cov2 - cov3)
    # } else {
    #   msDenFRRC <- var - cov1
    # }
    fFRRC <- meanSquares$msT/msDenFRRC
    ddfFRRC <- Inf
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    FTestStatsFRRC <- data.frame(fFRRC = fFRRC,
                                 ndfFRRC = (I-1),
                                 ddfFRRC = ddfFRRC,
                                 pFRRC = pFRRC)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J)
    tStat <- vector()
    PrGTt <- vector()
    CIFRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrFRRC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfFRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIFRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfFRRC) * stdErrFRRC, diffTRMeans[i] + qt(alpha/2, ddfFRRC) * stdErrFRRC))
    }
    ciDiffTrtFRRC <- data.frame(Treatment = diffTRName, 
                                Estimate = diffTRMeans, 
                                StdErr = rep(stdErrFRRC, choose(I, 2)),
                                DF = rep(ddfFRRC, choose(I, 2)), 
                                t = tStat, 
                                PrGTt = PrGTt, 
                                CILower = CIFRRC[,1],
                                CIUpper = CIFRRC[,2])
    
    dfSingleFRRC <- array(dim = I)
    msDenSingleFRRC <- array(dim = I)
    stdErrSingleFRRC <- array(dim = I)
    CISingleFRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleFRRC[i] <- varEachTrt[i] + (J - 1) * cov2EachTrt[i]
      dfSingleFRRC[i] <- Inf
      stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J)
      CISingleFRRC[i, ] <- sort(c(trtMeans[i] - qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i], trtMeans[i] + qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i]))
    }
    ciAvgRdrEachTrtFRRC <- data.frame(Treatment = paste0("Trt", modalityID), 
                                      Area = trtMeans, 
                                      StdErr = as.vector(stdErrSingleFRRC), 
                                      DF = as.vector(dfSingleFRRC), 
                                      CILower = CISingleFRRC[,1], 
                                      CIUpper = CISingleFRRC[,2], row.names = NULL)
    
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
    stdErrFRRC <- sqrt(2 * (varEachRdr - cov1EachRdr))
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
      PrGTt[n] <- 2 * pt(abs(tStat[n]), dfReaderFRRC[n], lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIReaderFRRC[n, ] <- sort(c(diffTRMeansFRRC[n] - qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n], diffTRMeansFRRC[n] + qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n]))
    }
    ciDiffTrtEachRdrFRRC <- data.frame(Reader = paste("Rdr", readerNames, sep = ""), 
                                       Treatment = trNames, 
                                       Estimate = diffTRMeansFRRC, 
                                       StdErr = stdErrFRRC, 
                                       DF = dfReaderFRRC, 
                                       t = tStat, 
                                       PrGTt = PrGTt, 
                                       CILower = CIReaderFRRC[,1],
                                       CIUpper = CIReaderFRRC[,2])
    
    varCovEachRdr <- data.frame(Reader = paste("Rdr", readerID, sep = ""),
                                Var = varEachRdr, 
                                Cov1 = cov1EachRdr)
    if (option == "FRRC"){
      return(data.frame(fomArray = fomArray, 
                        meanSquares = meanSquares, 
                        varComp = varComp,
                        FTestStatsFRRC = FTestStatsFRRC,
                        ciDiffTrtFRRC = ciDiffTrtFRRC, 
                        ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
                        ciDiffTrtEachRdrFRRC = ciDiffTrtEachRdrFRRC, 
                        varCovEachRdr = varCovEachRdr
      ))
    }
  }
  
  # ************ RRFC ****************
  # ************ RRFC ****************
  # ************ RRFC ****************
  if (option %in% c("RRFC", "ALL")) {
    msDenRRFC <- meanSquares$msTR
    fRRFC <- meanSquares$msT/msDenRRFC
    ddfRRFC <- ((I - 1) * (J - 1))
    pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
    FTestStatsRRFC <- data.frame(fRRFC = fRRFC,
                                 ndfRRFC = (I-1),
                                 ddfRRFC = ddfRRFC,
                                 pRRFC = pRRFC)
    stdErrRRFC <- sqrt(2 * msDenRRFC/J)
    tStat <- vector()
    PrGTt <- vector()
    CIRRFC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrRRFC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRFC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
    }
    ciDiffTrtRRFC <- data.frame(Treatment = diffTRName, 
                                Estimate = diffTRMeans, 
                                StdErr = rep(stdErrRRFC, choose(I, 2)), 
                                DF = rep(ddfRRFC, choose(I, 2)), 
                                t = tStat, 
                                PrGTt = PrGTt, 
                                CILower = CIRRFC[,1],
                                CIUpper = CIRRFC[,2])
    
    dfSingleRRFC <- array(dim = I)
    msDenSingleRRFC <- array(dim = I)
    stdErrSingleRRFC <- array(dim = I)
    CISingleRRFC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleRRFC[i] <- msRSingle[i]
      dfSingleRRFC[i] <- (J - 1)
      stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J)
      CISingleRRFC[i, ] <- sort(c(trtMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trtMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
    }
    ciAvgRdrEachTrtRRFC <- data.frame(Treatment = paste("Trt", modalityID, sep = ""), 
                                      Area = trtMeans, 
                                      StdErr = as.vector(stdErrSingleRRFC), 
                                      DF = as.vector(dfSingleRRFC), 
                                      CILower = CISingleRRFC[,1], 
                                      CIUpper = CISingleRRFC[,2], 
                                      row.names = NULL)
    
    if (option == "RRFC"){
      return(data.frame(fomArray = fomArray, 
                        meanSquares = meanSquares, 
                        varComp = varComp,
                        FTestStatsRRFC = FTestStatsRRFC, 
                        ciDiffTrtRRFC = ciDiffTrtRRFC, 
                        ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC
      ))
    }
  }
  
  return(list(
    fomArray = fomArray, 
    meanSquares = meanSquares, 
    varComp = varComp, 
    FTestStatsRRRC = FTestStatsRRRC, 
    ciDiffTrtRRRC = ciDiffTrtRRRC, 
    ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC,
    FTestStatsFRRC = FTestStatsFRRC,
    ciDiffTrtFRRC = ciDiffTrtFRRC, 
    ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
    ciDiffTrtEachRdrFRRC = ciDiffTrtEachRdrFRRC, 
    varCovEachRdr = varCovEachRdr, 
    FTestStatsRRFC = FTestStatsRRFC, 
    ciDiffTrtRRFC = ciDiffTrtRRFC, 
    ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC
  ))
} 

