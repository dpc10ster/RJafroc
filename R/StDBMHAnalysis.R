StDBMHAnalysis <- function(dataset, FOM, FPFValue, alpha, option) 
{
  NL <- dataset$NL
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  
  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  trtMeans <- rowMeans(foms) 
  
  trtMeanDiffs <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i] - trtMeans[ip]
      diffTRName[ii] <- paste0("Trt", modalityID[i], sep = "-", "Trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("TrtDiff" = diffTRName, "Estimate" = trtMeanDiffs) 
  
  ret <- UtilVarComponentsDBM(dataset, FOM, FPFValue)
  mSquares <- ret$mSquares
  varComp <- ret$varComp
  psVals <- ret$psVals
  
  msT <- mSquares$msT
  msR <- mSquares$msR
  msC <- mSquares$msC
  msTR <- mSquares$msTR
  msTC <- mSquares$msTC
  msRC <- mSquares$msRC
  msTRC <- mSquares$msTRC
  
  msArray <- c(msT, msR, msC, msTR, msTC, msRC, msTRC)
  dfArray <- c(I - 1, J - 1, K - 1, (I - 1) * (J - 1), (I - 1) * (K - 1), (J - 1) * (K - 1), (I - 1) * (J - 1) * (K - 1))
  ssArray <- msArray * dfArray
  msArray <- c(msArray, NA)
  dfArray <- c(dfArray, sum(dfArray))
  ssArray <- c(ssArray, sum(ssArray))
  if (FALSE) { ## begin obsolete code
    # This was the source of the problems I was having in test-St.R, where I had
    # to resort to skipping the test on mac for context("SignificanceTestingAllCombinations")
    # Here is the fix
    # original code:
    # sourceArray <- c("T", "R", "C", "TR", "TC", "RC", "TRC", "Total")
    # TRCanovaY <- data.frame(Source = sourceArray, 
    #                      SS = ssArray, 
    #                      DF = dfArray, 
    #                      MS = msArray, 
    #                      stringsAsFactors = FALSE)
    #
    # this is the fix 3/7/20
    # New code:
    # use attributes(TRCanovaY$Source) to examine ordering of levels to confirm it is correct
    # data.frame is reordering the levels alphabetically!
    # the folloiwng trick forces the correct order to be maintained
    # sourceArray <- c("Row1_T", "Row2_R", "Row3_C", "Row4_TR", 
    #                  "Row5_TC", "Row6_RC", "Row7_TRC", "Row8_Total")
    sourceArray <- c("T", "R", "C", "TR", "TC", "RC", "TRC", "Total")
    # forcing original order to be kept
    for (i in (1:length(sourceArray))) {
      sourceArray[i] <- paste0(paste0("Row",i,"_"),sourceArray[i])
    }
    TRCanovaY <- data.frame(Source = sourceArray, 
                            SS = ssArray, 
                            DF = dfArray, 
                            MS = msArray,
                            stringsAsFactors = FALSE)
    
    # print(attributes(TRCanovaY))
    # print(attributes(TRCanovaY$Source))
    # print(attributes(TRCanovaY$SS))
  } ## End obsolete code
  
  # 5/1/20: finally learned how to code a dataframe
  # Above comments obsolete
  TRCanovaY <- data.frame("Source" = c("T", "R", "C", "TR", "TC", "RC", "TRC", "Total"), 
                          "SS" = ssArray, 
                          "DF" = dfArray, 
                          "MS" = msArray)  
  
  msRSingle <- array(0, dim = c(I))
  msCSingle <- array(0, dim = c(I))
  msRCSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    for (j in 1:J) {
      msRSingle[i] <- msRSingle[i] + (mean(psVals[i, j, ]) - mean(psVals[i, , ]))^2
    }
    msRSingle[i] <- msRSingle[i] * K/(J - 1)
    
    for (k in 1:K) {
      msCSingle[i] <- msCSingle[i] + (mean(psVals[i, , k]) - mean(psVals[i, , ]))^2
    }
    msCSingle[i] <- msCSingle[i] * J/(K - 1)
    
    for (j in 1:J) {
      for (k in 1:K) {
        msRCSingle[i] <- msRCSingle[i] + (mean(psVals[i, j, k]) - mean(psVals[i, j, ]) - mean(psVals[i, , k]) + mean(psVals[i, , ]))^2
      }
    }
    msRCSingle[i] <- msRCSingle[i]/((J - 1) * (K - 1))
  }
  sourceArraySingle <- c("R", "C", "RC")
  dfArraySingle <- c(J - 1, K - 1, (J - 1) * (K - 1))
  msArraySingle <- t(cbind(msRSingle, msCSingle, msRCSingle))
  RCanovaYi <- data.frame(sourceArraySingle, 
                          dfArraySingle, 
                          msArraySingle, 
                          row.names = NULL, 
                          stringsAsFactors = FALSE)
  colnames(RCanovaYi) <- c("Source", "DF", paste0("Trt", sep = "", modalityID))
  
  diffTRMeans <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRMeans[ii] <- trtMeans[i] - trtMeans[ip]
      diffTRName[ii] <- paste0("Trt", modalityID[i], sep = "-", "Trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  
  msNum <- msT
  
  if (option %in% c("RRRC", "ALL")) {
    # ************ RRRC ****************
    # ************ RRRC ****************
    # ************ RRRC ****************
    msDenRRRC <- msTR + max(msTC - msTRC, 0)
    fRRRC <- msNum/msDenRRRC
    ddfRRRC <- msDenRRRC^2/(msTR^2/((I - 1) * (J - 1)))
    pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
    RRRC <- list()
    RRRC$FTests <- data.frame(f = fRRRC,
                              ndf = (I-1),
                              ddf = ddfRRRC,
                              p = pRRRC)
    stdErrRRRC <- sqrt(2 * msDenRRRC/J/K)
    tStat <- vector()
    PrGTt <- vector()
    CIRRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrRRRC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIRRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRRC) * stdErrRRRC, diffTRMeans[i] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
      
    }
    RRRC$ciDiffTrt <- data.frame(TrtDiff = diffTRName, 
                                 Estimate = diffTRMeans, 
                                 StdErr = rep(stdErrRRRC, choose(I, 2)), 
                                 DF = rep(ddfRRRC, choose(I, 2)), 
                                 t = tStat, 
                                 PrGTt = PrGTt, # renamed this consistently
                                 CILower = CIRRRC[,1],  # instead of adding CIRRC and then using a names() to split out the two values
                                 CIUpper = CIRRRC[,2],  # do:
                                 stringsAsFactors = FALSE)
    dfSingleRRRC <- array(dim = I)
    msDenSingleRRRC <- array(dim = I)
    stdErrSingleRRRC <- array(dim = I)
    CISingleRRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleRRRC[i] <- msRSingle[i] + max(msCSingle[i] - msRCSingle[i], 0)
      dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
      stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J/K)
      ciTemp <- sort(c(trtMeans[i] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trtMeans[i] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
      if (length(ciTemp) == 2) CISingleRRRC[i, ] <- ciTemp
      
    }
    RRRC$CISingle <- CISingleRRRC
    RRRC$ciAvgRdrEachTrt <- data.frame(Treatment = paste("Trt", modalityID, sep = ""), 
                                       Area = trtMeans, 
                                       StdErr = as.vector(stdErrSingleRRRC), # this was the critical fix, Peter
                                       DF = as.vector(dfSingleRRRC),  # this was the critical fix, Peter
                                       CILower = CISingleRRRC[,1], 
                                       CIUpper = CISingleRRRC[,2], 
                                       row.names = NULL, 
                                       stringsAsFactors = FALSE)
    # 5/4/20 removing all this as I better understand data.frame()
    if (option == "RRRC")
      return(list(
        foms = t(foms),
        # return transpose to match official code 
        # and it makes more sense to have readers in vertical direction 5/1/20
        trtMeans = trtMeans,
        trtMeanDiffs = trtMeanDiffs,
        TRCanovaY = TRCanovaY, 
        RCanovaYi = RCanovaYi, 
        varComp = varComp,
        RRRC = list (
          FTests = RRRC$FTests, 
          ciDiffTrt = RRRC$ciDiffTrt, 
          ciAvgRdrEachTrt = RRRC$ciAvgRdrEachTrt,
          CISingle = RRRC$CISingle
        ),
        FRRC = NULL,
        RRFC = NULL
      ))
  }
  
  if (option %in% c("FRRC", "ALL")) {
    # ************ FRRC ****************
    # ************ FRRC ****************
    # ************ FRRC ****************
    msDenFRRC <- msTC
    fFRRC <- msNum/msDenFRRC
    ddfFRRC <- (I - 1) * (K - 1)
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    FRRC <- list()
    FRRC$FTests <- data.frame(f = fFRRC,
                              ndf = (I-1),
                              ddf = ddfFRRC,
                              p = pFRRC)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J/K)
    tStat <- vector()
    PrGTt <- vector()
    CIFRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrFRRC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfFRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIFRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfFRRC) * stdErrFRRC, diffTRMeans[i] + qt(alpha/2, ddfFRRC) * stdErrFRRC))
    }
    FRRC$ciDiffTrt <- data.frame(Treatment = diffTRName, 
                                 Estimate = diffTRMeans, 
                                 StdErr = rep(stdErrFRRC, choose(I, 2)), 
                                 DF = rep(ddfFRRC, choose(I, 2)), 
                                 t = tStat, 
                                 PrGTt = PrGTt, 
                                 CILower = CIFRRC[,1], 
                                 CIUpper = CIFRRC[,2], 
                                 stringsAsFactors = FALSE)
    
    dfSingleFRRC <- array(dim = I)
    msDenSingleFRRC <- array(dim = I)
    stdErrSingleFRRC <- array(dim = I)
    CISingleFRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleFRRC[i] <- msCSingle[i]
      dfSingleFRRC[i] <- (K - 1)
      stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J/K)
      CISingleFRRC[i, ] <- sort(c(trtMeans[i] - qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i], trtMeans[i] + qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i]))
    }
    FRRC$ciAvgRdrEachTrt <- data.frame(Treatment = paste0("Trt", modalityID), 
                                       Area = trtMeans, 
                                       StdErr = as.vector(stdErrSingleFRRC), 
                                       DF = as.vector(dfSingleFRRC), 
                                       CILower = CISingleFRRC[,1], 
                                       CIUpper = CISingleFRRC[,2], 
                                       row.names = NULL, 
                                       stringsAsFactors = FALSE)
    ssTFRRC <- array(0, dim = c(J))
    ssCFRRC <- array(0, dim = c(J))
    ssTCFRRC <- array(0, dim = c(J))
    for (j in 1:J) {
      for (i in 1:I) {
        ssTFRRC[j] <- ssTFRRC[j] + (mean(psVals[i, j, ]) - mean(psVals[, j, ]))^2
      }
      ssTFRRC[j] <- ssTFRRC[j] * K
      
      for (k in 1:K) {
        ssCFRRC[j] <- ssCFRRC[j] + (mean(psVals[, j, k]) - mean(psVals[, j, ]))^2
      }
      ssCFRRC[j] <- ssCFRRC[j] * I
      
      for (i in 1:I) {
        for (k in 1:K) {
          ssTCFRRC[j] <- ssTCFRRC[j] + (mean(psVals[i, j, k]) - mean(psVals[i, j, ]) - mean(psVals[, j, k]) + mean(psVals[, j, ]))^2
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
    msAnovaEachRdrFRRC <- data.frame(sourceArrayFRRC, 
                                     dfArrayFRRC, 
                                     msArrayFRRC, 
                                     row.names = NULL, 
                                     stringsAsFactors = FALSE)
    colnames(msAnovaEachRdrFRRC) <- c("Source", "DF", paste0("Rdr", sep = "", readerID))
    FRRC$msAnovaEachRdr <- msAnovaEachRdrFRRC
    
    diffTRMeansFRRC <- array(dim = c(J, choose(I, 2)))
    for (j in 1:J) {
      ii <- 1
      for (i in 1:I) {
        if (i == I) 
          break
        for (ip in (i + 1):I) {
          diffTRMeansFRRC[j, ii] <- foms[i, j] - foms[ip, j]
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
    trDiffNames <- rep(diffTRName, J)
    dfReaderFRRC <- rep(K - 1, length(stdErrFRRC))
    CIReaderFRRC <- array(dim = c(length(stdErrFRRC), 2))
    tStat <- vector()
    PrGTt <- vector()
    for (n in 1:length(stdErrFRRC)) {
      tStat[n] <- diffTRMeansFRRC[n]/stdErrFRRC[n]
      PrGTt[n] <- 2 * pt(abs(tStat[n]), dfReaderFRRC[n], lower.tail = FALSE)
      CIReaderFRRC[n, ] <- sort(c(diffTRMeansFRRC[n] - qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n], diffTRMeansFRRC[n] + qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n]))
    }
    FRRC$ciDiffTrtEachRdr <- data.frame(Reader = paste("Rdr", readerNames, sep = ""), 
                                        Treatment = paste(trDiffNames, sep = ""), 
                                        Estimate = diffTRMeansFRRC, 
                                        StdErr = stdErrFRRC, 
                                        DF = dfReaderFRRC, 
                                        t = tStat, 
                                        PrGTt = PrGTt, 
                                        CILower = CIReaderFRRC[,1],
                                        CIUpper = CIReaderFRRC[,2], 
                                        stringsAsFactors = FALSE)
    if (option == "FRRC")
      return(list(
        foms = t(foms),
        # return transpose to match official code 
        # and it makes more sense to have readers in vertical direction 5/1/20
        trtMeans = trtMeans,
        trtMeanDiffs = trtMeanDiffs,
        TRCanovaY = TRCanovaY, 
        RCanovaYi = RCanovaYi, 
        varComp = varComp,
        RRRC = NULL,
        FRRC = list (
          FTests = FRRC$FTests, 
          ciDiffTrtFRRC = FRRC$ciDiffTrt, 
          ciAvgRdrEachTrt = FRRC$ciAvgRdrEachTrt, 
          msAnovaEachRdr = FRRC$msAnovaEachRdr, 
          ciDiffTrtEachRdr = FRRC$ciDiffTrtEachRdr
        ),
        RRFC = NULL
      ))
  }
  
  if (option %in% c("RRFC", "ALL")) {
    # ************ RRFC ****************
    # ************ RRFC ****************
    # ************ RRFC ****************
    msDenRRFC <- msTR
    fRRFC <- msNum/msDenRRFC
    ddfRRFC <- ((I - 1) * (J - 1))
    pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
    RRFC <- list()
    RRFC$FTests <- data.frame(f = fRRFC,
                              ndf = (I-1),
                              ddf = ddfRRFC,
                              p = pRRFC)
    stdErrRRFC <- sqrt(2 * msDenRRFC/J/K)
    tStat <- vector()
    PrGTt <- vector()
    CIRRFC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrRRFC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRFC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
    }
    RRFC$ciDiffTrt <- data.frame(Treatment = diffTRName, 
                                 Estimate = diffTRMeans, 
                                 StdErr = rep(stdErrRRFC, choose(I, 2)), 
                                 DF = rep(ddfRRFC, choose(I, 2)), 
                                 t = tStat, 
                                 PrGTt = PrGTt, 
                                 CILower = CIRRFC[,1],
                                 CIUpper = CIRRFC[,2], 
                                 stringsAsFactors = FALSE)
    dfSingleRRFC <- array(dim = I)
    msDenSingleRRFC <- array(dim = I)
    stdErrSingleRRFC <- array(dim = I)
    CISingleRRFC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleRRFC[i] <- msRSingle[i]
      dfSingleRRFC[i] <- (J - 1)
      stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J/K)
      CISingleRRFC[i, ] <- sort(c(trtMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trtMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
    }
    RRFC$ciAvgRdrEachTrt <- data.frame(Treatment = paste("Trt", modalityID, sep = ""), 
                                       Area = trtMeans, 
                                       StdErr = as.vector(stdErrSingleRRFC), 
                                       DF = as.vector(dfSingleRRFC), 
                                       CILower = CISingleRRFC[,1], 
                                       CIUpper = CISingleRRFC[,2], 
                                       row.names = NULL, 
                                       stringsAsFactors = FALSE)

    if (option == "RRFC")
      return(list(
        foms = t(foms),
        # return transpose to match official code 
        # and it makes more sense to have readers in vertical direction 5/1/20
        trtMeans = trtMeans,
        trtMeanDiffs = trtMeanDiffs,
        TRCanovaY = TRCanovaY, 
        RCanovaYi = RCanovaYi, 
        varComp = varComp,
        RRRC = NULL,
        FRRC = NULL,
        RRFC = list(
          FTests = RRFC$FTests, 
          ciDiffTrt = RRFC$ciDiffTrt, 
          ciAvgRdrEachTrt = RRFC$ciAvgRdrEachTrt
        )
      ))
  }
  
  return(list(
    foms = t(foms),
    # return transpose to match official code 
    # and it makes more sense to have readers in vertical direction 5/1/20
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs,
    TRCanovaY = TRCanovaY, 
    RCanovaYi = RCanovaYi, 
    varComp = varComp,
    RRRC = list(
      FTests = RRRC$FTests, 
      ciDiffTrt = RRRC$ciDiffTrt, 
      ciAvgRdrEachTrtRRRC = RRRC$ciAvgRdrEachTrt
    ),
    FRRC = list(
      FTests = FRRC$FTests, 
      ciDiffTrt = FRRC$ciDiffTrt, 
      ciAvgRdrEachTrtRRRC = FRRC$ciAvgRdrEachTrt,
      msAnovaEachRdr = FRRC$msAnovaEachRdr,
      ciDiffTrtEachRdr = FRRC$ciDiffTrtEachRdr
    ),
    RRFC = list(
      FTests = RRFC$FTests, 
      ciDiffTrt = RRFC$ciDiffTrt, 
      ciAvgRdrEachTrt = RRFC$ciAvgRdrEachTrt
    )
  ))
} 



