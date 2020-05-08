StORHAnalysis <- function(dataset, FOM, FPFValue, alpha = 0.05, covEstMethod = "jackknife", 
                          nBoots = 200, analysisOption = "ALL")  
{
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  
  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  trtMeans <- rowMeans(foms)
  trtMeans <- as.data.frame(trtMeans)
  colnames(trtMeans) <- "AvgFoms"
  
  trtMeanDiffs <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
      diffTRName[ii] <- paste0("Trt", modalityID[i], sep = "-", "Trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("Estimate" = trtMeanDiffs, 
                             stringsAsFactors = FALSE)
  rownames(trtMeanDiffs) <- diffTRName
  
  FOMs <- list(
    foms = foms,
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs
  )
  
  ret <- UtilVarComponentsOR(dataset, FOM, FPFValue, covEstMethod, nBoots)
  
  mSquaresOR <- ret$meanSquares
  ORVarComp <-  ret$varComp#;rownames(ORVarComp) <- "Estimates"
  msREachTrt <- ret$msREachTrt
  rhoOR <- ret$rhoOR

  ANOVA <- list()
  ANOVA$ORVarComp <- ORVarComp
  
  Cov1 <- ORVarComp["Cov1",1]
  Cov2 <- ORVarComp["Cov2",1]
  Cov3 <- ORVarComp["Cov3",1]
  Var <- ORVarComp["Var",1]
  
  msT <- mSquaresOR["msT",1]
  msR <- mSquaresOR["msR",1]
  msTR <- mSquaresOR["msTR",1]
  
  msArray <- c(msT, msR, msTR)
  dfArray <- c(I - 1, J - 1, (I - 1) * (J - 1))
  ssArray <- msArray * dfArray
  
  # ===========================================================================
  # *****            ANOVA Tables (OR analysis of reader AUCs)            *****
  # ===========================================================================
  TRanovaOR <- data.frame("SS" = ssArray, 
                          "DF" = dfArray, 
                          "MS" = msArray,
                          stringsAsFactors = FALSE)  
  rownames(TRanovaOR) <- c("T", "R", "TR")
  
  # if (TRUE || (length(dataset) != 12) || (dataset$design == "CROSSED")) {
  if (J > 0) {
    if ((length(dataset) != 12) || (dataset$design == "CROSSED")) {
      # oldFormat or new format CROSSED dataset
      varEachTrt <- vector(length = I)
      cov2EachTrt <- vector(length = I)
      for (i in 1:I) {
        fomSingle <- foms[i, ]
        dim(fomSingle) <- c(1, J)
        dsi <- DfExtractDataset(dataset, trts = i)
        # with a single treatment it is not possible to calculate Cov1
        # therefore the following line will fail for SPLIT-PLOT 
        # dataset; hence we skip it; see else block below
        ret <- gpfEstimateVarCov(dsi, FOM, FPFValue, nBoots, covEstMethod)
        varEachTrt[i] <- ret$Var
        cov2EachTrt[i] <- ret$Cov2
      } 
      
      dfSingleFRRC <- array(dim = I)
      msDenSingleFRRC <- array(dim = I)
      stdErrSingleFRRC <- array(dim = I)
      CISingleFRRC <- array(dim = c(I, 2))
      for (i in 1:I) {
        if (J > 1) msDenSingleFRRC[i] <- varEachTrt[i] + (J - 1) * cov2EachTrt[i] else msDenSingleFRRC[i] <- varEachTrt[i]
        dfSingleFRRC[i] <- Inf
        stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J)
        CISingleFRRC[i, ] <- c(trtMeans[i,1] + qt(alpha/2,   dfSingleFRRC[i]) * stdErrSingleFRRC[i], 
                               trtMeans[i,1] + qt(1-alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i])
      }
      ciAvgRdrEachTrtFRRC <- data.frame(Treatment = paste0("Trt", modalityID), 
                                        Area = trtMeans, 
                                        StdErr = as.vector(stdErrSingleFRRC), 
                                        DF = as.vector(dfSingleFRRC), 
                                        CILower = CISingleFRRC[,1], 
                                        CIUpper = CISingleFRRC[,2], 
                                        row.names = NULL, 
                                        stringsAsFactors = FALSE)
      
    } else {
      # NewFormat and SPLIT-PLOT dataset
      cov2EachTrt = rep(0, I)
      ciAvgRdrEachTrtFRRC <- NA
    }
    VarEachRdr <- vector(length = J)
    Cov1EachRdr <- vector(length = J)
    for (j in 1:J) {
      fomSingle <- foms[, j]
      dim(fomSingle) <- c(I, 1)
      dsj <- DfExtractDataset(dataset, rdrs = j)
      ret <- gpfEstimateVarCov(dsj, FOM, FPFValue, nBoots, covEstMethod)
      VarEachRdr[j] <- ret$Var
      Cov1EachRdr[j] <- ret$Cov1
    }
  }
  
  msRSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    msRSingle[i] <- sum((foms[i, ] - trtMeans[i,1])^2)/(J - 1)
  }
  
  # ************ RRRC ****************
  # ************ RRRC ****************
  # ************ RRRC ****************
  if (analysisOption %in% c("RRRC", "ALL")) {
    msDenRRRC <- mSquaresOR$msTR + max(J * (Cov2 - Cov3), 0)
    fRRRC <- mSquaresOR$msT/msDenRRRC
    ddfRRRC <- msDenRRRC^2/((mSquaresOR$msTR)^2/((I - 1) * (J - 1)))
    pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
    RRRC <- list()
    RRRC$FTests <- data.frame(f = fRRRC,
                              ndf = (I-1),
                              ddf = ddfRRRC,
                              p = pRRRC,
                              stringsAsFactors = FALSE)
    stdErrRRRC <- sqrt(2 * msDenRRRC/J)
    tStat <- vector()
    PrGTt <- vector()
    CIRRRC <- array(dim = c(length(trtMeanDiffs[,1]), 2))
    for (i in 1:length(trtMeanDiffs[,1])) {
      tStat[i] <- trtMeanDiffs[i,1]/stdErrRRRC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRRC, lower.tail = FALSE) # critical correction, noted by user Lucy D'Agostino McGowan
      ci <- sort(c(trtMeanDiffs[i,1] - qt(alpha/2, ddfRRRC) * stdErrRRRC, trtMeanDiffs[i,1] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
      if (length(ci) == 0){
        CIRRRC[i, ] <- c(NA, NA)
      }else{
        CIRRRC[i, ] <- ci
      }
    }
    RRRC$ciDiffTrt <- data.frame(Treatment = diffTRName, 
                                 Estimate = trtMeanDiffs, 
                                 StdErr = rep(stdErrRRRC, choose(I, 2)), 
                                 DF = rep(ddfRRRC, choose(I, 2)), 
                                 t = tStat, 
                                 PrGTt = PrGTt, 
                                 CILower = CIRRRC[,1],
                                 CIUpper = CIRRRC[,2], 
                                 stringsAsFactors = FALSE)
    
    dfSingleRRRC <- array(dim = I)
    msDenSingleRRRC <- array(dim = I)
    stdErrSingleRRRC <- array(dim = I)
    CISingleRRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleRRRC[i] <- msRSingle[i] + max(J * cov2EachTrt[i], 0)
      dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
      stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J)
      ci <- sort(c(trtMeans[i,1] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trtMeans[i,1] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
      if (length(ci) == 0){
        CISingleRRRC[i, ] <- c(NA, NA)
      }else{
        CISingleRRRC[i, ] <- ci
      }
      
    }
    RRRC$ciAvgRdrEachTrt <- data.frame(Treatment = paste("Trt", modalityID, sep = ""), 
                                       Area = trtMeans, 
                                       StdErr = as.vector(stdErrSingleRRRC), 
                                       DF = as.vector(dfSingleRRRC), 
                                       CILower = CISingleRRRC[,1], 
                                       CIUpper = CISingleRRRC[,2], 
                                       row.names = NULL,
                                       stringsAsFactors = FALSE)
   #### RRRC list completed 
    if (analysisOption == "RRRC"){
      return(list(
        FOMs = FOMs,
        FVCA = list(
          TRanovaOR = TRanovaOR,
          mSquaresOR = mSquaresOR, 
          ORVarComp = ORVarComp,
          rhoOR = rhoOR,
          msREachTrt = msREachTrt
        ),
        RRRC = RRRC
      ))
    }
  }
  
  # ************ FRRC ****************
  # ************ FRRC ****************
  # ************ FRRC ****************
  if (analysisOption %in% c("FRRC", "ALL")) {
    if (J > 1) msDenFRRC <- Var - Cov1 + (J - 1) * (Cov2 - Cov3) else msDenFRRC <- Var - Cov1
    fFRRC <- mSquaresOR$msT/msDenFRRC
    ddfFRRC <- Inf
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    FRRC <- list()
    FRRC$FTests <- data.frame(f = fFRRC,
                              ndf = (I-1),
                              ddf = ddfFRRC,
                              p = pFRRC,
                              stringsAsFactors = FALSE)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J)
    tStat <- vector()
    PrGTt <- vector()
    CIFRRC <- CIRRRC * 0
    for (i in 1:length(trtMeanDiffs)) {
      tStat[i] <- trtMeanDiffs[i,1]/stdErrFRRC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfFRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIFRRC[i, ] <- c(trtMeanDiffs[i,1] + qt(alpha/2, ddfFRRC) * stdErrFRRC, trtMeanDiffs[i,1] 
                       + qt(1-alpha/2, ddfFRRC) * stdErrFRRC)
    }
    FRRC$ciDiffTrt <- data.frame(Treatment = diffTRName, 
                                 Estimate = trtMeanDiffs, 
                                 StdErr = rep(stdErrFRRC, choose(I, 2)),
                                 DF = rep(ddfFRRC, choose(I, 2)), 
                                 t = tStat, 
                                 PrGTt = PrGTt, 
                                 CILower = CIFRRC[,1],
                                 CIUpper = CIFRRC[,2], 
                                 stringsAsFactors = FALSE)
    
    FRRC$ciAvgRdrEachTrt <- ciAvgRdrEachTrtFRRC # this was calculated above 4/29/20
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
    
    if (J > 1) {
      diffTRMeansFRRC <- as.vector(t(diffTRMeansFRRC))
      stdErrFRRC <- sqrt(2 * (VarEachRdr - Cov1EachRdr))
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
      FRRC$ciDiffTrtEachRdr <- data.frame(Reader = paste("Rdr", readerNames, sep = ""), 
                                          Treatment = trNames, 
                                          Estimate = diffTRMeansFRRC, 
                                          StdErr = stdErrFRRC, 
                                          DF = dfReaderFRRC, 
                                          t = tStat, 
                                          PrGTt = PrGTt, 
                                          CILower = CIReaderFRRC[,1],
                                          CIUpper = CIReaderFRRC[,2],
                                          stringsAsFactors = FALSE)
      
      FRRC$varCovEachRdr <- data.frame(Reader = paste("Rdr", readerID, sep = ""),
                                       Var = VarEachRdr, 
                                       Cov1 = Cov1EachRdr, 
                                       stringsAsFactors = FALSE)
    }
    if (analysisOption == "FRRC"){
      if (J > 1) {
        return(list(
          FOMs = FOMs,
          FVCA = list(
            TRanovaOR = TRanovaOR,
            mSquaresOR = mSquaresOR, 
            ORVarComp = ORVarComp,
            rhoOR = rhoOR,
            msREachTrt = msREachTrt
          ),
          RRRC = NULL,
          FRRC = list(
            FTests = FRRC$FTests,
            ciDiffTrt = FRRC$ciDiffTrt,
            ciDiffTrtEachRdr = FRRC$ciDiffTrtEachRdr,
            ciAvgRdrEachTrt = FRRC$ciAvgRdrEachTrt,
            varCovEachRdr = FRRC$varCovEachRdr
          ),
          RRFC = NULL
        ))
      } else {
        # needs debugging here, i.e., cross-check
        return(list(
          FOMs = FOMs,
          TRanovaOR = TRanovaOR,
          msT = mSquaresOR$msT, 
          ORVarComp = data.frame(Cov1 = ORVarComp$Cov1, 
                                 Var = ORVarComp$Var, 
                                 stringsAsFactors = FALSE),
          FTestsFRRC = FRRC$FTests,
          ciDiffTrtFRRC = FRRC$ciDiffTrt 
        ))
      }
    }
  }
  
  # ************ RRFC ****************
  # ************ RRFC ****************
  # ************ RRFC ****************
  if (analysisOption %in% c("RRFC", "ALL")) {
    # since Cov2 and Cov3 are zeroes for split-plot, FTestsRRFC will be 
    # identical to FTestsRRRC; other values below may differ
    # not sure about what is going one here; I am proceeding on assumption that
    # the only difference is setting Cov2 = Cov3 = 0, and reusing code from crossed
    # analysis
    msDenRRFC <- mSquaresOR$msTR
    fRRFC <- mSquaresOR$msT/msDenRRFC
    ddfRRFC <- ((I - 1) * (J - 1))
    pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
    RRFC <- list()
    RRFC$FTests <- data.frame(f = fRRFC,
                              ndf = (I-1),
                              ddf = ddfRRFC,
                              p = pRRFC, 
                              stringsAsFactors = FALSE)
    stdErrRRFC <- sqrt(2 * msDenRRFC/J)
    tStat <- vector()
    PrGTt <- vector()
    CIRRFC <- CIRRRC
    for (i in 1:length(trtMeanDiffs)) {
      tStat[i] <- trtMeanDiffs[i,1]/stdErrRRFC
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRFC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIRRFC[i, ] <- sort(c(trtMeanDiffs[i,1] - qt(alpha/2, ddfRRFC) * stdErrRRFC, trtMeanDiffs[i,1] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
    }
    RRFC$ciDiffTrt <- data.frame(Treatment = diffTRName, 
                                 Estimate = trtMeanDiffs, 
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
      stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J)
      CISingleRRFC[i, ] <- sort(c(trtMeans[i,1] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trtMeans[i,1] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
    }
    RRFC$ciAvgRdrEachTrt <- data.frame(Treatment = paste("Trt", modalityID, sep = ""), 
                                       Area = trtMeans, 
                                       StdErr = as.vector(stdErrSingleRRFC), 
                                       DF = as.vector(dfSingleRRFC), 
                                       CILower = CISingleRRFC[,1], 
                                       CIUpper = CISingleRRFC[,2], 
                                       row.names = NULL, 
                                       stringsAsFactors = FALSE)
    
    if (analysisOption == "RRFC"){
      return(list(
        FOMs = FOMs,
        FVCA = list(
          TRanovaOR = TRanovaOR,
          mSquaresOR = mSquaresOR, 
          ORVarComp = ORVarComp,
          rhoOR = rhoOR,
          msREachTrt = msREachTrt
        ),
        RRRC = NULL,
        FRRC = NULL,  
        RRFC = list(
          FTests = RRFC$FTests,
          ciDiffTrt = RRFC$ciDiffTrt,
          ciAvgRdrEachTrt = RRFC$ciAvgRdrEachTrt
        )
      ))
    }
  }
  
  return(list(
    FOMs = FOMs,
    FVCA = list(
      TRanovaOR = TRanovaOR,
      mSquaresOR = mSquaresOR, 
      ORVarComp = ORVarComp,
      rhoOR = rhoOR,
      msREachTrt = msREachTrt
    ),
    RRRC = list(
      FTests = RRRC$FTests,
      ciDiffTrt = RRRC$ciDiffTrt,
      ciAvgRdrEachTrt = RRRC$ciAvgRdrEachTrt
    ),
    FRRC = list(
      FTests = FRRC$FTests,
      ciDiffTrt = FRRC$ciDiffTrt,
      ciAvgRdrEachTrt = FRRC$ciAvgRdrEachTrt,
      ciDiffTrtEachRdr = FRRC$ciDiffTrtEachRdr,
      varCovEachRdr = FRRC$varCovEachRdr
    ),
    RRFC = list(
      FTests = RRFC$FTests,
      ciDiffTrt = RRFC$ciDiffTrt,
      ciAvgRdrEachTrt = RRFC$ciAvgRdrEachTrt
    )
  ))
} 

