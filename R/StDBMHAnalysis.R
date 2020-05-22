StDBMHAnalysis <- function(dataset, FOM, FPFValue, alpha, analysisOption) 
{
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  I <- dim(dataset$NL)[1]
  
  modalityID <- dataset$modalityID
  # readerID <- dataset$readerID

  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  ret <- UtilVarComponentsDBM(dataset, FOM, FPFValue)
  
  # foms <- ret$foms
  VarCom <- ret$VarCom
  TRCanova <- ret$TRCanova
  IndividualTrt <- ret$IndividualTrt
  IndividualRdr <- ret$IndividualRdr

  ANOVA <- list()
  ANOVA$TRCanova <- TRCanova
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
      trtMeanDiffs[ii] <- trtMeans[i,"Estimate"] - trtMeans[ip,"Estimate"]
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
    RRRC <- DBMSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- DBMSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- DBMSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- DBMSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- DBMSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- DBMSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
} 
  # ANOVA <- list()
  # ANOVA$TRCanova <- TRCanova
  # ANOVA$VarCom <- VarCom
  # ANOVA$IndividualTrt <- IndividualTrt
  # ANOVA$IndividualRdr <- IndividualRdr
  # 
  # diffTRMeans <- array(dim = choose(I, 2))
  # diffTRName <- array(dim = choose(I, 2))
  # ii <- 1
  # for (i in 1:I) {
  #   if (i == I) 
  #     break
  #   for (ip in (i + 1):I) {
  #     diffTRMeans[ii] <- trtMeans[i] - trtMeans[ip]
  #     diffTRName[ii] <- paste0("Trt", modalityID[i], sep = "-", "Trt", modalityID[ip]) # !sic
  #     ii <- ii + 1
  #   }
  # }
  # 
  # msNum <- msT
  # 
  # 
  # if (analysisOption %in% c("FRRC", "ALL")) {
    # ************ FRRC ****************
    # ************ FRRC ****************
    # ************ FRRC ****************
    
  #   if (analysisOption %in% c("RRFC", "ALL")) {
  #     # ************ RRFC ****************
  #     # ************ RRFC ****************
  #     # ************ RRFC ****************
  #     msDenRRFC <- msTR
  #     fRRFC <- msNum/msDenRRFC
  #     ddfRRFC <- ((I - 1) * (J - 1))
  #     pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
  #     RRFC <- list()
  #     RRFC$FTests <- data.frame(f = fRRFC,
  #                               ndf = (I-1),
  #                               ddf = ddfRRFC,
  #                               p = pRRFC, 
  #                               stringsAsFactors = FALSE)
  #     stdErrRRFC <- sqrt(2 * msDenRRFC/J/K)
  #     tStat <- vector()
  #     PrGTt <- vector()
  #     CIRRFC <- array(dim = c(length(diffTRMeans), 2))
  #     for (i in 1:length(diffTRMeans)) {
  #       tStat[i] <- diffTRMeans[i]/stdErrRRFC
  #       PrGTt[i] <- 2 * pt(abs(tStat[i]), ddfRRFC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
  #       CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
  #     }
  #     RRFC$ciDiffTrt <- data.frame(Treatment = diffTRName, 
  #                                  Estimate = diffTRMeans, 
  #                                  StdErr = rep(stdErrRRFC, choose(I, 2)), 
  #                                  DF = rep(ddfRRFC, choose(I, 2)), 
  #                                  t = tStat, 
  #                                  PrGTt = PrGTt, 
  #                                  CILower = CIRRFC[,1],
  #                                  CIUpper = CIRRFC[,2], 
  #                                  stringsAsFactors = FALSE)
  #     dfSingleRRFC <- array(dim = I)
  #     msDenSingleRRFC <- array(dim = I)
  #     stdErrSingleRRFC <- array(dim = I)
  #     CISingleRRFC <- array(dim = c(I, 2))
  #     for (i in 1:I) {
  #       msDenSingleRRFC[i] <- msRSingle[i]
  #       dfSingleRRFC[i] <- (J - 1)
  #       stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J/K)
  #       CISingleRRFC[i, ] <- sort(c(trtMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trtMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
  #     }
  #     RRFC$ciAvgRdrEachTrt <- data.frame(Treatment = paste("Trt", modalityID, sep = ""), 
  #                                        Area = trtMeans, 
  #                                        StdErr = as.vector(stdErrSingleRRFC), 
  #                                        DF = as.vector(dfSingleRRFC), 
  #                                        CILower = CISingleRRFC[,1], 
  #                                        CIUpper = CISingleRRFC[,2], 
  #                                        row.names = NULL, 
  #                                        stringsAsFactors = FALSE)
  #     
  #     if (analysisOption == "RRFC")
  #       return(list(
  #         FVCA = list (
  #           # return transpose to match official code 
  #           # and it makes more sense to have readers in vertical direction 5/1/20
  #           foms = t(foms),
  #           trtMeans = trtMeans,
  #           trtMeanDiffs = trtMeanDiffs,
  #           TRCanovaDBM = TRCanovaDBM, 
  #           IndividualTrt = IndividualTrt, 
  #           varCom = varCom
  #         ),
  #         RRRC = NULL,
  #         FRRC = NULL,
  #         RRFC = list(
  #           FTests = RRFC$FTests, 
  #           ciDiffTrt = RRFC$ciDiffTrt, 
  #           ciAvgRdrEachTrt = RRFC$ciAvgRdrEachTrt
  #         )
  #       ))
  #   }
  # }
  # 
  # return(list(
  #   FVCA = list (
  #     # return transpose to match official code 
  #     # and it makes more sense to have readers in vertical direction 5/1/20
  #     foms = t(foms),
  #     trtMeans = trtMeans,
  #     trtMeanDiffs = trtMeanDiffs,
  #     TRCanovaDBM = TRCanovaDBM, 
  #     IndividualTrt = IndividualTrt, 
  #     varCom = varCom
  #   ),
  #   RRRC = list(
  #     FTests = RRRC$FTests, 
  #     ciDiffTrt = RRRC$ciDiffTrt, 
  #     ciAvgRdrEachTrt = RRRC$ciAvgRdrEachTrt
  #   ),
  #   FRRC = list(
  #     FTests = FRRC$FTests, 
  #     ciDiffTrt = FRRC$ciDiffTrt, 
  #     ciAvgRdrEachTrt = FRRC$ciAvgRdrEachTrt,
  #     msAnovaEachRdr = FRRC$msAnovaEachRdr,
  #     ssAnovaEachRdr = ssAnovaEachRdr,
  #     ciDiffTrtEachRdr = FRRC$ciDiffTrtEachRdr
  #   ),
  #   RRFC = list(
  #     FTests = RRFC$FTests, 
  #     ciDiffTrt = RRFC$ciDiffTrt, 
  #     ciAvgRdrEachTrt = RRFC$ciAvgRdrEachTrt
  #   )
  # ))
  
  
  
  
  