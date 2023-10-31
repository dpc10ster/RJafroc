# 
# this checks out for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
# checked vs. OR-DBM MRMC 2.51 <beta> Build  20181028 </beta> output in inst/Iowa
# 
ORSummaryRRFC <- function(FOMStats, ANOVA, alpha) {
  
  if (!is.list(FOMStats$foms)) {
    
    # ===========================================================================
    #   *****    Analysis 3 (OR Analysis): Random Readers and Fixed Cases     *****
    # ===========================================================================
    
    I <- dim(FOMStats$foms)[1]
    J <- dim(FOMStats$foms)[2]
    modalityID <- rownames(FOMStats$foms)
    
    trtMeans <-  FOMStats$trtMeans
    trtMeanDiffs  <-  FOMStats$trtMeanDiffs
    # (Results apply to the population of readers but only for the cases used in
    #   this study)
    # 
    # These results result from using the OR model, but treating reader as a random 
    # factor and modality and case as fixed factors.  Because case is treated as a fixed
    # factor, it follows that Cov1 = Cov2 = Cov3 = 0; i.e., there is no correlation
    # between reader-performance measures (e.g, AUCs) due to reading the same
    # cases.  Thus the OR model reduces to a conventional modality x reader ANOVA
    # for the reader-performance outcomes, where reader is a random factor and
    # modality is a fixed factor.  This is the same as a repeated measures ANOVA
    # where modality is the repeated measures factor, i.e., readers provide an
    # outcome (e.g., AUC) under each modality.
    # Note that the DBM and OR papers do not discuss this approach, but rather 
    # it is included here for completeness.
    # 
    # a) Test for H0: Treatments have the same AUC
    msDen <- ANOVA$TRanova["TR","MS"]
    f <- ANOVA$TRanova["T","MS"]/msDen
    ddf <- ((I - 1) * (J - 1))
    p <- 1 - pf(f, I - 1, ddf)
    diffTRName <- rownames(FOMStats$trtMeanDiffs)
    RRFC <- list()
    RRFC$FTests <- data.frame(DF = c(I-1,(I-1)*(J-1)), 
                              MS = c(ANOVA$TRanova["T","MS"],ANOVA$TRanova["TR","MS"]), 
                              F = c(f,NA),  p = c(p,NA), 
                              row.names = c("T","TR"), 
                              stringsAsFactors = FALSE)
    
    #   b) 95% confidence intervals and hypothesis tests (H0: difference = 0)
    #   for modality AUC differences
    
    stdErr <- sqrt(2 * msDen/J)
    tStat <- vector()
    PrGTt <- vector()
    CI <- array(dim = c(choose(I,2), 2))
    for (i in 1:choose(I,2)) {
      tStat[i] <- trtMeanDiffs[i,1]/stdErr
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE) 
      CI[i, ] <- c(trtMeanDiffs[i,1] + qt(alpha/2, ddf) * stdErr, 
                   trtMeanDiffs[i,1] + qt(1-alpha/2, ddf) * stdErr)
    }
    RRFC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                                 StdErr = rep(stdErr, choose(I, 2)), 
                                 DF = rep(ddf, choose(I, 2)), 
                                 t = tStat, 
                                 PrGTt = PrGTt, 
                                 CILower = CI[,1],
                                 CIUpper = CI[,2],
                                 row.names = diffTRName, 
                                 stringsAsFactors = FALSE)
    
    # StdErr = sqrt[2/r * MS(T*R)]
    # DF = df[MS(T*R)] = (t-1)(r-1)
    # 95% CI: Difference +- t(.025;df) * StdErr
    # Note: If there are only 2 treatments, this is equivalent to a paired t-test applied
    # to the AUCs
    
    #   c) Single modality AUC 95% confidence intervals
    # (Each analysis is based only on data for the specified modality, 
    #   i.e. on the modality-specfic reader ANOVA of AUCs
    dfSingle <- array(dim = I)
    msDenSingle <- array(dim = I)
    stdErrSingle <- array(dim = I)
    CISingle <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingle[i] <- ANOVA$IndividualTrt[i, "msREachTrt"]
      dfSingle[i] <- (J - 1)
      stdErrSingle[i] <- sqrt(msDenSingle[i]/J)
      CISingle[i, ] <- sort(c(trtMeans[i,1] - 
                                qt(alpha/2, dfSingle[i]) * stdErrSingle[i], trtMeans[i,1] + 
                                qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
    }
    RRFC$ciAvgRdrEachTrt <- data.frame(Estimate = trtMeans, 
                                       StdErr = as.vector(stdErrSingle), 
                                       DF = as.vector(dfSingle), 
                                       CILower = CISingle[,1], 
                                       CIUpper = CISingle[,2], 
                                       row.names = modalityID, 
                                       stringsAsFactors = FALSE)
    
    # StdErr = sqrt[1/r * MS(R)]
    # DF = df[MS(R)] = r-1
    # 95% CI: AUC +- t(.025;df) * StdErr
    # Note: this is the conventional CI, treating the reader AUCs as a random sample.
  } else {
    
    I1 <- dim(FOMStats$foms[[2]])[1]
    I2 <- dim(FOMStats$foms[[1]])[1]
    J <- dim(FOMStats$foms[[1]])[2]
    
    I <- c(I2,I1)
    modalityID1 <- rownames(FOMStats$foms[[1]])
    modalityID2 <- rownames(FOMStats$foms[[2]])
    modalityID <- list(modalityID1, modalityID2)
    
    trtMeans <-  FOMStats$trtMeans
    trtMeanDiffs  <-  FOMStats$trtMeanDiffs
    
    FTests <- list(list(), list())
    names(FTests) <- c("AvgMod1", "AvgMod2")
    ciDiffTrt <- list(list(), list())
    names(ciDiffTrt) <- c("AvgMod1", "AvgMod2")
    ciAvgRdrEachTrt <- list(list(), list())
    names(ciAvgRdrEachTrt) <- c("AvgMod1", "AvgMod2")
    
    for (AvgIndx in 1:2) {
      diffTRName <- rownames(FOMStats$trtMeanDiffs[[AvgIndx]])
      msDen <- ANOVA$TRanova[[AvgIndx]]["TR","MS"]
      f <- ANOVA$TRanova[[AvgIndx]]["T","MS"]/msDen
      ddf <- ((I[AvgIndx] - 1) * (J - 1))
      p <- 1 - pf(f, I[AvgIndx] - 1, ddf)
      FTests[[AvgIndx]] <- data.frame(DF = c(I[AvgIndx]-1,(I[AvgIndx]-1)*(J-1)), 
                                MS = c(ANOVA$TRanova[[AvgIndx]]["T","MS"],ANOVA$TRanova[[AvgIndx]]["TR","MS"]), 
                                F = c(f,NA),  p = c(p,NA), 
                                row.names = c("T","TR"), 
                                stringsAsFactors = FALSE)
      
      stdErr <- sqrt(2 * msDen/J)
      tStat <- vector()
      PrGTt <- vector()
      CI <- array(dim = c(choose(I[AvgIndx],2), 2))
      for (i in 1:choose(I[AvgIndx],2)) {
        tStat[i] <- trtMeanDiffs[[AvgIndx]][i,1]/stdErr
        PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE) 
        CI[i, ] <- c(trtMeanDiffs[[AvgIndx]][i,1] + qt(alpha/2, ddf) * stdErr, 
                     trtMeanDiffs[[AvgIndx]][i,1] + qt(1-alpha/2, ddf) * stdErr)
      }
      ciDiffTrt[[AvgIndx]] <- data.frame(Estimate = trtMeanDiffs[[AvgIndx]], 
                                   StdErr = rep(stdErr, choose(I[AvgIndx], 2)), 
                                   DF = rep(ddf, choose(I[AvgIndx], 2)), 
                                   t = tStat, 
                                   PrGTt = PrGTt, 
                                   CILower = CI[,1],
                                   CIUpper = CI[,2],
                                   row.names = diffTRName, 
                                   stringsAsFactors = FALSE)
      
      dfSingle <- array(dim = I[AvgIndx])
      msDenSingle <- array(dim = I[AvgIndx])
      stdErrSingle <- array(dim = I[AvgIndx])
      CISingle <- array(dim = c(I[AvgIndx], 2))
      for (i in 1:I[AvgIndx]) {
        msDenSingle[i] <- ANOVA$IndividualTrt[[AvgIndx]][i, "msREachTrt"]
        dfSingle[i] <- (J - 1)
        stdErrSingle[i] <- sqrt(msDenSingle[i]/J)
        CISingle[i, ] <- sort(c(trtMeans[[AvgIndx]][i,1] - 
                                  qt(alpha/2, dfSingle[i]) * stdErrSingle[i], trtMeans[[AvgIndx]][i,1] + 
                                  qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
      }
      ciAvgRdrEachTrt[[AvgIndx]] <- data.frame(Estimate = trtMeans[[AvgIndx]], 
                                         StdErr = as.vector(stdErrSingle), 
                                         DF = as.vector(dfSingle), 
                                         CILower = CISingle[,1], 
                                         CIUpper = CISingle[,2], 
                                         row.names = modalityID[[AvgIndx]], 
                                         stringsAsFactors = FALSE)
    }
    RRFC <- list(FTests = FTests,
                 ciDiffTrt = ciDiffTrt,
                 ciAvgRdrEachTrt = ciAvgRdrEachTrt)
  }  
  return(RRFC) 
}