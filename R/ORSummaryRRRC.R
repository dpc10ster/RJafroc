ORSummaryRRRC <- function(FOMStats, ANOVA, alpha) {
  
  if (!is.list(FOMStats$foms)) {
    # factorial one-treatment dataset 
    
    # ===========================================================================
    #   *****    Analysis 1 (OR Analysis): Random Readers and Random Cases    *****
    # ===========================================================================
    #     (Results apply to the population of readers and cases)
    
    modalityID <- rownames(FOMStats$foms)
    readerID <- colnames(FOMStats$foms)
    I <- length(modalityID)
    J <- length(readerID)
    
    trtMeans <-  FOMStats$trtMeans
    trtMeanDiffs  <-  FOMStats$trtMeanDiffs
    
    TRanova <- ANOVA$TRanova
    VarCom <- ANOVA$VarCom
    
    # a) Test for H0: Treatments have the same AUC
    msDen <- TRanova["TR","MS"] + max(J * (VarCom["Cov2",1] - VarCom["Cov3",1]), 0)
    f <- TRanova["T","MS"]/msDen
    ddf <- msDen^2/((TRanova["TR","MS"])^2/((I - 1) * (J - 1)))
    p <- 1 - pf(f, I - 1, ddf)
    RRRC <- list()
    RRRC$FTests <- data.frame(DF = c((I-1),ddf),
                              MS = c(TRanova["T", "MS"], msDen),
                              FStat = c(f,NA),
                              p = c(p,NA),
                              row.names = c("Treatment", "Error"),
                              stringsAsFactors = FALSE)
    
    # Df(error term) = [MS(T*R) + r*max(Cov2 - Cov3,0)]**2/{MS(T*R)**2/[(t-1)(r-1)]}
    # Note: "Error term" is the denominator of the F statistic and is a linear
    # combination of mean squares, as defined above.  The value of this linear 
    # combination is given under the "Mean Square" column
    # Note: Df(error term) is called "ddf_H" in Hillis (2007).
    
    #   b) 95% confidence intervals and hypothesis tests (H0: difference = 0)
    #   for modality AUC differences
    stdErr <- sqrt(2 * msDen/J)
    tStat <- vector()
    PrGTt <- vector()
    CI <- array(dim = c(length(trtMeanDiffs[,1]), 2))
    for (i in 1:length(trtMeanDiffs[,1])) {
      tStat[i] <- trtMeanDiffs[i,1]/stdErr
      PrGTt[i] <- 2*pt(abs(tStat[i]), ddf, lower.tail = FALSE)
      ci <- sort(c(trtMeanDiffs[i,1] - qt(alpha/2, ddf) * stdErr, 
                   trtMeanDiffs[i,1] + qt(alpha/2, ddf) * stdErr))
      if (length(ci) == 0){
        CI[i, ] <- c(NA, NA)
      }else{
        CI[i, ] <- ci
      }
    }
    RRRC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                                 StdErr = rep(stdErr, choose(I, 2)), 
                                 DF = rep(ddf, choose(I, 2)), 
                                 t = tStat, 
                                 PrGTt = PrGTt, 
                                 CILower = CI[,1],
                                 CIUpper = CI[,2], 
                                 row.names = rownames(FOMStats$trtMeanDiffs), 
                                 stringsAsFactors = FALSE)
    
    # StdErr = sqrt{(2/r)*[MS(T*R) + r*max(Cov2 - Cov3,0)]}
    # Df same as df(error term) from (a)
    # 95% CI: Difference +- t(.025;df) * StdErr
    
    # if (dataset$descriptions$design == "FCTRL") {
    #   c) Single-modality 95% confidence intervals
    # (Each analysis is based only on data for the specified modality, i.e., 
    #   on the modality-specific reader ANOVA of AUCs and Cov2 estimates.)
    df <- array(dim = I)
    msDenSingle <- array(dim = I)
    stdErr <- array(dim = I)
    CI <- array(dim = c(I, 2))
    ci <- data.frame()
    for (i in 1:I) {
      # Hillis 2007 5.3. Single test inference using only corresponding data
      msDenSingle[i] <- ANOVA$IndividualTrt[i, "msREachTrt"] + max(J * ANOVA$IndividualTrt[i, "cov2EachTrt"], 0)
      df[i] <- (msDenSingle[i])^2/(ANOVA$IndividualTrt[i, "msREachTrt"])^2 * (J - 1)
      stdErr[i] <- sqrt(msDenSingle[i]/J) # Eqn. 25
      CI[i,] <- c(trtMeans[i,1] + qt(alpha/2, df[i]) * stdErr[i], 
                  trtMeans[i,1] + qt(1-alpha/2, df[i]) * stdErr[i]) # Eqn. 25
      rowName <- modalityID[i]
      ci <- rbind(ci, data.frame(Estimate = trtMeans[i,1], 
                                 StdErr = stdErr[i],
                                 DF = df[i],
                                 CILower = CI[i,1],
                                 CIUpper = CI[i,2],
                                 Cov2 = ANOVA$IndividualTrt[i,"cov2EachTrt"],
                                 row.names = rowName,
                                 stringsAsFactors = FALSE))
    }
    RRRC$ciAvgRdrEachTrt <- ci
    
    # StdErr = sqrt{1/r * [MS(R) + r*max(Cov2,0)]}
    # Df = [MS(R)+ max(r*cov2,0)]**2/[(MS(R)**2/(r-1)]
    # Note: Df is called "ddf_H_single" in Hillis (2007)
    # 95% CI: AUC +- t(.025;df) * StdErr
    
    return(RRRC)
    
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    foms <- FOMStats$foms
    I1 <- dim(foms[[2]])[1]
    I2 <- dim(foms[[1]])[1]
    I <- c(I2,I1)
    J <- dim(foms[[1]])[2]

    IndividualTrt <- ANOVA$IndividualTrt
    cov2EachTrt <- IndividualTrt[[1]][4]
    varEachTrt <- IndividualTrt[[1]][3]
    trtMeans <- FOMStats$trtMeans
    trtMeanDiffs <- FOMStats$trtMeanDiffs
    diffTRName <- FOMStats$diffTRName
    TRanova <- ANOVA$TRanova
    VarCom <- ANOVA$VarCom
 
    FTests <- list(list(), list())
    names(FTests) <- c("AvgMod1", "AvgMod2")
    ciDiffTrt <- list(list(), list())
    names(ciDiffTrt) <- c("AvgMod1", "AvgMod2")
    ciAvgRdrEachTrt <- list(list(), list())
    names(ciAvgRdrEachTrt) <- c("AvgMod1", "AvgMod2")
    
    I <- c(I2,I1)
    modalityID1 <- rownames(FOMStats$foms[[1]])
    modalityID2 <- rownames(FOMStats$foms[[2]])
    modalityID <- list(modalityID1, modalityID2)
    for (avgIndx in 1:2) {
      msDen <- TRanova[[avgIndx]]["TR","MS"] + 
        max(J * (VarCom[[avgIndx]]["Cov2",1] - VarCom[[avgIndx]]["Cov3",1]), 0)
      f <- TRanova[[avgIndx]]["T","MS"]/msDen
      ddf <- msDen^2/((TRanova[[avgIndx]]["TR","MS"])^2/((I[avgIndx] - 1) * (J - 1)))
      p <- 1 - pf(f, I[avgIndx] - 1, ddf)
      FTests[[avgIndx]] <- data.frame(DF = c((I[avgIndx]-1),ddf),
                                MS = c(TRanova[[avgIndx]]["T", "MS"], msDen),
                                FStat = c(f,NA),
                                p = c(p,NA),
                                row.names = c("Treatment", "Error"),
                                stringsAsFactors = FALSE)
      
      stdErr <- sqrt(2 * msDen/J)
      tStat <- vector()
      PrGTt <- vector()
      CI <- array(dim = c(length(trtMeanDiffs[[avgIndx]][,1]), 2))
      for (i in 1:length(trtMeanDiffs[[avgIndx]][,1])) {
        tStat[i] <- trtMeanDiffs[[avgIndx]][i,1]/stdErr
        PrGTt[i] <- 2*pt(abs(tStat[i]), ddf, lower.tail = FALSE)
        ci <- sort(c(trtMeanDiffs[[avgIndx]][i,1] - qt(alpha/2, ddf) * stdErr, 
                     trtMeanDiffs[[avgIndx]][i,1] + qt(alpha/2, ddf) * stdErr))
        if (length(ci) == 0){
          CI[i, ] <- c(NA, NA)
        }else{
          CI[i, ] <- ci
        }
      }
      ciDiffTrt[[avgIndx]] <- data.frame(Estimate = trtMeanDiffs[[avgIndx]], 
                                   StdErr = rep(stdErr, choose(I[avgIndx], 2)), 
                                   DF = rep(ddf, choose(I[avgIndx], 2)), 
                                   t = tStat, 
                                   PrGTt = PrGTt, 
                                   CILower = CI[,1],
                                   CIUpper = CI[,2], 
                                   row.names = rownames(trtMeanDiffs[[avgIndx]]), 
                                   stringsAsFactors = FALSE)
      
      df <- array(dim = I[avgIndx])
      msDenSingle <- array(dim = I[avgIndx])
      stdErr <- array(dim = I[avgIndx])
      CI <- array(dim = c(I[avgIndx], 2))
      ci <- data.frame()
      for (i in 1:I[avgIndx]) {
        msDenSingle[i] <- IndividualTrt[[avgIndx]][i, "msREachTrt"] + 
          max(J * IndividualTrt[[avgIndx]][i, "cov2EachTrt"], 0)
        df[i] <- (msDenSingle[i])^2/(IndividualTrt[[avgIndx]][i, "msREachTrt"])^2 * (J - 1)
        stdErr[i] <- sqrt(msDenSingle[i]/J)
        CI[i,] <- c(trtMeans[[avgIndx]][i,1] + qt(alpha/2, df[i]) * stdErr[i], 
                    trtMeans[[avgIndx]][i,1] + qt(1-alpha/2, df[i]) * stdErr[i]) # Eqn. 25
        rowName <- modalityID[[avgIndx]][i]
        ci <- rbind(ci, data.frame(Estimate = trtMeans[[avgIndx]][i,1], 
                                   StdErr = stdErr[i],
                                   DF = df[i],
                                   CILower = CI[i,1],
                                   CIUpper = CI[i,2],
                                   Cov2 = IndividualTrt[[avgIndx]][i,"cov2EachTrt"],
                                   row.names = rowName,
                                   stringsAsFactors = FALSE))
      }
      ciAvgRdrEachTrt[[avgIndx]] <- ci
    }
    RRRC <- list(FTests = FTests,
                 ciDiffTrt = ciDiffTrt,
                 ciAvgRdrEachTrt = ciAvgRdrEachTrt)
  }  
  return(RRRC) 
}

