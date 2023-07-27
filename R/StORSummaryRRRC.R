# 
# this checks out for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
# checked vs. OR-DBM MRMC 2.51 <beta> Build  20181028 </beta> output in inst/Iowa
# 
ORSummaryRRRC <- function(dataset, FOMStats, ANOVA, alpha, diffTRName) {
  # ===========================================================================
  #   *****    Analysis 1 (OR Analysis): Random Readers and Random Cases    *****
  # ===========================================================================
  #     (Results apply to the population of readers and cases)
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
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
  #   for treatment AUC differences
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
                               row.names = diffTRName, 
                               stringsAsFactors = FALSE)
  
  # StdErr = sqrt{(2/r)*[MS(T*R) + r*max(Cov2 - Cov3,0)]}
  # Df same as df(error term) from (a)
  # 95% CI: Difference +- t(.025;df) * StdErr
  
  # if (dataset$descriptions$design == "FCTRL") {
  #   c) Single-treatment 95% confidence intervals
  # (Each analysis is based only on data for the specified treatment, i.e., 
  #   on the treatment-specific reader ANOVA of AUCs and Cov2 estimates.)
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
    rowName <- paste0("trt", modalityID[i])
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
}

