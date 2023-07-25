# 
# This was compared to JAFROC Version 4.2.1 (run using Windows-8 under VMware Fusion on iMac) 
# Results are in inst/JAFROC
# It checks out with Windows JAFROC for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
# Current Iowa software does not do DBM analysis; 
# The outputs were also compared to current
# Iowa software and agrees for all except minor differences for FRRC (since in OR analysis the ddf is Inf instead
# of (I-1)(K-1))
# 
DBMSummaryRRFC <- function(dataset, FOM, FOMStats, ANOVA, alpha, diffTRName) {
  
  readerID <- dataset$descriptions$readerID
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  J <- length(readerID)
  K2 <- dim(dataset$ratings$LL)[3]
  K <- dim(dataset$ratings$NL)[3]
  K1 <- K - K2
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    K <- K1
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    K <- K2
  }
  
  trtMeans <-  FOMStats$trtMeans
  trtMeanDiffs  <-  FOMStats$trtMeanDiffs$Estimate
 
  TRCanova <- ANOVA$TRCanova
  
  msDen <- TRCanova["TR", "MS"]
  f <- TRCanova["T", "MS"]/msDen
  ddf <- (I - 1) * (J - 1)
  p <- 1 - pf(f, I - 1, ddf)
  RRFC <- list()
  RRFC$FTests <- data.frame(DF = c((I-1),ddf),
                            MS = c(TRCanova["T", "MS"], msDen),
                            FStat = c(f,NA),
                            p = c(p,NA),
                            row.names = c("Treatment", "Error"),
                            stringsAsFactors = FALSE)
  
  stdErr <- sqrt(2 * msDen/J/K)
  tStat <- vector()
  PrGTt <- vector()
  CI <- array(dim = c(choose(I,2),2))
  for (i in 1:length(trtMeanDiffs)) {
    tStat[i] <- trtMeanDiffs[i]/stdErr
    PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE)
    CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, ddf) * stdErr, 
                      trtMeanDiffs[i] + qt(alpha/2, ddf) * stdErr))
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
  
  dfSingle <- array(dim = I)
  msDenSingle <- array(dim = I)
  stdErrSingle <- array(dim = I)
  CISingle <- array(dim = c(I, 2))
  for (i in 1:I) {
    msDenSingle[i] <- ANOVA$IndividualTrt["msR",i+1]
    dfSingle[i] <- (J - 1)
    stdErrSingle[i] <- sqrt(msDenSingle[i]/J/K)
    CISingle[i, ] <- sort(c(trtMeans[i, "Estimate"] - qt(alpha/2, dfSingle[i]) * stdErrSingle[i], 
                            trtMeans[i, "Estimate"] + qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
  }
  RRFC$ciAvgRdrEachTrt <- data.frame(Estimate = trtMeans, 
                                     StdErr = as.vector(stdErrSingle), 
                                     DF = as.vector(dfSingle), 
                                     CILower = CISingle[,1], 
                                     CIUpper = CISingle[,2], 
                                     row.names = paste0("trt", modalityID), 
                                     stringsAsFactors = FALSE)
  
  return(RRFC) 
}