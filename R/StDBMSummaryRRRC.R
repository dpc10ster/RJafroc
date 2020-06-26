# 
# This was compared to JAFROC Version 4.2.1 (run using Windows-8 under VMware Fusion on iMac) 
# Results are in inst/JAFROC
# It checks out with Windows JAFROC for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
# Current Iowa software does not do DBM analysis; 
# The outputs were also compared to current
# Iowa software and agrees for all except minor differences for FRRC (since in OR analysis the ddf is Inf instead
# of (I-1)(K-1))
# 
DBMSummaryRRRC <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
  
  readerID <- dataset$descriptions$readerID
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(dataset$ratings$NL)[3]
  
  trtMeans <-  FOMs$trtMeans[,"Estimate"]
  trtMeanDiffs  <-  FOMs$trtMeanDiffs$Estimate
  
  TRCanova <- ANOVA$TRCanova
  IndividualTrt <- ANOVA$IndividualTrt

  msDen <- TRCanova["TR", "MS"] + max(TRCanova["TC", "MS"] - TRCanova["TRC", "MS"], 0)
  f <- TRCanova["T", "MS"]/msDen
  ddf <- msDen^2/((TRCanova["TR", "MS"])^2/((I - 1) * (J - 1)))
  p <- 1 - pf(f, I - 1, ddf)
  RRRC <- list()
  RRRC$FTests <- data.frame(DF = c((I-1),ddf),
                            MS = c(TRCanova["T", "MS"], msDen),
                            FStat = c(f,NA),
                            p = c(p,NA),
                            row.names = c("Treatment", "Error"),
                            stringsAsFactors = FALSE)
  
  stdErr <- sqrt(2 * msDen/J/K)
  tStat <- vector()
  PrGTt <- vector()
  CI <- array(dim = c(choose(I,2),2))
  df <- ddf
  for (i in 1:length(trtMeanDiffs)) {
    tStat[i] <- trtMeanDiffs[i]/stdErr
    PrGTt[i] <- 2 * pt(abs(tStat[i]), df, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
    CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, df) * stdErr, trtMeanDiffs[i] + qt(alpha/2, df) * stdErr))
    
  }
  RRRC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                               StdErr = rep(stdErr, choose(I, 2)), 
                               DF = rep(df, choose(I, 2)), 
                               t = tStat, 
                               PrGTt = PrGTt, # renamed this consistently
                               CILower = CI[,1],  # instead of adding CIRRC and then using a names() to split out the two values
                               CIUpper = CI[,2],  # do:
                               row.names = diffTRName,
                               stringsAsFactors = FALSE)
  
  dfSingle <- array(dim = I)
  msDenSingle <- array(dim = I)
  stdErrSingle <- array(dim = I)
  CISingle <- array(dim = c(I, 2))
  for (i in 1:I) {
    msDenSingle[i] <- IndividualTrt["msR",i+1] + max(IndividualTrt["msC",i+1] - IndividualTrt["msRC",i+1], 0) # first member is DF
    dfSingle[i] <- msDenSingle[i]^2/IndividualTrt["msR",i+1]^2 * (J - 1)
    stdErrSingle[i] <- sqrt(msDenSingle[i]/J/K)
    ciTemp <- sort(c(trtMeans[i] - qt(alpha/2, dfSingle[i]) * stdErrSingle[i], 
                     trtMeans[i] + qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
    if (length(ciTemp) == 2) CISingle[i, ] <- ciTemp
    
  }
  RRRC$ciAvgRdrEachTrt <- data.frame(Estimate = trtMeans, 
                                     StdErr = stdErrSingle, 
                                     DF = dfSingle,
                                     CILower = CISingle[,1], 
                                     CIUpper = CISingle[,2], 
                                     row.names = paste0("trt", modalityID), 
                                     stringsAsFactors = FALSE)
  
  return(RRRC)
}