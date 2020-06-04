# 
# This was compared to JAFROC Version 4.2.1 (run using Windows-8 under VMware Fusion on iMac) 
# Results are in inst/JAFROC
# It checks out with Windows JAFROC for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
# Current Iowa software does not do DBMH analysis; 
# The outputs were also compared to current
# Iowa software and agrees for all except minor differences for FRRC (since in OR analysis the ddf is Inf instead
# of (I-1)(K-1))
# 
DBMSummaryFRRC <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
  
  readerID <- dataset$descriptions$readerID
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(dataset$ratings$NL)[3]
  
  foms <- FOMs$foms
  trtMeans <- FOMs$trtMeans[,"Estimate"]
  trtMeanDiffs  <-FOMs$trtMeanDiffs$Estimate
  
  TRCanova <- ANOVA$TRCanova
  IndividualRdr <- unlist(ANOVA$IndividualRdr["msTC",2:(J+1)])
  
  msDen <- TRCanova["TC", "MS"]
  f <- TRCanova["T", "MS"]/msDen
  ddf <- (I - 1) * (K - 1)
  p <- 1 - pf(f, I - 1, ddf)
  FRRC <- list()
  FRRC$FTests <- data.frame(DF = c((I-1),(I - 1) * (K - 1)),
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
    PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE)  # critical correction noted by user Lucy D'Agostino McGowan
    CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, ddf) * stdErr, trtMeanDiffs[i] + qt(alpha/2, ddf) * stdErr))
  }
  FRRC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
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
    msDenSingle[i] <- ANOVA$IndividualTrt["msC", i+1]
    dfSingle[i] <- (K - 1)
    stdErrSingle[i] <- sqrt(msDenSingle[i]/J/K)
    CISingle[i, ] <- sort(c(trtMeans[i] - qt(alpha/2, dfSingle[i]) * stdErrSingle[i], 
                            trtMeans[i] + qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
  }
  FRRC$ciAvgRdrEachTrt <- data.frame(Estimate = trtMeans, 
                                     StdErr = as.vector(stdErrSingle), 
                                     DF = as.vector(dfSingle), 
                                     CILower = CISingle[,1], 
                                     CIUpper = CISingle[,2], 
                                     row.names = paste0("trt", modalityID), 
                                     stringsAsFactors = FALSE)
  
  trtMeanDiffs <- array(dim = c(J, choose(I, 2)))
  Reader <- array(dim = c(J, choose(I, 2)))
  dfReader <- array(dim = c(J, choose(I, 2)))
  stdErr <- array(dim = c(J, choose(I, 2)))
  tStat <- array(dim = c(J, choose(I, 2)))
  trDiffNames <- array(dim = c(J, choose(I, 2)))
  PrGTt <- array(dim = c(J, choose(I, 2)))
  CIReader <- array(dim = c(J, choose(I, 2),2))
  ci <- data.frame()
  for (j in 1:J) {
    Reader[j,] <- rep(readerID[j], choose(I, 2))
    stdErr[j,] <- sqrt(2 * IndividualRdr[j]/K)
    pair <- 1
    for (i in 1:I) {
      if (i == I) break
      for (ip in (i + 1):I) {
        dfReader[j,pair] <- K-1
        trtMeanDiffs[j, pair] <- foms[i, j] - foms[ip, j]
        trDiffNames[j,pair] <- diffTRName[pair]
        tStat[j,pair] <- trtMeanDiffs[j,pair]/stdErr[j,pair]
        PrGTt[j,pair] <- 2 * pt(abs(tStat[j,pair]), dfReader[j,pair], lower.tail = FALSE)
        CIReader[j, pair,] <- sort(c(trtMeanDiffs[j,pair] - qt(alpha/2, dfReader[j,pair]) * stdErr[j,pair], 
                                     trtMeanDiffs[j,pair] + qt(alpha/2, dfReader[j,pair]) * stdErr[j,pair]))
        rowName <- paste0("rdr", Reader[j,pair], "::", trDiffNames[j, pair])
        ci <- rbind(ci, data.frame(Estimate = trtMeanDiffs[j, pair], 
                                   StdErr = stdErr[j,pair], 
                                   DF = dfReader[j,pair],
                                   t = tStat[j, pair], 
                                   PrGTt = PrGTt[j, pair], 
                                   CILower = CIReader[j, pair,1],
                                   CIUpper = CIReader[j, pair,2],
                                   row.names = rowName,
                                   stringsAsFactors = FALSE))
        pair <- pair + 1
      }
    }
  }
  FRRC$ciDiffTrtEachRdr <- ci

  return(FRRC) 
}