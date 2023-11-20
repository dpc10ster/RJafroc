StDBMAnalysis <- function(dataset, 
                          FOM, 
                          analysisOption,
                          alpha,
                          FPFValue) 
{
  
  if (length(dim(dataset$ratings$NL)) == 4) {
    # factorial one-treatment dataset 
    
    I <- dim(dataset$ratings$NL)[1]
    
    modalityID <- dataset$descriptions$modalityID
    
    vc <- UtilDBMVarComp(dataset, FOM, FPFValue)
    
    VarCom <- vc$VarCom
    TRCanova <- vc$TRCanova
    IndividualTrt <- vc$IndividualTrt
    IndividualRdr <- vc$IndividualRdr
    
    ANOVA <- list()
    ANOVA$TRCanova <- TRCanova
    ANOVA$VarCom <- VarCom
    ANOVA$IndividualTrt <- IndividualTrt
    ANOVA$IndividualRdr <- IndividualRdr
    
    fom_ij <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    trtMeans <- rowMeans(fom_ij) 
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
    
    FOMStats <- list(
      foms = fom_ij,
      trtMeans = trtMeans,
      trtMeanDiffs = trtMeanDiffs,
      diffTRName = diffTRName
    )
    
    K2 <- dim(dataset$ratings$LL)[3]
    K <- dim(dataset$ratings$NL)[3]
    K1 <- K - K2
    if (FOM %in% c("MaxNLF", "HrSp")) {
      K <- K1
    } else if (FOM %in% c("MaxLLF", "HrSe")) {
      K <- K2
    }
    
    if (analysisOption == "RRRC") {
      RRRC <- DBM_RRRC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRRC = RRRC
      ))
    }  
    
    if (analysisOption == "FRRC") {
      FRRC <- DBM_FRRC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        FRRC = FRRC
      ))
    }  
    
    if (analysisOption == "RRFC") {
      RRFC <- DBM_RRFC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRFC = RRFC
      ))
    }  
    
    if (analysisOption == "ALL") {
      RRRC <- DBM_RRRC(K, FOMStats, ANOVA, alpha)
      FRRC <- DBM_FRRC(K, FOMStats, ANOVA, alpha)
      RRFC <- DBM_RRFC(K, FOMStats, ANOVA, alpha)
      
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRRC = RRRC,
        FRRC = FRRC,
        RRFC = RRFC
      ))
    }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
    
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    modalityID1 <- dataset$descriptions$modalityID1
    modalityID2 <- dataset$descriptions$modalityID2
    modalityID <- list(modalityID2, modalityID1)
    I1 <- length(modalityID1)
    I2 <- length(modalityID2)
    I <- c(I2,I1)
    
    vc <- UtilDBMVarComp(dataset, FOM, FPFValue)
    
    ANOVA <- list()
    ANOVA$TRCanova <- vc$TRCanova
    ANOVA$VarCom <- vc$VarCom
    ANOVA$IndividualTrt <- vc$IndividualTrt
    ANOVA$IndividualRdr <- vc$IndividualRdr
    
    fom_i1i2j <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    fomsAvgEachXModality <- Arr2List(dataset, fom_i1i2j)
    trtMeanDiffs <- list()
    trtMeans <- list()
    diffTRName <- list()
    for (avgIndx in 1:2) { # treatment index that FOM was averaged over
      trtMeans1 <- rowMeans(fomsAvgEachXModality[[avgIndx]])
      trtMeans[[avgIndx]] <- as.data.frame(trtMeans1)
      colnames(trtMeans[[avgIndx]]) <- "Estimate"
      trtMeanDiffs1 <- array(dim = choose(I[avgIndx], 2))
      diffTRName[[avgIndx]] <- array(dim = choose(I[avgIndx], 2))
      
      ii <- 1
      for (i in 1:I[avgIndx]) {
        if (i == I[avgIndx]) 
          break
        for (ip in (i + 1):I[avgIndx]) {
          trtMeanDiffs1[ii] <- trtMeans[[avgIndx]][i,1] - trtMeans[[avgIndx]][ip,1]
          diffTRName[[avgIndx]][ii] <- paste0("trt", modalityID[[avgIndx]][i], 
                                              sep = "-", 
                                              "trt", modalityID[[avgIndx]][ip])
          ii <- ii + 1
        }
      }
      
      trtMeanDiffs[[avgIndx]] <- data.frame("Estimate" = trtMeanDiffs1,
                                            row.names = diffTRName[[avgIndx]],
                                            stringsAsFactors = FALSE)
    }
    
    names(trtMeans) <- c("AvgMod1", "AvgMod2")
    names(trtMeanDiffs) <- c("AvgMod1", "AvgMod2")
    
    FOMStats <- list(
      foms = fomsAvgEachXModality,
      trtMeans = trtMeans,
      trtMeanDiffs = trtMeanDiffs,
      diffTRName = diffTRName)
    
    K2 <- dim(dataset$ratings$LL)[4]
    K <- dim(dataset$ratings$NL)[4]
    K1 <- K - K2
    if (FOM %in% c("MaxNLF", "HrSp")) {
      K <- K1
    } else if (FOM %in% c("MaxLLF", "HrSe")) {
      K <- K2
    }
    
    if (analysisOption == "RRRC") {
      RRRC <- DBM_RRRC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRRC = RRRC
      ))
    }  
    
    if (analysisOption == "FRRC") {
      FRRC <- DBM_FRRC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        FRRC = FRRC
      ))
    }  
    
    if (analysisOption == "RRFC") {
      RRFC <- DBM_RRFC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRFC = RRFC
      ))
    }  
    
    if (analysisOption == "ALL") {
      RRRC <- DBM_RRRC(K, FOMStats, ANOVA, alpha)
      FRRC <- DBM_FRRC(K, FOMStats, ANOVA, alpha)
      RRFC <- DBM_RRFC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRRC = RRRC,
        FRRC = FRRC,
        RRFC = RRFC
      ))
    } 
  } 
} 




DBM_RRRC <- function(K, FOMStats, ANOVA, alpha) {
  # 
  # This was compared to JAFROC Version 4.2.1 (run using Windows-8 under VMware Fusion on iMac) 
  # Results are in inst/JAFROC
  # It checks out with Windows JAFROC for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
  # Current Iowa software does not do DBM analysis; 
  # The outputs were also compared to current
  # Iowa software and agrees for all except minor differences for FRRC (since in OR analysis the ddf is Inf instead
  # of (I-1)(K-1))
  # 
  
  if (!is.list(FOMStats$foms)) {
    # factorial one-treatment dataset
    
    modalityID <- rownames(FOMStats$foms)
    readerID <- colnames(FOMStats$foms)
    I <- length(modalityID)
    J <- length(readerID)
    
    trtMeans <-  FOMStats$trtMeans[,"Estimate"]
    trtMeanDiffs  <-  FOMStats$trtMeanDiffs$Estimate
    diffTRName <- FOMStats$diffTRName
    
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
      PrGTt[i] <- 2 * pt(abs(tStat[i]), df, lower.tail = FALSE)
      CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, df) * stdErr, 
                        trtMeanDiffs[i] + qt(alpha/2, df) * stdErr))
      
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
                                       row.names = modalityID, 
                                       stringsAsFactors = FALSE)
    
    return(RRRC)
    
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    modalityID2 <- rownames(FOMStats$foms[[1]])
    modalityID1 <- rownames(FOMStats$foms[[2]])
    I1 <- length(modalityID1)
    I2 <- length(modalityID2)
    modalityID <- list(modalityID2, modalityID1) # ??
    I <- c(I2,I1)
    readerID <- colnames(FOMStats$foms[[1]])
    J <- length(readerID)
    
    FTests <- list(list(), list())
    names(FTests) <- c("AvgMod1", "AvgMod2")
    ciDiffTrt <- list(list(), list())
    names(ciDiffTrt) <- c("AvgMod1", "AvgMod2")
    ciAvgRdrEachTrt <- list(list(), list())
    names(ciAvgRdrEachTrt) <- c("AvgMod1", "AvgMod2")
    
    for (avgIndx in 1:2) {
      
      trtMeans <-  FOMStats$trtMeans[[avgIndx]][,"Estimate"]
      trtMeanDiffs  <-  FOMStats$trtMeanDiffs[[avgIndx]]$Estimate
      diffTRName <- FOMStats$diffTRName[[avgIndx]]
      
      TRCanova <- ANOVA$TRCanova[[avgIndx]]
      IndividualTrt <- ANOVA$IndividualTrt[[avgIndx]]
      
      msDen <- TRCanova["TR", "MS"] + max(TRCanova["TC", "MS"] - TRCanova["TRC", "MS"], 0)
      f <- TRCanova["T", "MS"]/msDen
      ddf <- msDen^2/((TRCanova["TR", "MS"])^2/((I[avgIndx] - 1) * (J - 1)))
      p <- 1 - pf(f, I[avgIndx] - 1, ddf)
      FTests[[avgIndx]] <- data.frame(DF = c((I[avgIndx]-1),ddf),
                                      MS = c(TRCanova["T", "MS"], msDen),
                                      FStat = c(f,NA),
                                      p = c(p,NA),
                                      row.names = c("Treatment", "Error"),
                                      stringsAsFactors = FALSE)
      
      stdErr <- sqrt(2 * msDen/J/K)
      tStat <- vector()
      PrGTt <- vector()
      CI <- array(dim = c(choose(I[avgIndx],2),2))
      df <- ddf
      for (i in 1:length(trtMeanDiffs)) {
        tStat[i] <- trtMeanDiffs[i]/stdErr
        PrGTt[i] <- 2 * pt(abs(tStat[i]), df, lower.tail = FALSE)
        CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, df) * stdErr, 
                          trtMeanDiffs[i] + qt(alpha/2, df) * stdErr))
        
      }
      ciDiffTrt[[avgIndx]] <- data.frame(Estimate = trtMeanDiffs, 
                                         StdErr = rep(stdErr, choose(I[avgIndx], 2)), 
                                         DF = rep(df, choose(I[avgIndx], 2)), 
                                         t = tStat, 
                                         PrGTt = PrGTt, # renamed this consistently
                                         CILower = CI[,1],  # instead of adding CIRRC and then using a names() to split out the two values
                                         CIUpper = CI[,2],  # do:
                                         row.names = diffTRName,
                                         stringsAsFactors = FALSE)
      
      dfSingle <- array(dim = I[avgIndx])
      msDenSingle <- array(dim = I[avgIndx])
      stdErrSingle <- array(dim = I[avgIndx])
      CISingle <- array(dim = c(I[avgIndx], 2))
      for (i in 1:I[avgIndx]) {
        msDenSingle[i] <- IndividualTrt["msR",i+1] + max(IndividualTrt["msC",i+1] - IndividualTrt["msRC",i+1], 0) # first member is DF
        dfSingle[i] <- msDenSingle[i]^2/IndividualTrt["msR",i+1]^2 * (J - 1)
        stdErrSingle[i] <- sqrt(msDenSingle[i]/J/K)
        ciTemp <- sort(c(trtMeans[i] - qt(alpha/2, dfSingle[i]) * stdErrSingle[i], 
                         trtMeans[i] + qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
        if (length(ciTemp) == 2) CISingle[i, ] <- ciTemp
        
      }
      ciAvgRdrEachTrt[[avgIndx]] <- data.frame(Estimate = trtMeans, 
                                               StdErr = stdErrSingle, 
                                               DF = dfSingle,
                                               CILower = CISingle[,1], 
                                               CIUpper = CISingle[,2], 
                                               row.names = modalityID[[avgIndx]], # ??
                                               stringsAsFactors = FALSE)
      
      
    } # end of for (avgIndx in 1:2)
    
    RRRC <- list(FTests = FTests,
                 ciDiffTrt = ciDiffTrt,
                 ciAvgRdrEachTrt = ciAvgRdrEachTrt)
    
    return(RRRC) 
    
  } # end of else clause
}




DBM_FRRC <- function(K, FOMStats, ANOVA, alpha) {
  # 
  # This was compared to JAFROC Version 4.2.1 (run using Windows-8 under VMware Fusion on iMac) 
  # Results are in inst/JAFROC
  # It checks out with Windows JAFROC for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
  # Current Iowa software does not do DBM analysis; 
  # The outputs were also compared to current
  # Iowa software and agrees for all except minor differences for FRRC (since in OR analysis the ddf is Inf instead
  # of (I-1)(K-1))
  # 
  
  if (!is.list(FOMStats$foms)) {
    # factorial one-treatment dataset
    
    modalityID <- rownames(FOMStats$foms)
    readerID <- colnames(FOMStats$foms)
    I <- length(modalityID)
    J <- length(readerID)
    foms <- FOMStats$foms
    
    trtMeans <-  FOMStats$trtMeans[,"Estimate"]
    trtMeanDiffs  <-  FOMStats$trtMeanDiffs$Estimate
    diffTRName <- FOMStats$diffTRName
    
    TRCanova <- ANOVA$TRCanova
    IndividualRdr <- unlist(ANOVA$IndividualRdr["msTC", 2:(J+1)])
    
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
      PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE)
      CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, ddf) * stdErr, 
                        trtMeanDiffs[i] + qt(alpha/2, ddf) * stdErr))
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
                                       row.names = modalityID, 
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
          rowName <- paste0(Reader[j,pair], "::", trDiffNames[j, pair])
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
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    modalityID2 <- rownames(FOMStats$foms[[1]])
    modalityID1 <- rownames(FOMStats$foms[[2]])
    I1 <- length(modalityID1)
    I2 <- length(modalityID2)
    modalityID <- list(modalityID2, modalityID1) # ??
    I <- c(I2,I1)
    readerID <- colnames(FOMStats$foms[[1]])
    J <- length(readerID)
    
    FTests <- list(list(), list())
    names(FTests) <- c("AvgMod1", "AvgMod2")
    ciDiffTrt <- list(list(), list())
    names(ciDiffTrt) <- c("AvgMod1", "AvgMod2")
    ciAvgRdrEachTrt <- list(list(), list())
    names(ciAvgRdrEachTrt) <- c("AvgMod1", "AvgMod2")
    ciDiffTrtEachRdr <- list(list(), list())
    names(ciDiffTrtEachRdr) <- c("AvgMod1", "AvgMod2")
    
    for (avgIndx in 1:2) {
      
      foms <- FOMStats$foms[[avgIndx]]
      trtMeans <-  FOMStats$trtMeans[[avgIndx]][,"Estimate"]
      trtMeanDiffs  <-  FOMStats$trtMeanDiffs[[avgIndx]]$Estimate
      diffTRName <- FOMStats$diffTRName[[avgIndx]]
      
      TRCanova <- ANOVA$TRCanova[[avgIndx]]
      IndividualTrt <- ANOVA$IndividualTrt[[avgIndx]]
      IndividualRdr <- unlist(ANOVA$IndividualRdr[[avgIndx]]["msTC", 2:(J+1)])
      msDen <- TRCanova["TC", "MS"]
      f <- TRCanova["T", "MS"]/msDen
      ddf <- (I[avgIndx] - 1) * (K - 1)
      p <- 1 - pf(f, I[avgIndx] - 1, ddf)
      FTests[[avgIndx]] <- data.frame(DF = c((I[avgIndx]-1),(I[avgIndx] - 1) * (K - 1)),
                                      MS = c(TRCanova["T", "MS"], msDen),
                                      FStat = c(f,NA),
                                      p = c(p,NA),
                                      row.names = c("Treatment", "Error"),
                                      stringsAsFactors = FALSE)
      
      stdErr <- sqrt(2 * msDen/J/K)
      tStat <- vector()
      PrGTt <- vector()
      CI <- array(dim = c(choose(I[avgIndx],2),2))
      for (i in 1:length(trtMeanDiffs)) {
        tStat[i] <- trtMeanDiffs[i]/stdErr
        PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE)
        CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, ddf) * stdErr, 
                          trtMeanDiffs[i] + qt(alpha/2, ddf) * stdErr))
      }
      ciDiffTrt[[avgIndx]] <- data.frame(Estimate = trtMeanDiffs, 
                                         StdErr = rep(stdErr, choose(I[avgIndx], 2)), 
                                         DF = rep(ddf, choose(I[avgIndx], 2)), 
                                         t = tStat, 
                                         PrGTt = PrGTt, 
                                         CILower = CI[,1], 
                                         CIUpper = CI[,2],
                                         row.names = diffTRName,
                                         stringsAsFactors = FALSE)
      
      dfSingle <- array(dim = I[avgIndx])
      msDenSingle <- array(dim = I[avgIndx])
      stdErrSingle <- array(dim = I[avgIndx])
      CISingle <- array(dim = c(I[avgIndx], 2))
      for (i in 1:I[avgIndx]) {
        msDenSingle[i] <- IndividualTrt["msC", i+1]
        dfSingle[i] <- (K - 1)
        stdErrSingle[i] <- sqrt(msDenSingle[i]/J/K)
        CISingle[i, ] <- sort(c(trtMeans[i] - qt(alpha/2, dfSingle[i]) * stdErrSingle[i], 
                                trtMeans[i] + qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
      }
      ciAvgRdrEachTrt[[avgIndx]] <- data.frame(Estimate = trtMeans, 
                                               StdErr = as.vector(stdErrSingle), 
                                               DF = as.vector(dfSingle), 
                                               CILower = CISingle[,1], 
                                               CIUpper = CISingle[,2], 
                                               row.names = modalityID[[avgIndx]], 
                                               stringsAsFactors = FALSE)
      
      trtMeanDiffs <- array(dim = c(J, choose(I[avgIndx], 2)))
      Reader <- array(dim = c(J, choose(I[avgIndx], 2)))
      dfReader <- array(dim = c(J, choose(I[avgIndx], 2)))
      stdErr <- array(dim = c(J, choose(I[avgIndx], 2)))
      tStat <- array(dim = c(J, choose(I[avgIndx], 2)))
      trDiffNames <- array(dim = c(J, choose(I[avgIndx], 2)))
      PrGTt <- array(dim = c(J, choose(I[avgIndx], 2)))
      CIReader <- array(dim = c(J, choose(I[avgIndx], 2),2))
      ci <- data.frame()
      for (j in 1:J) {
        Reader[j,] <- rep(readerID[j], choose(I[avgIndx], 2))
        stdErr[j,] <- sqrt(2 * IndividualRdr[j]/K)
        pair <- 1
        for (i in 1:I[avgIndx]) {
          if (i == I[avgIndx]) break
          for (ip in (i + 1):I[avgIndx]) {
            dfReader[j,pair] <- K-1
            trtMeanDiffs[j, pair] <- foms[i, j] - foms[ip, j]
            trDiffNames[j,pair] <- diffTRName[pair]
            tStat[j,pair] <- trtMeanDiffs[j,pair]/stdErr[j,pair]
            PrGTt[j,pair] <- 2 * pt(abs(tStat[j,pair]), dfReader[j,pair], lower.tail = FALSE)
            CIReader[j, pair,] <- sort(c(trtMeanDiffs[j,pair] - qt(alpha/2, dfReader[j,pair]) * stdErr[j,pair], 
                                         trtMeanDiffs[j,pair] + qt(alpha/2, dfReader[j,pair]) * stdErr[j,pair]))
            rowName <- paste0(Reader[j,pair], "::", trDiffNames[j, pair])
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
      ciDiffTrtEachRdr[[avgIndx]] <- ci
    } # end of for (avgIndx in 1:2)
    
    FRRC <- list(FTests = FTests,
                 ciDiffTrt = ciDiffTrt,
                 ciAvgRdrEachTrt = ciAvgRdrEachTrt,
                 ciDiffTrtEachRdr = ciDiffTrtEachRdr)
    return(FRRC) 
  }
}




DBM_RRFC <- function(K, FOMStats, ANOVA, alpha) {
  # 
  # This was compared to JAFROC Version 4.2.1 (run using Windows-8 under VMware Fusion on iMac) 
  # Results are in inst/JAFROC
  # It checks out with Windows JAFROC for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
  # Current Iowa software does not do DBM analysis; 
  # The outputs were also compared to current
  # Iowa software and agrees for all except minor differences for FRRC (since in OR analysis the ddf is Inf instead
  # of (I-1)(K-1))
  # 
  
  if (!is.list(FOMStats$foms)) {
    # factorial one-treatment dataset
    
    modalityID <- rownames(FOMStats$foms)
    readerID <- colnames(FOMStats$foms)
    I <- length(modalityID)
    J <- length(readerID)
    
    trtMeans <-  FOMStats$trtMeans
    trtMeanDiffs  <-  FOMStats$trtMeanDiffs$Estimate
    diffTRName <- FOMStats$diffTRName
    
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
                                       row.names = modalityID, 
                                       stringsAsFactors = FALSE)
    
    return(RRFC) 
    
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    modalityID2 <- rownames(FOMStats$foms[[1]])
    modalityID1 <- rownames(FOMStats$foms[[2]])
    I1 <- length(modalityID1)
    I2 <- length(modalityID2)
    modalityID <- list(modalityID2, modalityID1) # ??
    I <- c(I2,I1)
    readerID <- colnames(FOMStats$foms[[1]])
    J <- length(readerID)
    
    FTests <- list(list(), list())
    names(FTests) <- c("AvgMod1", "AvgMod2")
    ciDiffTrt <- list(list(), list())
    names(ciDiffTrt) <- c("AvgMod1", "AvgMod2")
    ciAvgRdrEachTrt <- list(list(), list())
    names(ciAvgRdrEachTrt) <- c("AvgMod1", "AvgMod2")
    
    for (avgIndx in 1:2) {
      
      # trtMeans <-  FOMStats$trtMeans[[avgIndx]][,"Estimate"]
      trtMeans <-  FOMStats$trtMeans[[avgIndx]]
      trtMeanDiffs  <-  FOMStats$trtMeanDiffs[[avgIndx]]$Estimate
      diffTRName <- FOMStats$diffTRName[[avgIndx]]
      
      TRCanova <- ANOVA$TRCanova[[avgIndx]]
      IndividualTrt <- ANOVA$IndividualTrt[[avgIndx]]
      
      msDen <- TRCanova["TR", "MS"]
      f <- TRCanova["T", "MS"]/msDen
      ddf <- (I[avgIndx] - 1) * (J - 1)
      p <- 1 - pf(f, I[avgIndx] - 1, ddf)
      FTests[[avgIndx]] <- data.frame(DF = c((I[avgIndx]-1),ddf),
                                      MS = c(TRCanova["T", "MS"], msDen),
                                      FStat = c(f,NA),
                                      p = c(p,NA),
                                      row.names = c("Treatment", "Error"),
                                      stringsAsFactors = FALSE)
      
      stdErr <- sqrt(2 * msDen/J/K)
      tStat <- vector()
      PrGTt <- vector()
      CI <- array(dim = c(choose(I[avgIndx],2),2))
      for (i in 1:length(trtMeanDiffs)) {
        tStat[i] <- trtMeanDiffs[i]/stdErr
        PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE)
        CI[i, ] <- sort(c(trtMeanDiffs[i] - qt(alpha/2, ddf) * stdErr, 
                          trtMeanDiffs[i] + qt(alpha/2, ddf) * stdErr))
      }
      ciDiffTrt[[avgIndx]] <- data.frame(Estimate = trtMeanDiffs, 
                                         StdErr = rep(stdErr, choose(I[avgIndx], 2)), 
                                         DF = rep(ddf, choose(I[avgIndx], 2)), 
                                         t = tStat, 
                                         PrGTt = PrGTt, 
                                         CILower = CI[,1],
                                         CIUpper = CI[,2], 
                                         row.names = diffTRName,
                                         stringsAsFactors = FALSE)
      
      dfSingle <- array(dim = I[avgIndx])
      msDenSingle <- array(dim = I[avgIndx])
      stdErrSingle <- array(dim = I[avgIndx])
      CISingle <- array(dim = c(I[avgIndx], 2))
      for (i in 1:I[avgIndx]) {
        msDenSingle[i] <- IndividualTrt["msR",i+1]
        dfSingle[i] <- (J - 1)
        stdErrSingle[i] <- sqrt(msDenSingle[i]/J/K)
        CISingle[i, ] <- sort(c(trtMeans[i, "Estimate"] - qt(alpha/2, dfSingle[i]) * stdErrSingle[i], 
                                trtMeans[i, "Estimate"] + qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
      }
      ciAvgRdrEachTrt[[avgIndx]] <- data.frame(Estimate = trtMeans, 
                                               StdErr = as.vector(stdErrSingle), 
                                               DF = as.vector(dfSingle), 
                                               CILower = CISingle[,1], 
                                               CIUpper = CISingle[,2], 
                                               row.names = modalityID[[avgIndx]], 
                                               stringsAsFactors = FALSE)
    }
    
    RRFC <- list(FTests = FTests,
                 ciDiffTrt = ciDiffTrt,
                 ciAvgRdrEachTrt = ciAvgRdrEachTrt)
    
    return(RRFC) 
  }
}