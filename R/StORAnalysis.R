StORAnalysis <- function(dataset,
                         FOM,
                         covEstMethod, 
                         analysisOption,
                         alpha,
                         FPFValue,
                         nBoots, 
                         seed,
                         details)  
  
{
  
  if (length(dim(dataset$ratings$NL)) == 4) {
    # factorial one-treatment dataset 
    
    readerID <- dataset$descriptions$readerID
    modalityID <- dataset$descriptions$modalityID
    I <- length(modalityID)
    J <- length(readerID)
    
    K2 <- dim(dataset$ratings$LL)[3]
    K <- dim(dataset$ratings$NL)[3]
    K1 <- K - K2
    if (FOM %in% c("MaxNLF", "HrSp")) {
      K <- K1
    } else if (FOM %in% c("MaxLLF", "HrSe")) {
      K <- K2
    }
    
    vc <- UtilORVarComp(dataset, FOM, covEstMethod, FPFValue, nBoots, seed)
    
    TRanova <- vc$TRanova
    VarCom <-  vc$VarCom
    IndividualTrt <- vc$IndividualTrt
    IndividualRdr <- vc$IndividualRdr
    
    ANOVA <- list()
    ANOVA$TRanova <- TRanova
    ANOVA$VarCom <- VarCom
    ANOVA$IndividualTrt <- IndividualTrt
    ANOVA$IndividualRdr <- IndividualRdr
    
    fom_ij <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    
    trtMeans <- rowMeans(fom_ij)
    trtMeans <- as.data.frame(trtMeans)
    colnames(trtMeans) <- "Estimate"
    
    modalityID <- rownames(fom_ij)
    I <- length(modalityID)
    
    trtMeanDiffs <- array(dim = choose(I, 2))
    diffTRName <- array(dim = choose(I, 2))
    ii <- 1
    for (i in 1:I) {
      if (i == I) 
        break
      for (ip in (i + 1):I) {
        trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
        diffTRName[ii] <- paste0(modalityID[i], sep = "-", modalityID[ip]) # !sic
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
    
    if (analysisOption == "RRRC") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      RRRC <- OR_RRRC(FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRRC = RRRC
      ))
    }  
    
    if (analysisOption == "FRRC") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      FRRC <- OR_FRRC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        FRRC = FRRC
      ))
    }  
    
    if (analysisOption == "RRFC") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      RRFC <- OR_RRFC(FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRFC = RRFC
      ))
    }  
    
    if (analysisOption == "ALL") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      RRRC <- OR_RRRC(FOMStats, ANOVA, alpha)
      FRRC <- OR_FRRC(K, FOMStats, ANOVA, alpha)
      RRFC <- OR_RRFC(FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRRC = RRRC,
        FRRC = FRRC,
        RRFC = RRFC
      ))
    } 
    
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    modalityID1 <- dataset$descriptions$modalityID1
    modalityID2 <- dataset$descriptions$modalityID2
    modalityID <- list(modalityID2, modalityID1)
    I1 <- length(modalityID1)
    I2 <- length(modalityID2)
    I <- c(I2,I1)
    
    readerID <- dataset$descriptions$readerID
    J <- length(readerID)
    
    K2 <- dim(dataset$ratings$LL)[4]
    K <- dim(dataset$ratings$NL)[4]
    K1 <- K - K2
    if (FOM %in% c("MaxNLF", "HrSp")) { # !!!DPC!!! need to check these FOMs
      K <- K1
    } else if (FOM %in% c("MaxLLF", "HrSe")) {
      K <- K2
    }
    
    fom_i1i2j <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    fomsAvgEachXModality <- Arr2List(dataset, fom_i1i2j)
    
    vc <- UtilORVarComp(dataset, FOM, covEstMethod, FPFValue, nBoots, seed)
    TRanova <- vc$TRanova
    VarCom <-  vc$VarCom
    IndividualTrt <- vc$IndividualTrt
    IndividualRdr <- vc$IndividualRdr
    
    ANOVA <- list()
    ANOVA$TRanova <- TRanova
    ANOVA$VarCom <- VarCom
    ANOVA$IndividualTrt <- IndividualTrt
    ANOVA$IndividualRdr <- IndividualRdr
    
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
          diffTRName[[avgIndx]][ii] <- paste0("trt", 
                                              modalityID[[avgIndx]][i], 
                                              sep = "-", "trt", 
                                              modalityID[[avgIndx]][ip])
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
    
    if (analysisOption == "RRRC") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      RRRC <- OR_RRRC(FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4], # do not show diffTreatmentName
        ANOVA = ANOVA,
        RRRC = RRRC
      ))
    }  
    
    if (analysisOption == "FRRC") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      FRRC <- OR_FRRC(K, FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        FRRC = FRRC
      ))
    }  
    
    if (analysisOption == "RRFC") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      RRFC <- OR_RRFC(FOMStats, ANOVA, alpha)
      return(list(
        FOMs = FOMStats[-4],
        ANOVA = ANOVA,
        RRFC = RRFC
      ))
    }  
    
    if (analysisOption == "ALL") {
      Explanations(dataset, FOM, method = "OR", analysisOption = analysisOption, details)
      RRRC <- OR_RRRC(FOMStats, ANOVA, alpha)
      FRRC <- OR_FRRC(K, FOMStats, ANOVA, alpha)
      RRFC <- OR_RRFC(FOMStats, ANOVA, alpha)
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




OR_RRRC<- function(FOMStats, ANOVA, alpha) {
  
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
    
    #   b) 1-alpha confidence intervals and hypothesis tests (H0: difference = 0)
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
    # 1-alpha CI: Difference +- t(.025;df) * StdErr
    
    # if (dataset$descriptions$design == "FCTRL") {
    #   c) Single-modality 1-alpha confidence intervals
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
    # 1-alpha CI: AUC +- t(.025;df) * StdErr
    
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
    
    return(RRRC) 
  }  
}



OR_FRRC <- function(K, FOMStats, ANOVA, alpha) {
  # 
  # this checks out for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
  # checked vs. OR-DBM MRMC 2.51 <beta> Build  20181028 </beta> output in inst/Iowa
  # 
  
  if (!is.list(FOMStats$foms)) {
    # factorial one-treatment dataset 
    
    #   ===========================================================================
    #     *****    Analysis 2 (OR Analysis): Fixed Readers and Random Cases     *****
    #   ===========================================================================
    #       (Results apply to the population of cases but only for the readers used in
    #        this study. Chi-square or Z tests are used; these are appropriate for 
    #        moderate or large case sample sizes.)
    #     
    foms <-  FOMStats$foms
    modalityID <- rownames(FOMStats$foms)
    readerID <- colnames(FOMStats$foms)
    I <- length(modalityID)
    J <- length(readerID)
    
    trtMeanDiffs  <-  FOMStats$trtMeanDiffs
    diffTRName <- rownames(FOMStats$trtMeanDiffs)
    
    #  a) Chi-square test for H0: Treatments have the same AUC
    #  Note: The chi-square statistic is denoted by X2 or by X2(df), where df is its 
    #  corresponding degrees of freedom.
    # 
    
    # Need to check presence of max in following formula
    # max() is not present in formulae, but cant hurt, in my opinion
    FRRC <- list()
    if (I > 1) {
      if (J > 1) {
        msDen <- ANOVA$VarCom["Var","Estimates"] - ANOVA$VarCom["Cov1","Estimates"] + 
          (J - 1) * max(ANOVA$VarCom["Cov2","Estimates"] - ANOVA$VarCom["Cov3","Estimates"] ,0)
      }
      # following has to handled explicitly as otherwise it will return NA
      else msDen <- ANOVA$VarCom["Var","Estimates"] - ANOVA$VarCom["Cov1","Estimates"]
      chisqVal <- (I-1)*ANOVA$TRanova["T","MS"]/msDen
      p <- 1 - pchisq(chisqVal, I - 1)
      FRRC$FTests <- data.frame(MS = c(ANOVA$TRanova["T", "MS"], msDen),
                                Chisq = c(chisqVal,NA),
                                DF = c(I - 1, NA),
                                p = c(p,NA),
                                row.names = c("Treatment", "Error"),
                                stringsAsFactors = FALSE)
      
      #   b) 1-alpha confidence intervals and hypothesis tests (H0: difference = 0)
      #   for modality AUC differences
      stdErr <- sqrt(2 * msDen/J)
      zStat <- vector()
      PrGTz <- vector()
      CI <- array(dim = c(choose(I,2),2))
      for (i in 1:choose(I,2)) {
        zStat[i] <- trtMeanDiffs[i,1]/stdErr
        PrGTz[i] <- 2 * pnorm(abs(zStat[i]), lower.tail = FALSE)
        CI[i, ] <- c(trtMeanDiffs[i,1] + qnorm(alpha/2) * stdErr, 
                     trtMeanDiffs[i,1] + qnorm(1-alpha/2) * stdErr)
      }
      FRRC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                                   StdErr = rep(stdErr, choose(I, 2)),
                                   z = zStat, 
                                   PrGTz = PrGTz, 
                                   CILower = CI[,1],
                                   CIUpper = CI[,2], 
                                   row.names = diffTRName,
                                   stringsAsFactors = FALSE)
    }
    # StdErr = sqrt{2/r * [(Var(error) - Cov1 + (r-1)*max(Cov2 - Cov3,0)]}
    # 1-alpha CI: difference +- z(.025) * StdErr
    
    
    # c) Single modality AUC 1-alpha confidence intervals
    # (Each analysis is based only on data for the specified modality, i.e., on
    #   the specific reader ANOVA of AUCs and error-variance and Cov2 estimates.)
    # 
    # Hillis 2007 5.3. not applicable here
    # FRRC: Rdr Avg FOM for each modality obeys a normal distribution
    stdErr <- vector()
    df <- vector()
    CI <- array(dim = c(I,2))
    ci <- data.frame()
    for (i in 1:I) {
      df[i] <- K - 1
      # Following equation is out of OR-DBM MRMC 2.51 <beta> Build  20181028 </beta> #
      # TBA Need a better reference #
      # See for example, inst/Iowa/VanDyke.txt, lines 228-243; shown next: #
      # RStudio debugger buggy when I have these comments, does not stop at break points #
      # LINE 228    c) Single modality AUC 1-alpha confidence intervals #
      # (Each analysis is based only on data for the specified modality, i.e., on #
      # the specific reader ANOVA of AUCs and error-variance and Cov2 estimates.) #
      # 
      # Treatment      AUC      Std Error   1-alpha Confidence Interval #
      # ----------  ----------  ----------  ------------------------- #
      #   1  0.89703704  0.02428971  (0.84943008 , 0.94464399) #
      #   2  0.94083736  0.01677632  (0.90795637 , 0.97371835) #
      # 
      # Treatment  Var(Error)     Cov2   #
      # ----------  ----------  ---------- #
      #   1  0.00101410  0.00048396 #
      #   2  0.00059047  0.00020419 #
      # 
      #           StdErr = sqrt{1/r * [Var(error) + (r-1)*max(Cov2,0)]} #
      # LINE 243: 1-alpha CI: AUC +- z(.025) * StdErr #
      # the Var_i and Cov2_i values check out for dataset02 #
      stdErr[i] <- sqrt((ANOVA$IndividualTrt[i,"varEachTrt"] + 
                           # added max() function 8/25/20
                           (J-1)*max(ANOVA$IndividualTrt[i,"cov2EachTrt"],0))/J)
      CI[i, ] <- c(FOMStats$trtMeans[i,1] + qnorm(alpha/2) * stdErr[i],
                   FOMStats$trtMeans[i,1] + qnorm(1-alpha/2) * stdErr[i])
      rowName <- modalityID[i]
      ci <- rbind(ci, data.frame(Estimate = FOMStats$trtMeans[i,1], 
                                 StdErr = stdErr[i],
                                 DF = df[i],
                                 CILower = CI[i,1],
                                 CIUpper = CI[i,2],
                                 row.names = rowName,
                                 stringsAsFactors = FALSE))
    }
    FRRC$ciAvgRdrEachTrt <- ci
    
    if (I > 1) {
      #   d) Single-reader 1-alpha confidence intervals and tests (H0: difference = 0) for 
      #    modality AUC differences.
      #    (Each analysis is based only on data for the specified reader, i.e, on the 
      #    reader-specific AUC, error-variance and Cov1 estimates.)
      trtMeanDiffs <- array(dim = c(J, choose(I, 2)))
      Reader <- array(dim = c(J, choose(I, 2)))
      stdErr <- array(dim = c(J, choose(I, 2)))
      zStat <- array(dim = c(J, choose(I, 2)))
      trDiffNames <- array(dim = c(J, choose(I, 2)))
      PrGTz <- array(dim = c(J, choose(I, 2)))
      CIReader <- array(dim = c(J, choose(I, 2),2))
      ci <- data.frame()
      for (j in 1:J) {
        Reader[j,] <- rep(readerID[j], choose(I, 2))
        stdErr[j,] <- sqrt(2 * (ANOVA$IndividualRdr[j,"varEachRdr"] - ANOVA$IndividualRdr[j,"cov1EachRdr"]))
        pair <- 1
        for (i in 1:I) {
          if (i == I) break
          for (ip in (i + 1):I) {
            trtMeanDiffs[j, pair] <- foms[i, j] - foms[ip, j]
            trDiffNames[j,pair] <- diffTRName[pair]
            zStat[j,pair] <- trtMeanDiffs[j,pair]/stdErr[j,pair]
            PrGTz[j,pair] <- 2 * pnorm(abs(zStat[j,pair]), lower.tail = FALSE)
            CIReader[j, pair,] <- c(trtMeanDiffs[j,pair] + qnorm(alpha/2) * stdErr[j,pair], 
                                    trtMeanDiffs[j,pair] + qnorm(1-alpha/2) * stdErr[j,pair])
            rowName <- paste0(Reader[j,pair], "::", trDiffNames[j, pair])
            ci <- rbind(ci, data.frame(Estimate = trtMeanDiffs[j, pair], 
                                       StdErr = stdErr[j,pair], 
                                       z = zStat[j, pair], 
                                       PrGTz = PrGTz[j, pair], 
                                       CILower = CIReader[j, pair,1],
                                       CIUpper = CIReader[j, pair,2],
                                       row.names = rowName,
                                       stringsAsFactors = FALSE))
            pair <- pair + 1
          }
        }
      }
      FRRC$ciDiffTrtEachRdr <- ci
      # StdErr = sqrt[2*(Var(error) - Cov1)]
      # 1-alpha CI: Difference +- z(.025) * StdErr
      
      FRRC$IndividualRdrVarCov1 <- ANOVA$IndividualRdr[,3:4]
    }
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    foms <- FOMStats$foms
    I1 <- dim(foms[[2]])[1]
    I2 <- dim(foms[[1]])[1]
    J <- dim(foms[[1]])[2]
    
    IndividualTrt <- ANOVA$IndividualTrt
    cov2EachTrt <- IndividualTrt[[1]][4]
    varEachTrt <- IndividualTrt[[1]][3]
    trtMeans <- FOMStats$trtMeans
    trtMeanDiffs <- FOMStats$trtMeanDiffs
    TRanova <- ANOVA$TRanova
    VarCom <- ANOVA$VarCom
    
    I <- c(I2,I1)
    modalityID1 <- rownames(FOMStats$foms[[1]])
    modalityID2 <- rownames(FOMStats$foms[[2]])
    modalityID <- list(modalityID1, modalityID2)
    readerID <- colnames(FOMStats$foms[[1]])
    
    FTests <- list(list(), list())
    names(FTests) <- c("AvgMod1", "AvgMod2")
    ciDiffTrt <- list(list(), list())
    names(ciDiffTrt) <- c("AvgMod1", "AvgMod2")
    ciAvgRdrEachTrt <- list(list(), list())
    names(ciAvgRdrEachTrt) <- c("AvgMod1", "AvgMod2")
    ciDiffTrtEachRdr <- list(list(), list())
    names(ciDiffTrtEachRdr) <- c("AvgMod1", "AvgMod2")
    IndividualRdrVarCov1 <- list(list(), list())
    names(IndividualRdrVarCov1) <- c("AvgMod1", "AvgMod2")
    
    for (AvgIndx in 1:2) {
      if (I[AvgIndx] > 1) {
        if (J > 1) {
          msDen <- ANOVA$VarCom[[AvgIndx]]["Var","Estimates"] - ANOVA$VarCom[[AvgIndx]]["Cov1","Estimates"] + 
            (J - 1) * max(ANOVA$VarCom[[AvgIndx]]["Cov2","Estimates"] - ANOVA$VarCom[[AvgIndx]]["Cov3","Estimates"] ,0)
        }
        # following has to handled explicitly as otherwise it will return NA
        else msDen <- ANOVA$VarCom[[AvgIndx]]["Var","Estimates"] - ANOVA$VarCom[[AvgIndx]]["Cov1","Estimates"]
        chisqVal <- (I[AvgIndx]-1)*ANOVA$TRanova[[AvgIndx]]["T","MS"]/msDen
        p <- 1 - pchisq(chisqVal, I[AvgIndx] - 1)
        FTests[[AvgIndx]] <- data.frame(MS = c(ANOVA$TRanova[[AvgIndx]]["T", "MS"], msDen),
                                        Chisq = c(chisqVal,NA),
                                        DF = c(I[AvgIndx] - 1, NA),
                                        p = c(p,NA),
                                        row.names = c("Treatment", "Error"),
                                        stringsAsFactors = FALSE)
        
        stdErr <- sqrt(2 * msDen/J)
        zStat <- vector()
        PrGTz <- vector()
        CI <- array(dim = c(choose(I[AvgIndx],2),2))
        diffTRName <- rownames(FOMStats$trtMeanDiffs[[AvgIndx]])
        if (NROW(trtMeanDiffs[[AvgIndx]]) > 1) {
          for (i in 1:choose(I[AvgIndx],2)) {
            zStat[i] <- trtMeanDiffs[[AvgIndx]][i,1]/stdErr
            PrGTz[i] <- 2 * pnorm(abs(zStat[i]), lower.tail = FALSE)
            CI[i, ] <- c(trtMeanDiffs[[AvgIndx]][i,1] + qnorm(alpha/2) * stdErr, 
                         trtMeanDiffs[[AvgIndx]][i,1] + qnorm(1-alpha/2) * stdErr)
          } 
          ciDiffTrt[[AvgIndx]] <- data.frame(Estimate = trtMeanDiffs[[AvgIndx]], 
                                             StdErr = rep(stdErr, choose(I[AvgIndx], 2)),
                                             z = zStat, 
                                             PrGTz = PrGTz, 
                                             CILower = CI[,1],
                                             CIUpper = CI[,2], 
                                             row.names = diffTRName,
                                             stringsAsFactors = FALSE)
        } else {
          zStat <- trtMeanDiffs[[AvgIndx]]/stdErr
          PrGTz <- 2 * pnorm(abs(zStat[i]), lower.tail = FALSE)
          CI <- c(trtMeanDiffs[[AvgIndx]] + qnorm(alpha/2) * stdErr, 
                  trtMeanDiffs[[AvgIndx]] + qnorm(1-alpha/2) * stdErr)
          ciDiffTrt[[AvgIndx]] <- data.frame(Estimate = trtMeanDiffs[[AvgIndx]], 
                                             StdErr = rep(stdErr, choose(I[AvgIndx], 2)),
                                             z = zStat, 
                                             PrGTz = PrGTz, 
                                             CILower = CI[1],
                                             CIUpper = CI[2], 
                                             row.names = diffTRName,
                                             stringsAsFactors = FALSE)
        }
        
        stdErr <- vector()
        df <- vector()
        CI <- array(dim = c(I[AvgIndx],2))
        ci <- data.frame()
        for (i in 1:I[AvgIndx]) {
          df[i] <- K - 1
          stdErr[i] <- sqrt((ANOVA$IndividualTrt[[AvgIndx]][i,"varEachTrt"] + 
                               (J-1)*max(ANOVA$IndividualTrt[[AvgIndx]][i,"cov2EachTrt"],0))/J)
          CI[i, ] <- c(FOMStats$trtMeans[[AvgIndx]][i,1] + qnorm(alpha/2) * stdErr[i],
                       FOMStats$trtMeans[[AvgIndx]][i,1] + qnorm(1-alpha/2) * stdErr[i])
          rowName <- paste0(modalityID[[AvgIndx]][i])
          ci <- rbind(ci, data.frame(Estimate = FOMStats$trtMeans[[AvgIndx]][i,1], 
                                     StdErr = stdErr[i],
                                     DF = df[i],
                                     CILower = CI[i,1],
                                     CIUpper = CI[i,2],
                                     row.names = rowName,
                                     stringsAsFactors = FALSE))
        }
        ciAvgRdrEachTrt[[AvgIndx]] <- ci
        
        if (I[AvgIndx] > 1) {
          trtMeanDiffs <- array(dim = c(J, choose(I[AvgIndx], 2)))
          Reader <- array(dim = c(J, choose(I[AvgIndx], 2)))
          stdErr <- array(dim = c(J, choose(I[AvgIndx], 2)))
          zStat <- array(dim = c(J, choose(I[AvgIndx], 2)))
          trDiffNames <- array(dim = c(J, choose(I[AvgIndx], 2)))
          PrGTz <- array(dim = c(J, choose(I[AvgIndx], 2)))
          CIReader <- array(dim = c(J, choose(I[AvgIndx], 2),2))
          ci <- data.frame()
          for (j in 1:J) {
            Reader[j,] <- rep(readerID[j], choose(I[AvgIndx], 2))
            stdErr[j,] <- sqrt(2 * (ANOVA$IndividualRdr[[AvgIndx]][j,"varEachRdr"] - 
                                      ANOVA$IndividualRdr[[AvgIndx]][j,"cov1EachRdr"]))
            pair <- 1
            for (i in 1:I[AvgIndx]) {
              if (i == I[AvgIndx]) break
              for (ip in (i + 1):I[AvgIndx]) {
                trtMeanDiffs[j, pair] <- foms[[AvgIndx]][i, j] - foms[[AvgIndx]][ip, j]
                trDiffNames[j,pair] <- diffTRName[pair]
                zStat[j,pair] <- trtMeanDiffs[j,pair]/stdErr[j,pair]
                PrGTz[j,pair] <- 2 * pnorm(abs(zStat[j,pair]), lower.tail = FALSE)
                CIReader[j, pair,] <- c(trtMeanDiffs[j,pair] + qnorm(alpha/2) * stdErr[j,pair], 
                                        trtMeanDiffs[j,pair] + qnorm(1-alpha/2) * stdErr[j,pair])
                rowName <- paste0(Reader[j,pair], "::", trDiffNames[j, pair])
                ci <- rbind(ci, data.frame(Estimate = trtMeanDiffs[j, pair], 
                                           StdErr = stdErr[j,pair], 
                                           z = zStat[j, pair], 
                                           PrGTz = PrGTz[j, pair], 
                                           CILower = CIReader[j, pair,1],
                                           CIUpper = CIReader[j, pair,2],
                                           row.names = rowName,
                                           stringsAsFactors = FALSE))
                pair <- pair + 1
              }
            }
          }
          ciDiffTrtEachRdr[[AvgIndx]] <- ci
          IndividualRdrVarCov1[[AvgIndx]] <- ANOVA$IndividualRdr[[AvgIndx]][,3:4]
        }
      }
    }
    FRRC <- list(FTests = FTests,
                 ciDiffTrt = ciDiffTrt,
                 ciAvgRdrEachTrt = ciAvgRdrEachTrt,
                 ciDiffTrtEachRdr = ciDiffTrtEachRdr,
                 IndividualRdrVarCov1 = IndividualRdrVarCov1)
  }
  
  return(FRRC) 
}




OR_RRFC <- function(FOMStats, ANOVA, alpha) {
  # 
  # this checks out for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
  # checked vs. OR-DBM MRMC 2.51 <beta> Build  20181028 </beta> output in inst/Iowa
  # 
  
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
    # These results are from using the OR sampling model, but treating reader as a random 
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
    
    #   b) 1-alpha confidence intervals and hypothesis tests (H0: difference = 0)
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
    # 1-alpha CI: Difference +- t(.025;df) * StdErr
    # Note: If there are only 2 treatments, this is equivalent to a paired t-test applied
    # to the AUCs
    
    #   c) Single modality AUC 1-alpha confidence intervals
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
    # 1-alpha CI: AUC +- t(.025;df) * StdErr
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




#' @importFrom stats runif
varCompBS <- function(dataset, FOM, FPFValue, nBoots, seed) 
{
  stop("not yet implemented: varCompBS")
  
  # if (dataset$descriptions$design != "FCTRL") stop("This functions requires a factorial dataset")  
  # 
  # set.seed(seed) ## added 4/28/20, to test reproducibility with RJafrocBook code
  # NL <- dataset$ratings$NL
  # LL <- dataset$ratings$LL
  # perCase <- dataset$lesions$perCase
  # IDs <- dataset$lesions$IDs
  # weights <- dataset$lesions$weights
  # 
  # I <- length(NL[,1,1,1])
  # J <- length(NL[1,,1,1])
  # K <- length(NL[1,1,,1])
  # K2 <- length(LL[1,1,,1])
  # K1 <- (K - K2)
  # maxNL <- length(NL[1,1,1,])
  # maxLL <- length(LL[1,1,1,])
  # 
  # if (FOM %in% c("MaxNLF", "HrSp")) {
  #   stop("This needs fixing")
  #   fomBsArray <- array(dim = c(I, J, nBoots))
  #   for (b in 1:nBoots) {
  #     kBs <- ceiling(runif(K1) * K1)
  #     for (i in 1:I) {
  #       for (j in 1:J) {
  #         NLbs <- NL[i, j, kBs, ]
  #         LLbs <- LL[i, j, , ]
  #         dim(NLbs) <- c(K1, maxNL)
  #         dim(LLbs) <- c(K2, maxLL)
  #         fomBsArray[i, j, b] <- MyFom_ij(NLbs, LLbs, 
  #                                         perCase, IDs, 
  #                                         weights, maxNL, 
  #                                         maxLL, K1, K2, 
  #                                         FOM, FPFValue)
  #       }
  #     }
  #   }
  # } else if (FOM %in% c("MaxLLF", "HrSe")) {
  #   stop("This needs fixing")
  #   fomBsArray <- array(dim = c(I, J, nBoots))
  #   for (b in 1:nBoots) {
  #     kBs <- ceiling(runif(K2) * K2)
  #     for (i in 1:I) {
  #       for (j in 1:J) {
  #         NLbs <- NL[i, j, c(1:K1, (kBs + K1)), ]
  #         LLbs <- LL[i, j, kBs, ]
  #         dim(NLbs) <- c(K, maxNL)
  #         dim(LLbs) <- c(K2, maxLL)
  #         lesionIDBs <- IDs[kBs, ]
  #         dim(lesionIDBs) <- c(K2, maxLL)
  #         lesionWeightBs <- weights[kBs, ]
  #         dim(lesionWeightBs) <- c(K2, maxLL)
  #         fomBsArray[i, j, b] <- MyFom_ij(NLbs, LLbs, 
  #                                         perCase[kBs], lesionIDBs, 
  #                                         lesionWeightBs, maxNL, maxLL, 
  #                                         K1, K2, FOM, FPFValue)
  #       }
  #     }
  #   }
  # } else { # original code had errors; see Fadi RRRC code; Aug 9, 2017 !!dpc!!!
  #   ## however, following code needs checking
  #   ##stop("this code needs checking; contact Dr. Chakraborty with dataset and code that lands here; 8/9/2017")
  #   fomBsArray <- array(dim = c(I, J, nBoots))
  #   for (b in 1:nBoots) {
  #     k1bs <- ceiling(runif(K1) * K1)
  #     k2bs <- ceiling(runif(K2) * K2)
  #     for (i in 1:I) {
  #       for (j in 1:J) {
  #         NLbs <- NL[i, j, c(k1bs, k2bs + K1), ]
  #         lesionVectorbs <- perCase[k2bs]            
  #         LLbs <- LL[i, j, k2bs,1:max(lesionVectorbs)] 
  #         dim(NLbs) <- c(K, maxNL)
  #         dim(LLbs) <- c(K2, max(lesionVectorbs))  
  #         lesionIDBs <- IDs[k2bs, ]
  #         dim(lesionIDBs) <- c(K2, maxLL)
  #         lesionWeightBs <- weights[k2bs, ]
  #         dim(lesionWeightBs) <- c(K2, maxLL)
  #         fomBsArray[i, j, b] <- MyFom_ij(NLbs, LLbs, lesionVectorbs, lesionIDBs, 
  #                                         lesionWeightBs, maxNL, maxLL, K1, K2, FOM, FPFValue)
  #       }
  #     }
  #   }
  # }
  # 
  # Cov <- FOM2ORVarComp(fomBsArray, varInflFactor = FALSE)
  # Var <- Cov$Var
  # Cov1 <- Cov$Cov1
  # Cov2 <- Cov$Cov2
  # Cov3 <- Cov$Cov3
  # 
  # return(list(
  #   Var = Var, 
  #   Cov1 = Cov1, 
  #   Cov2 = Cov2, 
  #   Cov3 = Cov3
  # ))
  
}




varCompDeLong <- function(FOM)
{
  stop("not yet implemented: varCompDeLong")
  
  # if (dataset$descriptions$design != "FCTRL") stop("This functions requires a factorial dataset")  
  # if (FOM != "Wilcoxon") stop("This functions requires FOM = `Wilcoxon`,\n")  
  # 
  # UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  # NL <- dataset$ratings$NL
  # LL <- dataset$ratings$LL
  # perCase <- dataset$lesions$perCase
  # 
  # I <- length(NL[,1,1,1])
  # J <- length(NL[1,,1,1])
  # K <- length(NL[1,1,,1])
  # K2 <- length(LL[1,1,,1])
  # K1 <- (K - K2)
  # maxNL <- length(NL[1,1,1,])
  # maxLL <- length(LL[1,1,1,])
  # if ((maxNL != 1) || (maxLL != 1)) stop("dataset error in varCompDeLong")
  # 
  # fomArray <- UtilFigureOfMerit(dataset, FOM)
  # 
  # if (!FOM %in% c("Wilcoxon", "HrAuc", "ROI")) 
  #   stop("DeLong\"s method can only be used for trapezoidal figures of merit.")
  # 
  # if (FOM == "ROI") {
  #   kI01 <- which(apply((NL[1, 1, , ] != UNINITIALIZED), 1, any))
  #   numKI01 <- rowSums((NL[1, 1, , ] != UNINITIALIZED))
  #   I01 <- length(kI01)
  #   I10 <- K2
  #   N <- sum((NL[1, 1, , ] != UNINITIALIZED))
  #   M <- sum(perCase)
  #   V01 <- array(dim = c(I, J, I01, maxNL))
  #   V10 <- array(dim = c(I, J, I10, maxLL))
  #   for (i in 1:I) {
  #     for (j in 1:J) {
  #       for (k in 1:I10) {
  #         for (el in 1:perCase[k]) {
  #           V10[i, j, k, el] <- (sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) < LL[i, j, k, el]) 
  #                                + 0.5 * sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) == LL[i, j, k, el]))/N
  #         }
  #       }
  #       for (k in 1:I01) {
  #         for (el in 1:maxNL) {
  #           if (NL[i, j, kI01[k], el] == UNINITIALIZED) 
  #             next
  #           V01[i, j, k, el] <- (sum(NL[i, j, kI01[k], el] < as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])) 
  #                                + 0.5 * sum(NL[i, j, kI01[k], el] == as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])))/M
  #         }
  #       }
  #     }
  #   }
  #   s10 <- array(0, dim = c(I, I, J, J))
  #   s01 <- array(0, dim = c(I, I, J, J))
  #   s11 <- array(0, dim = c(I, I, J, J))
  #   for (i in 1:I) {
  #     for (ip in 1:I) {
  #       for (j in 1:J) {
  #         for (jp in 1:J) {
  #           for (k in 1:I10) {
  #             s10[i, ip, j, jp] <- (s10[i, ip, j, jp]
  #                                   + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])])
  #                                      - perCase[k] * fomArray[i, j])
  #                                   * (sum(V10[ip, jp, k, !is.na(V10[ip, jp, k, ])]) 
  #                                      - perCase[k] * fomArray[ip, jp]))
  #           }
  #           for (k in 1:I01) {
  #             s01[i, ip, j, jp] <- (s01[i, ip, j, jp] 
  #                                   + (sum(V01[i, j, k, !is.na(V01[i, j, k, ])]) 
  #                                      - numKI01[kI01[k]] * fomArray[i, j]) 
  #                                   * (sum(V01[ip, jp, k, !is.na(V01[ip, jp, k, ])]) 
  #                                      - numKI01[kI01[k]] * fomArray[ip, jp]))
  #           }
  #           allAbn <- 0
  #           for (k in 1:K2) {
  #             if (all(NL[ip, jp, k + K1, ] == UNINITIALIZED)) {
  #               allAbn <- allAbn + 1
  #               next
  #             }                  
  #             s11[i, ip, j, jp] <- (s11[i, ip, j, jp] 
  #                                   + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])]) 
  #                                      - perCase[k] * fomArray[i, j]) 
  #                                   * (sum(V01[ip, jp, k + K1 - allAbn, !is.na(V01[ip, jp, k + K1 - allAbn, ])]) 
  #                                      - numKI01[K1 + k] * fomArray[ip, jp]))
  #           }
  #         }
  #       }
  #     }
  #   }
  #   s10 <- s10 * I10/(I10 - 1)/M
  #   s01 <- s01 * I01/(I01 - 1)/N
  #   s11 <- s11 * K/(K - 1)
  #   S <- array(0, dim = c(I, I, J, J))
  #   for (i in 1:I) {
  #     for (ip in 1:I) {
  #       for (j in 1:J) {
  #         for (jp in 1:J) {
  #           S[i, ip, j, jp] <- s10[i, ip, j, jp]/M + s01[i, ip, j, jp]/N + s11[i, ip, j, jp]/(M * N) + s11[ip, i, jp, j]/(M * N)
  #         }
  #       }
  #     }
  #   }
  # } else {
  #   # ROC
  #   V10 <- array(dim = c(I, J, K2))
  #   V01 <- array(dim = c(I, J, K1))
  #   for (i in 1:I) {
  #     for (j in 1:J) {
  #       nl <- NL[i, j, 1:K1, ]
  #       ll <- cbind(NL[i, j, (K1 + 1):K, ], LL[i, j, , ])
  #       dim(nl) <- c(K1, maxNL)
  #       dim(ll) <- c(K2, maxNL + maxLL)
  #       fp <- apply(nl, 1, max)
  #       tp <- apply(ll, 1, max)
  #       for (k in 1:K2) {
  #         V10[i, j, k] <- (sum(fp < tp[k]) + 0.5 * sum(fp == tp[k]))/K1
  #       }
  #       for (k in 1:K1) {
  #         V01[i, j, k] <- (sum(fp[k] < tp) + 0.5 * sum(fp[k] == tp))/K2
  #       }
  #     }
  #   }
  #   s10 <- array(dim = c(I, I, J, J))
  #   s01 <- array(dim = c(I, I, J, J))
  #   for (i in 1:I) {
  #     for (ip in 1:I) {
  #       for (j in 1:J) {
  #         for (jp in 1:J) {
  #           s10[i, ip, j, jp] <- sum((V10[i, j, ] - fomArray[i, j]) * (V10[ip, jp, ] - fomArray[ip, jp]))/(K2 - 1)
  #           s01[i, ip, j, jp] <- sum((V01[i, j, ] - fomArray[i, j]) * (V01[ip, jp, ] - fomArray[ip, jp]))/(K1 - 1)
  #         }
  #       }
  #     }
  #   }
  #   S <- s10/K2 + s01/K1
  # }
  # 
  # covariances <- S
  # Var <- 0
  # count <- 0
  # for (i in 1:I) {
  #   for (j in 1:J) {
  #     Var <- Var + covariances[i, i, j, j]
  #     count <- count + 1
  #   }
  # }
  # if (count > 0) Var <- Var/count else Var <- 0
  # 
  # Cov1 <- 0
  # count <- 0
  # for (i in 1:I) {
  #   for (ip in 1:I) {
  #     for (j in 1:J) {
  #       if (ip != i) {
  #         Cov1 <- Cov1 + covariances[i, ip, j, j]
  #         count <- count + 1
  #       }
  #     }
  #   }
  # }
  # if (count > 0) Cov1 <- Cov1/count else Cov1 <- 0
  # 
  # Cov2 <- 0
  # count <- 0
  # for (i in 1:I) {
  #   for (j in 1:J) {
  #     for (jp in 1:J) {
  #       if (j != jp) {
  #         Cov2 <- Cov2 + covariances[i, i, j, jp]
  #         count <- count + 1
  #       }
  #     }
  #   }
  # }
  # if (count > 0) Cov2 <- Cov2/count else Cov2 <- 0
  # 
  # Cov3 <- 0
  # count <- 0
  # for (i in 1:I) {
  #   for (ip in 1:I) {
  #     if (i != ip) {
  #       for (j in 1:J) {
  #         for (jp in 1:J) {
  #           if (j != jp) {
  #             Cov3 <- Cov3 + covariances[i, ip, j, jp]
  #             count <- count + 1
  #           }
  #         }
  #       }
  #     }
  #   }
  # }
  # if (count > 0) Cov3 <- Cov3/count else Cov3 <- 0
  # 
  # return(list(
  #   Var = Var, 
  #   Cov1 = Cov1, 
  #   Cov2 = Cov2, 
  #   Cov3 = Cov3))
}

