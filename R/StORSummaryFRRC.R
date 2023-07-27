# 
# this checks out for dataset02 and DfFroc2Roc(dataset04), i.e., VanDyke and FedRoc datasets
# checked vs. OR-DBM MRMC 2.51 <beta> Build  20181028 </beta> output in inst/Iowa
# 
ORSummaryFRRC <- function(dataset, FOMStats, ANOVA, alpha, diffTRName) {
  #   ===========================================================================
  #     *****    Analysis 2 (OR Analysis): Fixed Readers and Random Cases     *****
  #   ===========================================================================
  #       (Results apply to the population of cases but only for the readers used in
  #        this study. Chi-square or Z tests are used; these are appropriate for 
  #        moderate or large case sample sizes.)
  #     
  readerID <- dataset$descriptions$readerID
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(dataset$ratings$NL)[3]
  
  foms <-  FOMStats$foms
  trtMeanDiffs  <-  FOMStats$trtMeanDiffs
  
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
    # X2 = (t-1)*MS(T)/[Var(error) - Cov1 + (r-1)*max(Cov2 - Cov3,0)]
    
    #   b) 95% confidence intervals and hypothesis tests (H0: difference = 0)
    #   for treatment AUC differences
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
  # 95% CI: difference +- z(.025) * StdErr
  
  
  # c) Single treatment AUC 95% confidence intervals
  # (Each analysis is based only on data for the specified treatment, i.e., on
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
    # LINE 228    c) Single treatment AUC 95% confidence intervals #
    # (Each analysis is based only on data for the specified treatment, i.e., on #
    # the specific reader ANOVA of AUCs and error-variance and Cov2 estimates.) #
    # 
    # Treatment      AUC      Std Error   95% Confidence Interval #
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
    # LINE 243: 95% CI: AUC +- z(.025) * StdErr #
    # the Var_i and Cov2_i values check out for dataset02 #
    stdErr[i] <- sqrt((ANOVA$IndividualTrt[i,"varEachTrt"] + 
                         # added max() function 8/25/20
                         (J-1)*max(ANOVA$IndividualTrt[i,"cov2EachTrt"],0))/J)
    CI[i, ] <- c(FOMStats$trtMeans[i,1] + qnorm(alpha/2) * stdErr[i],
                 FOMStats$trtMeans[i,1] + qnorm(1-alpha/2) * stdErr[i])
    rowName <- paste0("trt", modalityID[i])
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
    #   d) Single-reader 95% confidence intervals and tests (H0: difference = 0) for 
    #    treatment AUC differences.
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
          rowName <- paste0("rdr", Reader[j,pair], "::", trDiffNames[j, pair])
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
    # 95% CI: Difference +- z(.025) * StdErr
    
    FRRC$IndividualRdrVarCov1 <- ANOVA$IndividualRdr[,3:4]
  }
  
  return(FRRC) 
}