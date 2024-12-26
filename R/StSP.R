#' Significance testing for unbalanced split-plot datasets
#' 
#' @description  Obuchowski-Rockette significance testing for balanced
#'    split-plot datasets as described in Hillis 2014. 
#'
#' @param dataset 
#'      
#' \itemize{
#'     \item The dataset, see \code{\link{RJafroc-package}}. 
#'     \item \code{dataset$descriptions$design} must be "SPLIT-PLOT-A" or "SPLIT-PLOT-C". 
#'     \item SPLIT-PLOT-A = Reader nested within test; Hillis 2014 Table VII part (a). 
#'     \item SPLIT-PLOT-C = Case nested within reader; Hillis 2014 Table VII part (c). 
#' }
#'      
#' @param FOM The figure of merit
#' 
#' @param alpha The significance level of the test, default 0.05
#'    
#' @param analysisOption Factors regarded as random vs. those regarded as fixed:
#' \itemize{ 
#'    \item \code{"RRRC"} = random-reader random case, the default,
#'    \item \code{"FRRC"} = fixed-reader random case, 
#'    \item \code{"RRFC"} = random-reader fixed case, 
#' } 
#'    
#' @return Results of the analysis
#' 
#' 
#' @note Two toy balanced split-plot ROC datasets are in directory 
#'   \code{inst/extdata/toyFiles/ROC}:
#' \itemize{ 
#'    \item SPLIT-PLOT-A dataset \code{rocSpA.xlsx}
#'    \item SPLIT-PLOT-C dataset \code{rocSpC.xlsx}
#' }
#' 
#' 
#' @references 
#' Hillis SL (2014) A marginal-mean ANOVA approach for analyzing multireader
#' multicase radiological imaging data, Statistics in medicine 33, 330-360.
#' 
#'
#'
#' @examples
#' ## Analyze included dataset 
#' fileName <- system.file("extdata", "/toyFiles/ROC/rocSpA.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' dsSpA <- DfReadSP_A(fileName)
#' ret <- StSP(dsSpA, FOM = "Wilcoxon")
#' 
#' ## Analyze embedded dataset 
#' ret <- StSP(datasetFROCSpC, FOM = "wAFROC")
#' 
#' 
#'
#' @importFrom stats pf pt qt cov pchisq pnorm qnorm
#' @importFrom Rcpp evalCpp
#' @useDynLib RJafroc
#'
#'      
#' @export

# 11/5/2024 focus for now on balanced split plot A dataset
StSP <- function(dataset, FOM, alpha = 0.05, analysisOption = "RRRC")
{
  
  if (dataset$descriptions$design == "SPLIT-PLOT-A") {
    
    return(OR_SP_A_UNB(dataset, FOM, alpha, analysisOption))
    
  } else if (dataset$descriptions$design == "SPLIT-PLOT-C") {
    browser()
    stop("Code under development\n")
    return(OR_SP_C(dataset, FOM, alpha, analysisOption))
    
  } else stop("StSP() function requires a split plot A or C design")
}



FOMijk2VarCov <- function(resampleFOMijk, varInflFactor) {
  I <- dim(resampleFOMijk)[1]
  J <- dim(resampleFOMijk)[2]
  K <- dim(resampleFOMijk)[3]
  
  covariances <- array(dim = c(I, I, J, J))
  
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          covariances[i, ip, j, jp] <- cov(resampleFOMijk[i, j, ], resampleFOMijk[ip, jp, ])
        }
      }
    }
  }
  
  Var <- 0
  count <- 0
  I <- dim(covariances)[1]
  J <- dim(covariances)[3]
  for (i in 1:I) {
    for (j in 1:J) {
      Var <- Var + covariances[i, i, j, j]
      count <- count + 1
    }
  }
  if (count > 0) Var <- Var/count else Var <- 0
  
  Cov1 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        if (ip != i) {
          Cov1 <- Cov1 + covariances[i, ip, j, j]
          count <- count + 1
        }
      }
    }
  }
  if (count > 0) Cov1 <- Cov1/count else Cov1 <- 0
  
  Cov2 <- 0
  count <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (jp in 1:J) {
        if (j != jp) {
          Cov2 <- Cov2 + covariances[i, i, j, jp]
          count <- count + 1
        }
      }
    }
  }
  if (count > 0) Cov2 <- Cov2/count else Cov2 <- 0
  
  Cov3 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      if (i != ip) {
        for (j in 1:J) {
          for (jp in 1:J) {
            if (j != jp) {
              Cov3 <- Cov3 + covariances[i, ip, j, jp]
              count <- count + 1
            }
          }
        }
      }
    }
  }
  if (count > 0) Cov3 <- Cov3/count else Cov3 <- 0
  
  if (varInflFactor)  {
    Var <-  Var * (K - 1)^2/K  # see paper by Efron and Stein 
    Cov1 <-  Cov1 * (K - 1)^2/K
    Cov2  <-  Cov2 * (K - 1)^2/K
    Cov3 <-  Cov3  * (K - 1)^2/K
  }
  
  return(list(
    Var = Var,
    Cov1 = Cov1,
    Cov2 = Cov2,
    Cov3 = Cov3
  ))
  
}



OR_FRRC_SP <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
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
  
  foms <-  FOMs$foms
  trtMeanDiffs  <-  FOMs$trtMeanDiffs
  
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
    CI[i, ] <- c(FOMs$trtMeans[i,1] + qnorm(alpha/2) * stdErr[i],
                 FOMs$trtMeans[i,1] + qnorm(1-alpha/2) * stdErr[i])
    rowName <- paste0("trt", modalityID[i])
    ci <- rbind(ci, data.frame(Estimate = FOMs$trtMeans[i,1], 
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



OR_RRFC_SP <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
  # ===========================================================================
  #   *****    Analysis 3 (OR Analysis): Random Readers and Fixed Cases     *****
  # ===========================================================================
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  
  trtMeans <-  FOMs$trtMeans
  trtMeanDiffs  <-  FOMs$trtMeanDiffs
  
  # since IndividualTrt["Cov2","VarCom"] and IndividualTrt["Cov3","VarCom"] are zeroes for split-plot-c, FTests will be 
  # identical to FTestsRRRC; other values below may differ
  # not sure about what is going one here; I am proceeding on assumption that
  # the only difference is setting IndividualTrt["Cov2","VarCom"] = IndividualTrt["Cov3","VarCom"] = 0, and reusing code from crossed
  # analysis
  
  # (Results apply to the population of readers but only for the cases used in
  #   this study)
  # 
  # These results result from using the OR model, but treating reader as a random 
  # factor and treatment and case as fixed factors.  Because case is treated as a fixed
  # factor, it follows that Cov1 = Cov2 = Cov3 = 0; i.e., there is no correlation
  # between reader-performance measures (e.g, AUCs) due to reading the same
  # cases.  Thus the OR model reduces to a conventional treatment x reader ANOVA
  # for the reader-performance outcomes, where reader is a random factor and
  # treatment is a fixed factor.  This is the same as a repeated measures ANOVA
  # where treatment is the repeated measures factor, i.e., readers provide an
  # outcome (e.g., AUC) under each treatment.
  # Note that the DBM and OR papers do not discuss this approach, but rather 
  # it is included here for completeness.
  # 
  # a) Test for H0: Treatments have the same AUC
  msDen <- ANOVA$TRanova["TR","MS"]
  f <- ANOVA$TRanova["T","MS"]/msDen
  ddf <- ((I - 1) * (J - 1))
  p <- 1 - pf(f, I - 1, ddf)
  RRFC <- list()
  RRFC$FTests <- data.frame(DF = c(I-1,(I-1)*(J-1)), 
                            MS = c(ANOVA$TRanova["T","MS"],ANOVA$TRanova["TR","MS"]), 
                            F = c(f,NA),  p = c(p,NA), 
                            row.names = c("T","TR"), 
                            stringsAsFactors = FALSE)
  
  #   b) 95% confidence intervals and hypothesis tests (H0: difference = 0)
  #   for treatment AUC differences
  
  stdErr <- sqrt(2 * msDen/J)
  tStat <- vector()
  PrGTt <- vector()
  CI <- array(dim = c(choose(I,2), 2))
  for (i in 1:choose(I,2)) {
    tStat[i] <- trtMeanDiffs[i,1]/stdErr
    PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE)  # correction, noted by user Lucy D'Agostino McGowan
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
  
  #   c) Single treatment AUC 95% confidence intervals
  # (Each analysis is based only on data for the specified treatment, 
  #   i.e. on the treatment-specfic reader ANOVA of AUCs
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
                                     row.names = paste0("Trt", modalityID), 
                                     stringsAsFactors = FALSE)
  
  # StdErr = sqrt[1/r * MS(R)]
  # DF = df[MS(R)] = r-1
  # 95% CI: AUC +- t(.025;df) * StdErr
  # Note: this is the conventional CI, treating the reader AUCs as a random sample.
  
  return(RRFC) 
}



OR_RRRC_SP <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
  # ===========================================================================
  #   *****    Analysis 1 (OR Analysis): Random Readers and Random Cases    *****
  # ===========================================================================
  #     (Results apply to the population of readers and cases)
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  
  trtMeans <-  FOMs$trtMeans
  trtMeanDiffs  <-  FOMs$trtMeanDiffs
  
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
    ci <- sort(c(trtMeanDiffs[i,1] - qt(alpha/2, ddf) * stdErr, trtMeanDiffs[i,1] + qt(alpha/2, ddf) * stdErr))
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



convert2dataset <- function(NL, LL, LL_IL, 
                            perCase, IDs, weights,
                            fileName, type, name, truthTableStr, design,
                            modalityID, readerID) {
  ratings <- list(NL = NL,
                  LL = LL,
                  LL_IL = LL_IL)
  
  lesions <- list(perCase = perCase,
                  IDs = IDs,
                  weights = weights)
  
  descriptions <- list(
    fileName = tools::file_path_sans_ext(basename(fileName)),
    type = type,
    name = name,
    truthTableStr = truthTableStr,
    design = design,
    modalityID = modalityID,
    readerID = readerID)
  
  dataset <- list(ratings = ratings, 
                  lesions = lesions, 
                  descriptions = descriptions)
  
  return(dataset)
  
}



FOM_SP <- function(dataset, FOM) {
  
  # dataType <- dataset$descriptions$type
  # if (dataType == "ROC" && FOM != "Wilcoxon") {
  #   errMsg <- paste0("Must use Wilcoxon figure of merit with ROC data.")
  #   stop(errMsg)
  # }
  # 
  # if (dataType == "ROI" && FOM != "ROI") {
  #   cat("Incorrect FOM supplied for ROI data, changing to 'ROI'\n")
  #   FOM <- "ROI"
  # }
  # 
  # if (!(dataType %in% c("ROC", "LROC")) && FOM == "Wilcoxon")
  #   stop("Cannot use `Wilcoxon` FOM with `FROC` or `ROI` data.")
  # 
  # if (dataType != "ROI" && FOM == "ROI") {
  #   errMsg <- paste0("Only ROI data can be analyzed using ROI figure of merit.")
  #   stop(errMsg)
  # }
  # 
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2  
  
  if (K1 == 0 && !(FOM %in% c("AFROC1", "wAFROC1"))) {
    errMsg <- paste0("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.")
    stop(errMsg)
  }
  
  design <- dataset$descriptions$design
  t <- dataset$descriptions$truthTableStr
  
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  fomArray <- array(dim = c(I, J/I))
  if (design == "SPLIT-PLOT-A") {
    for (i in 1:I) {
      for (j in (J/I*(i-1)+1):(J/I*i)) {
        j1 <- j-J/I*(i-1) 
        if (all(is.na(t[i,j,,1]))) next # if t[] for all normal cases for selected i,j are NAs, skip 
        if (all(is.na(t[i,j,,2]))) next # if t[] for all abnormal cases for selected i,j are NAs, skip 
        k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) # see comments for SPLIT-PLOT-C
        k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K] # ditto:
        nl_ij <- NL[i, j, k1_ij_sub, ]
        perCase_ij <- dataset$lesions$perCase[k2_ij_sub]
        maxLL_ij <- max(perCase_ij)
        ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL_ij]
        k1ij <- sum(!is.na(t[i,j,,1]))
        k2ij <- sum(!is.na(t[i,j,,2]))
        lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        lW_ij <- dataset$lesions$weights[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        dim(nl_ij) <- c(k1ij+k2ij, maxNL)
        dim(ll_ij) <- c(k2ij, maxLL_ij)
        fomArray[i, j1] <- MyFom_ij_SP(nl_ij, ll_ij, perCase_ij, lID_ij, lW_ij, maxNL, maxLL_ij, k1ij, k2ij, FOM)
        next
      }
    }  
  } 
  # else { # design == "SPLIT-PLOT-C") 
  #   if (all(is.na(t[i,j,,1]))) next # if t[] for all normal   cases for selected i,j are NAs, skip 
  #   if (all(is.na(t[i,j,,2]))) next # if t[] for all abnormal cases for selected i,j are NAs, skip 
  #   # k1 refers to normal   case k-indices
  #   # k2 refers to abnormal case k-indices
  #   k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) # k1-indices of all cases meeting the i,j criteria
  #   k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K] # k2-indices of all cases meeting the i,j criteria
  #   nl_ij <- NL[i, j, k1_ij_sub, ] # NL ratings for all cases meeting the i,j criteria
  #   perCase_ij <- dataset$lesions$perCase[k2_ij_sub] # perCase indices for all abnormal cases meeting the i,j criteria
  #   maxLL_ij <- max(perCase_ij)
  #   ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL_ij]
  #   k1ij <- sum(!is.na(t[i,j,,1]))
  #   k2ij <- sum(!is.na(t[i,j,,2]))
  #   lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL_ij, drop = FALSE]
  #   lW_jj <- dataset$lesions$weights[k2_ij_sub,1:maxLL_ij, drop = FALSE]
  #   dim(nl_ij) <- c(k1ij+k2ij, maxNL)
  #   dim(ll_ij) <- c(k2ij, maxLL_ij)
  #   fomArray[i, j] <- MyFom_ij_SP(nl_ij, ll_ij, perCase_ij, lID_ij, lW_jj, maxNL, maxLL_ij, k1ij, k2ij, FOM)
  #   next
  # }
  
  modalityID <- dataset$descriptions$modalityID
  # readers in other treatments can have same names as there is not need to 
  # distinguish between them as, readers with same names in 
  # different treatments are different readers
  readerID <- dataset$descriptions$readerID[1:(J/I)]
  rownames(fomArray) <- paste("trt", sep = "", modalityID)
  colnames(fomArray) <- paste("rdr", sep = "", readerID)
  return(as.data.frame(fomArray))
} 





PseudovaluesSP_A1 <- function(dataset, FOM) {
  
  dataType <- dataset$descriptions$type
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  # account for 15+ FOMs  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    # FOMs defined over NORMAL cases
    jkFomValues <- array(dim = c(I, J/I, K1)) # index reader with j1 defined below
    jkPseudoValues <- array(dim = c(I, J/I, K1)) # index reader with j1 defined below
  }  else if (FOM %in% c("MaxLLF", "HrSe")) { # after checking StOldCode.R, HrSe belongs in this group, depends only on abnormal cases
    # FOMs defined over ABNORMAL cases
    jkFomValues <- array(dim = c(I, J/I, K2))  # index reader with j1 defined below
    jkPseudoValues <- array(dim = c(I, J/I, K2))  # index reader with j1 defined below
  } else if (FOM %in% c("Wilcoxon", "HrAuc", "SongA1", 
                        "AFROC", "AFROC1", "wAFROC1", "wAFROC",
                        "MaxNLFAllCases", "ROI", "SongA2",
                        "PCL", "ALROC")) { # TBA may not handle ROI correctly
    # FOMs defined over ALL cases
    jkFomValues <- array(dim = c(I, J/I, K))  # index reader with j1 defined below
    jkPseudoValues <- array(dim = c(I, J/I, K))  # index reader with j1 defined below
  } else stop("Illegal FOM specified")
  
  t <- dataset$descriptions$truthTableStr
  fomArray <- FOM_SP(dataset, FOM) # I X I matrix with no NAs
  lastCase <- 0
  caseTransitions <- array(dim = J)
  for (i in 1:I) {
    for (j in (J/I*(i-1)+1):(J/I*i)) {
      # for indexing smaller I X I fomArray matrix with no NAs
      # for indexing smaller I X J/I X K jkFomValues matrix with no NAs
      # for indexing smaller I X J/I X K jkPseudoValues matrix with no NAs
      j1 <- j-J/I*(i-1) 
      # NOTATION
      # k1_ij_logi = logical array of NORMAL cases meeting the i,j criteria, length K1 
      k1_ij_logi <- !is.na(t[i,j,,1])
      # k2_ij_logi = logical array of ABNORMAL cases meeting the i,j criteria, length K2 
      k2_ij_logi <- !is.na(t[i,j,,2])[(K1+1):K]
      # k_ij_logi = logical array of ALL cases meeting the i,j criteria, length K 
      k_ij_logi <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) 
      if (sum(k_ij_logi) == 0) next
      
      # perCase_ij = indices for all abnormal cases meeting the i,j criteria
      perCase_ij <- dataset$lesions$perCase[k2_ij_logi] 
      K1_ij <- sum(!is.na(t[i,j,,1]))
      K2_ij <- sum(!is.na(t[i,j,,2]))
      K_ij <- K1_ij + K2_ij
      lID_ij <- dataset$lesions$IDs[k2_ij_logi,1:maxLL, drop = FALSE]
      lW_ij <- dataset$lesions$weights[k2_ij_logi,1:maxLL, drop = FALSE]
      # nl_ij = NL ratings for all cases meeting the i,j criteria
      nl_ij <- NL[i, j, k_ij_logi, 1:maxNL]; dim(nl_ij) <- c(K_ij, maxNL)
      # ll_ij = LL ratings for all cases meeting the i,j criteria
      ll_ij <- LL[i, j, k2_ij_logi, 1:maxLL]; dim(ll_ij) <- c(K2_ij, maxLL)
      
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
        for (k in 1:K1_ij) {
          # NOTATION
          # kIndxNor: case index for the 3rd dimension of normal cases, 
          # ranges from 1 to K1
          kIndxNor <- which(k1_ij_logi)[k];if (is.na(kIndxNor)) 
            stop("Indexing error in Pseudovalues")
          # FOMs defined over NORMAL cases
          nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
          llij_jk <- ll_ij;dim(llij_jk) <- c(K2_ij, maxLL)
          lV_j_jk <- perCase_ij
          lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
          lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
          if (is.na(jkFomValues[i, j1, kIndxNor])) {
            jkFomValues[i, j1, kIndxNor] <- 
              MyFom_ij_SP(nlij_jk, llij_jk, lV_j_jk, 
                          lID_j_jk, lW_j_jk, maxNL, maxLL, 
                          K1_ij - 1, K2_ij, FOM)
          } else stop("overwriting Pseudovalues")
          if (is.na(jkPseudoValues[i, j1, kIndxNor])) {
            jkPseudoValues[i, j1, kIndxNor] <- 
              fomArray[i, j1] * K1_ij - jkFomValues[i, j1, kIndxNor] * (K1_ij - 1)
          } else stop("overwriting Pseudovalues")
        }
      } else if (FOM %in% c("MaxLLF", "HrSe")) { 
        # FOMs defined over ABNORMAL cases
        for (k in 1:K2_ij) {
          # NOTATION
          # kIndxAbn: case index for the 3rd dimension of abormnal cases, 
          # ranges from 1 to K2
          kIndxAbn <- which(k2_ij_logi)[k];if (is.na(kIndxAbn)) 
            stop("Indexing error in Pseudovalues")
          nlij_jk <- nl_ij[-(k+K1_ij), ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
          llij_jk <- ll_ij[-k, ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
          lV_j_jk <- perCase_ij[-k]
          lW_j_jk <- lW_ij[-k, ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
          lID_j_jk <- lID_ij[-k, ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
          if (is.na(jkFomValues[i, j1, kIndxAbn])) {
            jkFomValues[i, j1, kIndxAbn] <- 
              MyFom_ij_SP(nlij_jk, llij_jk, lV_j_jk, 
                          lID_j_jk, lW_j_jk, maxNL, maxLL, 
                          K1_ij, K2_ij - 1, FOM)
          } else stop("overwriting Pseudovalues 3")
          if (is.na(jkPseudoValues[i, j1, kIndxAbn])) {
            jkPseudoValues[i, j1, kIndxAbn] <- 
              fomArray[i, j1] * K2_ij - jkFomValues[i, j1, kIndxAbn] * (K2_ij - 1)
          } else stop("overwriting Pseudovalues")
        }
      } else { 
        # FOMs defined over ALL cases
        for (k in 1:K_ij) {
          # kIndxAll: case index for the 3rd dimension of all cases, range 1:K
          kIndxAll <- which(k_ij_logi)[k]
          if (is.na(kIndxAll)) stop("Indexing error in Pseudovalues() in StSp.R")
          if (k <= K1_ij) {
            nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_ij;dim(llij_jk) <- c(K2_ij, maxLL)
            lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
            lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
            if (is.na(jkFomValues[i, j1, kIndxAll])) {
              jkFomValues[i, j1, kIndxAll] <- 
                MyFom_ij_SP(nlij_jk, llij_jk, perCase_ij, 
                            lID_j_jk, lW_j_jk, maxNL, maxLL, 
                            K1_ij - 1, K2_ij, FOM)
            } else stop("overwriting Pseudovalues")
            if (is.na(jkPseudoValues[i, j1, kIndxAll])) {
              jkPseudoValues[i, j1, kIndxAll] <- 
                fomArray[i, j1] * K_ij - jkFomValues[i, j1, kIndxAll] * (K_ij - 1)
            } else stop("overwriting Pseudovalues")
          } else { # (k > K1_ij)
            nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_ij[-(k - K1_ij), ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
            lV_j_jk <- perCase_ij[-(k - K1_ij)]
            lW_j_jk <- lW_ij[-(k - K1_ij), ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
            lID_j_jk <- lID_ij[-(k - K1_ij), ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
            if (is.na(jkFomValues[i, j1, kIndxAll])) {
              jkFomValues[i, j1, kIndxAll] <- 
                MyFom_ij_SP(nlij_jk, llij_jk, lV_j_jk, 
                            lID_j_jk, lW_j_jk, maxNL, maxLL, 
                            K1_ij, K2_ij - 1, FOM)
            } else stop("overwriting Pseudovalues")
            if (is.na(jkPseudoValues[i, j1, kIndxAll])) {
              jkPseudoValues[i, j1, kIndxAll] <- 
                fomArray[i, j1] * K_ij - jkFomValues[i, j1, kIndxAll] * (K_ij - 1)
            } else stop("overwriting Pseudovalues")
          }
        }
      }
      # center the pseudovalues 
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
        # FOMs defined over NORMAL cases
        jkPseudoValues[i, j1, which(k1_ij_logi)] <- 
          jkPseudoValues[i, j1, which(k1_ij_logi)] + 
          (fomArray[i, j1] - mean(jkPseudoValues[i, j1, which(k1_ij_logi)]))
      }  else if (FOM %in% c("MaxLLF", "HrSe")) {
        # FOMs defined over ABNORMAL cases
        jkPseudoValues[i, j1, which(k2_ij_logi)] <- 
          jkPseudoValues[i, j1, which(k2_ij_logi)] + 
          (fomArray[i, j1] - mean(jkPseudoValues[i, j1, which(k2_ij_logi)]))
      } else {
        # FOMs defined over ALL cases
        jkPseudoValues[i, j1, which(k_ij_logi)] <- 
          jkPseudoValues[i, j1, which(k_ij_logi)] + 
          (fomArray[i, j1] - mean(jkPseudoValues[i, j1, which(k_ij_logi)]))
      }
      caseTransitions[j] <- lastCase
      lastCase <- (lastCase + K_ij) %% K
    }
  }
  
  # there should not be any NAs in each of the following arrays
  # any(is.na(jkPseudoValues))
  # [1] FALSE
  # any(is.na(jkFomValues))
  # [1] FALSE
  # any(is.na(fomArray))
  # [1] FALSE
  
  caseTransitions <- c(caseTransitions, K)
  return(list(
    jkPseudoValues = jkPseudoValues, 
    jkFomValues = jkFomValues,
    caseTransitions = caseTransitions
  ))
}




#' @importFrom stats approx
MyFom_ij_SP <- function(nl, ll, 
                        perCase, lesionID, 
                        lesionWeight, maxNL, 
                        maxLL, K1, K2, FOM) {
  if (!FOM %in% c("Wilcoxon", "HrAuc", "HrSe", "HrSp", 
                  "AFROC1", "AFROC", 
                  "wAFROC1", "wAFROC", 
                  "MaxLLF", "MaxNLF", "MaxNLFAllCases")){
    errMsg <- paste0(FOM, " is not an available figure of merit.")
    stop(errMsg)
  }
  
  fom <- NA
  fom <- switch(FOM,
                "Wilcoxon" = TrapezoidalArea(nl, K1, ll, K2),
                "HrAuc" = HrAuc(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "HrSe" = HrSe(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "HrSp" = HrSp(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "AFROC1" = AFROC1(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "AFROC" = AFROC(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "wAFROC1" = wAFROC1(nl, ll, perCase, c(K1, K2), maxNL, maxLL, lesionWeight), 
                "wAFROC" = wAFROC(nl, ll, perCase, c(K1, K2), maxNL, maxLL, lesionWeight),
                "MaxLLF" = MaxLLF(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "MaxNLF" = MaxNLF(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "MaxNLFAllCases" = MaxNLFAllCases(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
  )
  return(fom)
} 


