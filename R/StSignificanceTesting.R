#' Perform significance testing, DBMH or ORH
#' 
#' @description  Performs Dorfman-Berbaum-Metz (DBM) or Obuchowski-Rockette (OR) 
#'    significance testing with Hillis' improvements, for specified dataset; 
#'    significance testing refers to analysis designed to assign a P-value for 
#'    rejecting a null hypothesis (NH); the most common NH is that the reader-averaged 
#'    figure of merit (FOM) difference between treatments is zero. The results of 
#'    the analysis are better visualized in the text or, preferably, 
#'    Excel-formatted, files produced by \link{UtilOutputReport}. 
#'
#'  
#' @param dataset The dataset to be analyzed, see \link{RJafroc-package}
#' @param FOM The figure of merit, default \code{"wJAFROC"}, 
#'    see \link{UtilFigureOfMerit}
#' @param alpha The significance level of the test of the null hypothesis that all 
#'    treatment effects are zero; the default alpha is 0.05
#' @param method The significance testing method to be used. There are two options: 
#'    \code{"DBMH"} (the default) or \code{"ORH"}, representing the Dorfman-Berbaum-Metz
#'    and the Obuchowski-Rockette significance testing methods, respectively. 
#' @param covEstMethod The method used to estimate the covariance matrix 
#'    in ORH analysis; it can be \code{"Jackknife"}, \code{"Bootstrap"} 
#'    or \code{"DeLong"}, the last assumes \code{FOM = "Wilcoxon"}, otherwise 
#'    an error results. This parameter is not relevant if the analysis method 
#'    is \code{"DBMH"}
#' @param nBoots The number of bootstraps (default is 200), relevant only if 
#'    the \code{"Bootstrap"} method is used to estimate the covariance matrix
#'    in the ORH method 
#' @param option Determines which factors are regarded as random vs. fixed: 
#'    \code{"RRRC"} = random-reader random case, 
#'    \code{"FRRC"} = fixed-reader random case, 
#'    \code{"RRFC"} = random-reader fixed case, 
#'    \code{"ALL"} outputs the results of \code{"RRRC"}, \code{"FRRC"} 
#'    and \code{"RRFC"} analyses
#' @param VarCompFlag If TRUE, only the appropriate (DBM or OR) variance components 
#'    (six in all) are returned, default is FALSE
#' @param FPFValue Only needed for LROC data; where to evaluate a partial curve based
#'    figure of merit. The default is 0.2.
#' 
#' @return For \code{method = "DBMH"}  the returned value is a list with 22 members:
#' @return \item{fomArray}{The figure of merit array for each treatment-reader 
#'    combination}
#' @return \item{anovaY}{The ANOVA table of the pseudovalues over all treatments}
#' @return \item{anovaYi}{The ANOVA table of the pseudovalues for each treatment}
#' @return \item{varComp}{The variance components of the pseudovalue model underlying 
#'    the analysis, 6 values, in the following order: c("Var(R)", "Var(C)", 
#'    "Var(T*R)", "Var(T*C)", "Var(R*C)", "Var(Error)")}
#' @return \item{fRRRC}{For \strong{random-reader random-case} (RRRC) analysis, the 
#'    F-statistic for rejecting the null hypothesis of no treatment effect}
#' @return \item{ddfRRRC}{For RRRC analysis, the denominator degrees of freedom 
#'    of the F statistic}
#' @return \item{pRRRC}{For RRRC analysis, the p-value of the significance 
#'    test of the NH}
#' @return \item{ciDiffTrtRRRC}{For RRRC analysis, the confidence intervals and related 
#'    test statistics for the FOM differences between pairs of treatments}
#' @return \item{ciAvgRdrEachTrtRRRC}{For RRRC analysis, the confidence intervals 
#'    and related test statistics for rdr. avg. FOM in each treatment}
#' @return \item{fFRRC}{For \strong{fixed-reader random-case} (FRRC) analysis, the 
#'    F-statistic for rejecting the NH}
#' @return \item{ddfFRRC}{For FRRC analysis, the denominator degrees of freedom 
#'    of the F-statistic}
#' @return \item{pFRRC}{For FRRC analysis, the p-value of the significance 
#'    test of the NH}
#' @return \item{ciDiffTrtFRRC}{For FRRC analysis, the confidence intervals and related 
#'    test statistics for the FOM differences between pairs of treatments}
#' @return \item{ciAvgRdrEachTrtFRRC}{For FRRC analysis, the confidence intervals and 
#'    related tests for rdr. avg. FOM in each treatment}
#' @return \item{ssAnovaEachRdr}{The sum of squares table of the ANOVA of the 
#'    pseudovalues for each reader (based on data for the specified reader)}
#' @return \item{msAnovaEachRdr}{The mean squares table of the ANOVA of the 
#'    pseudovalues for each reader (based on data for the specified reader)}
#' @return \item{ciDiffTrtEachRdr}{The confidence intervals and related tests of the 
#'    FOM differences between pairs of treatments for each reader}
#' @return \item{fRRFC}{For \strong{random-reader fixed-case} (RRFC) analysis, 
#'    the F statistic}
#' @return \item{ddfRRFC}{For RRFC analysis, the denominator degrees of freedom 
#'    of the F statistic}
#' @return \item{pRRFC}{For RRFC analysis, the p-value for rejecting the NH}
#' @return \item{ciDiffTrtRRFC}{For RRFC analysis, the confidence intervals and 
#'    related test statistics for the FOM differences between pairs of treatments}
#' @return \item{ciAvgRdrEachTrtRRFC}{For RRFC analysis, the confidence intervals 
#'    and related tests for reader averaged FOM in each treatment}
#' 
#' 
#' @return For method = "ORH" the return value is a list with with 21 members:
#' @return \item{fomArray}{Figures of merit array. See the return of 
#'    \link{UtilFigureOfMerit}}
#' @return \item{msT}{Mean square of the figure of merit corresponding to 
#'    the treatment effect}
#' @return \item{msTR}{Mean square of the figure of merit corresponding to 
#'    the treatment-reader effect}
#' @return \item{varComp}{The variance components of the pseudovalue model underlying 
#'    the analysis, 6 values, in
#' the following order: c("Var(R)", "Var(T*R)", "COV1", "COV2", "COV3", "Var(Error)")}
#' @return \item{fRRRC}{Same as \code{DBMH} method}
#' @return \item{ddfRRRC}{Same as \code{DBMH} method}
#' @return \item{pRRRC}{Same as \code{DBMH} method}
#' @return \item{ciDiffTrtRRRC}{Same as \code{DBMH} method}
#' @return \item{ciAvgRdrEachTrtRRRC}{Same as \code{DBMH} method}
#' @return \item{fFRRC}{Same as \code{DBMH} method}
#' @return \item{ddfFRRC}{Same as \code{DBMH} method}
#' @return \item{pFRRC}{Same as \code{DBMH} method}
#' @return \item{ciDiffTrtFRRC}{Same as \code{DBMH} method}
#' @return \item{ciAvgRdrEachTrtFRRC}{Same as \code{DBMH} method}
#' @return \item{ciDiffTrtEachRdr}{Same as \code{DBMH} method}
#' @return \item{varCovEachRdr}{Obuchowski-Rockette Variance and Cov1 
#'    estimates for each reader}
#' @return \item{fRRFC}{Same as \code{DBMH} method}
#' @return \item{ddfRRFC}{Same as \code{DBMH} method}
#' @return \item{pRRFC}{Same as \code{DBMH} method}
#' @return \item{ciDiffTrtRRFC}{Same as \code{DBMH} method}
#' @return \item{ciAvgRdrEachTrtRRFC}{Same as \code{DBMH} method}
#' 
#' 
#' @examples
#' retDbmRoc  <- StSignificanceTesting(dataset02, 
#' FOM = "Wilcoxon", method = "DBMH") 
#' 
#' \dontrun{
#' retDbmwJAFROC  <- StSignificanceTesting(dataset05) # default is weighted JAFROC
#' 
#' retDbmHrAuc  <- StSignificanceTesting(dataset05, 
#' FOM = "HrAuc", method = "DBMH") 
#' print(retDbmHrAuc) 
#' 
#' retDbmSongA1  <- StSignificanceTesting(dataset05, 
#' FOM = "SongA1", method = "DBMH") 
#' print(retDbmSongA1)
#' 
#' retDbmSongA2  <- StSignificanceTesting(dataset05, 
#' FOM = "SongA2", method = "DBMH") 
#' print(retDbmSongA2)
#' 
#' retDbmwJafroc1  <- StSignificanceTesting(dataset05, 
#' FOM = "wJAFROC1", method = "DBMH")
#' print(retDbmwJafroc1)
#'  
#' retDbmJafroc1  <- StSignificanceTesting(dataset05, 
#' FOM = "JAFROC1", method = "DBMH")
#' print(retDbmJafroc1)
#'  
#' retDbmJAFROC  <- StSignificanceTesting(dataset05, 
#' FOM = "JAFROC", method = "DBMH") 
#' print(retDbmJAFROC)
#'  
#' }
#' 
#' retOR <- StSignificanceTesting(dataset02, 
#' FOM = "Wilcoxon", method = "ORH")
#' print(retOR)
#'
#' 
#' @references
#' Dorfman DD, Berbaum KS, Metz CE (1992) ROC characteristic rating analysis: 
#' Generalization to the Population of Readers and Patients with the Jackknife method, Invest. Radiol. 27, 723-731.
#' 
#' Obuchowski NA, Rockette HE (1995) Hypothesis Testing of the Diagnostic Accuracy for Multiple Diagnostic Tests:  
#' An ANOVA Approach with Dependent Observations, Communications in Statistics: Simulation and Computation 24, 285-308.
#' 
#' Hillis SL (2014) A marginal-mean ANOVA approach for analyzing multireader multicase radiological imaging data, 
#' Statistics in medicine 33, 330-360.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#'
#'
#' @importFrom stats pf pt qt
#' @importFrom Rcpp evalCpp
#' @useDynLib RJafroc
#'
#'      
#' @export
StSignificanceTesting <- function(dataset, FOM = "wJAFROC", alpha = 0.05, method = "DBMH", 
                                  covEstMethod = "Jackknife", nBoots = 200, option = "ALL", 
                                  VarCompFlag = FALSE, FPFValue = 0.2)
{
  if (dataset$dataType == "ROI") {
    method <- "ORH"
    covEstMethod <- "DeLong" 
    FOM <- "ROI"
    cat("ROI dataset: using method = `ORH``, covEstMethod = `DeLong`` and FOM = `ROI`.\n")
  }
  if (method == "DBMH"){
    return(StDBMHAnalysis(dataset, FOM, alpha, option, FPFValue = FPFValue))
  } else if (method == "ORH"){
    return(StORHAnalysis(dataset, FOM, alpha, covEstMethod, nBoots, option, FPFValue = FPFValue))
  } else {
    errMsg <- sprintf("%s is not a valid analysis method.", method)
    stop(errMsg)
  }
}




StDBMHAnalysis <- function(dataset, FOM = "wJAFROC", alpha = 0.05, option = "ALL", 
                           VarCompFlag = FALSE, FPFValue = 0.2) {
  NL <- dataset$NL
  LL <- dataset$LL
  lesionNum <- dataset$lesionNum
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  if (!option %in% c("RRRC", "FRRC", "RRFC", "ALL")){
    errMsg <- sprintf("%s is not a valid option.", option)
    stop(errMsg)
  }    
  
  if (I < 2) {
    stop("The analysis requires at least 2 treatments; use StSignificanceTestingSingleFixedFactor()")
  }
  
  fomArray <- UtilFigureOfMerit(dataset, FOM)
  trMeans <- rowMeans(fomArray)
  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    jkFOMArray <- array(dim = c(I, J, K1))
    pseudoValues <- array(dim = c(I, J, K1))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K1) {
          nl <- NL[i, j, -k, ]
          ll <- LL[i, j, , ]
          dim(nl) <- c(K - 1, maxNL)
          dim(ll) <- c(K2, max(lesionNum))
          jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM)
          pseudoValues[i, j, k] <- fomArray[i, j] * K1 - jkFOMArray[i, j, k] * (K1 - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    jkFOMArray <- array(dim = c(I, J, K2))
    pseudoValues <- array(dim = c(I, J, K2))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K2) {
          nl <- NL[i, j, -(k + K1), ]
          ll <- LL[i, j, -k, ]
          dim(nl) <- c(K - 1, maxNL)
          dim(ll) <- c(K2 - 1, max(lesionNum))
          lesionIDJk <- lesionID[-k, ]
          dim(lesionIDJk) <- c(K2 -1, max(lesionNum))
          lesionWeightJk <- lesionWeight[-k, ]
          dim(lesionWeightJk) <- c(K2 -1, max(lesionNum))
          jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum[-k], lesionIDJk, lesionWeightJk, maxNL, maxLL, K1, K2 - 1, FOM)
          pseudoValues[i, j, k] <- fomArray[i, j] * K2 - jkFOMArray[i, j, k] * (K2 - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  } else {
    jkFOMArray <- array(dim = c(I, J, K))
    pseudoValues <- array(dim = c(I, J, K))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K) {
          if (k <= K1) {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(lesionNum))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM)
          } else {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, -(k - K1), ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(lesionNum))
            lesionIDJk <- lesionID[-(k - K1), ]
            dim(lesionIDJk) <- c(K2 -1, max(lesionNum))
            lesionWeightJk <- lesionWeight[-(k - K1), ]
            dim(lesionWeightJk) <- c(K2 -1, max(lesionNum))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum[-(k - K1)], lesionIDJk, lesionWeightJk, maxNL, maxLL, K1, K2 - 1, FOM)
          }
          pseudoValues[i, j, k] <- fomArray[i, j] * K - jkFOMArray[i, j, k] * (K - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  }
  
  K <- length(pseudoValues[1, 1, ])
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(pseudoValues[i, , ]) - mean(pseudoValues))^2
  }
  msT <- msT * K * J/(I - 1)
  
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(pseudoValues[, j, ]) - mean(pseudoValues))^2
  }
  msR <- msR * K * I/(J - 1)
  
  
  msC <- 0
  for (k in 1:K) {
    msC <- msC + (mean(pseudoValues[, , k]) - mean(pseudoValues))^2
  }
  msC <- msC * I * J/(K - 1)
  
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, j, ]) + mean(pseudoValues))^2
    }
  }
  msTR <- msTR * K/((I - 1) * (J - 1))
  
  
  msTC <- 0
  for (i in 1:I) {
    for (k in 1:K) {
      msTC <- msTC + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
    }
  }
  msTC <- msTC * J/((I - 1) * (K - 1))
  
  
  msRC <- 0
  for (j in 1:J) {
    for (k in 1:K) {
      msRC <- msRC + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
    }
  }
  msRC <- msRC * I/((J - 1) * (K - 1))
  
  msTRC <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K) {
        msTRC <- msTRC + (pseudoValues[i, j, k] - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) - mean(pseudoValues[, j, k]) + 
                            mean(pseudoValues[i, , ]) + mean(pseudoValues[, j, ]) + mean(pseudoValues[, , k]) - mean(pseudoValues))^2
      }
    }
  }
  msTRC <- msTRC/((I - 1) * (J - 1) * (K - 1))
  
  varR <- (msR - msTR - msRC + msTRC)/(I * K)
  varC <- (msC - msTC - msRC + msTRC)/(I * J)
  varTR <- (msTR - msTRC)/K
  varTC <- (msTC - msTRC)/J
  varRC <- (msRC - msTRC)/I
  varErr <- msTRC
  varComp <- c(varR, varC, varTR, varTC, varRC, varErr)
  varCompName <- c("Var(R)", "Var(C)", "Var(T*R)", "Var(T*C)", "Var(R*C)", "Var(Error)")
  varComp <- data.frame(varComp, row.names = varCompName)
  if (VarCompFlag){
    return (varComp)
  }
  
  msArray <- c(msT, msR, msC, msTR, msTC, msRC, msTRC)
  dfArray <- c(I - 1, J - 1, K - 1, (I - 1) * (J - 1), (I - 1) * (K - 1), (J - 1) * (K - 1), (I - 1) * (J - 1) * (K - 1))
  ssArray <- msArray * dfArray
  msArray <- c(msArray, NA)
  dfArray <- c(dfArray, sum(dfArray))
  ssArray <- c(ssArray, sum(ssArray))
  sourceArray <- c("T", "R", "C", "TR", "TC", "RC", "TRC", "Total")
  anovaY <- data.frame(Source = sourceArray, SS = ssArray, DF = dfArray, MS = msArray)
  
  msRSingle <- array(0, dim = c(I))
  msCSingle <- array(0, dim = c(I))
  msRCSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    for (j in 1:J) {
      msRSingle[i] <- msRSingle[i] + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]))^2
    }
    msRSingle[i] <- msRSingle[i] * K/(J - 1)
    
    
    for (k in 1:K) {
      msCSingle[i] <- msCSingle[i] + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]))^2
    }
    msCSingle[i] <- msCSingle[i] * J/(K - 1)
    
    for (j in 1:J) {
      for (k in 1:K) {
        msRCSingle[i] <- msRCSingle[i] + (mean(pseudoValues[i, j, k]) - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) + mean(pseudoValues[i, , ]))^2
      }
    }
    msRCSingle[i] <- msRCSingle[i]/((J - 1) * (K - 1))
  }
  sourceArraySingle <- c("R", "C", "RC")
  dfArraySingle <- c(J - 1, K - 1, (J - 1) * (K - 1))
  msArraySingle <- t(cbind(msRSingle, msCSingle, msRCSingle))
  msSingleTable <- data.frame(sourceArraySingle, dfArraySingle, msArraySingle, row.names = NULL)
  colnames(msSingleTable) <- c("Source", "DF", modalityID)
  
  diffTRMeans <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRMeans[ii] <- trMeans[i] - trMeans[ip]
      diffTRName[ii] <- paste(modalityID[i], modalityID[ip], sep = " - ")
      ii <- ii + 1
    }
  }
  
  msNum <- msT
  
  if (option %in% c("RRRC", "ALL")) {
    # ************ RRRC ****************
    if (J > 1) {
      msDenRRRC <- msTR + max(msTC - msTRC, 0)
      fRRRC <- msNum/msDenRRRC
      ddfRRRC <- msDenRRRC^2/(msTR^2/((I - 1) * (J - 1)))
      pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
      stdErrRRRC <- sqrt(2 * msDenRRRC/J/K)
      tStat <- vector()
      tPr <- vector()
      CIRRRC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRRC
        tPr[i] <- 2 * pt(abs(tStat[i]), ddfRRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
        CIRRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRRC) * stdErrRRRC, diffTRMeans[i] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
        
      }
      ciDiffTrtRRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRRC, choose(I, 2)), DF = rep(ddfRRRC, choose(I, 2)), t = tStat, p = tPr, CI = CIRRRC)
      colnames(ciDiffTrtRRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
      
      dfSingleRRRC <- array(dim = I)
      msDenSingleRRRC <- array(dim = I)
      stdErrSingleRRRC <- array(dim = I)
      CISingleRRRC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRRC[i] <- msRSingle[i] + max(msCSingle[i] - msRCSingle[i], 0)
        dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
        stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J/K)
        ciTemp <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trMeans[i] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
        if (length(ciTemp) == 2) CISingleRRRC[i, ] <- ciTemp
        
      }
      ciAvgRdrEachTrtRRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRRC, DF = dfSingleRRRC, CI = CISingleRRRC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRRC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    } else {
      fRRRC <- NA
      ddfRRRC <- NA
      pRRRC <- NA
      ciDiffTrtRRRC <- NA
      ciAvgRdrEachTrtRRRC <- NA
    }
    if (option == "RRRC")
      return(list(fomArray = fomArray, anovaY = anovaY, anovaYi = msSingleTable, varComp = varComp, 
                  fRRRC = fRRRC, ddfRRRC = ddfRRRC, pRRRC = pRRRC, ciDiffTrtRRRC = ciDiffTrtRRRC, ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC))
  }
  
  if (option %in% c("FRRC", "ALL")) {
    # ************ FRRC ****************
    msDenFRRC <- msTC
    fFRRC <- msNum/msDenFRRC
    ddfFRRC <- (I - 1) * (K - 1)
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J/K)
    tStat <- vector()
    tPr <- vector()
    CIFRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrFRRC
      tPr[i] <- 2 * pt(abs(tStat[i]), ddfFRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIFRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfFRRC) * stdErrFRRC, diffTRMeans[i] + qt(alpha/2, ddfFRRC) * stdErrFRRC))
    }
    ciDiffTrtFRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrFRRC, choose(I, 2)), DF = rep(ddfFRRC, choose(I, 2)), t = tStat, p = tPr, CI = CIFRRC)
    colnames(ciDiffTrtFRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
    
    dfSingleFRRC <- array(dim = I)
    msDenSingleFRRC <- array(dim = I)
    stdErrSingleFRRC <- array(dim = I)
    CISingleFRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleFRRC[i] <- msCSingle[i]
      dfSingleFRRC[i] <- (K - 1)
      stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J/K)
      CISingleFRRC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i], trMeans[i] + qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i]))
    }
    ciAvgRdrEachTrtFRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleFRRC, DF = dfSingleFRRC, CI = CISingleFRRC, row.names = NULL)
    colnames(ciAvgRdrEachTrtFRRC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    
    ssTFRRC <- array(0, dim = c(J))
    ssCFRRC <- array(0, dim = c(J))
    ssTCFRRC <- array(0, dim = c(J))
    for (j in 1:J) {
      for (i in 1:I) {
        ssTFRRC[j] <- ssTFRRC[j] + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[, j, ]))^2
      }
      ssTFRRC[j] <- ssTFRRC[j] * K
      
      for (k in 1:K) {
        ssCFRRC[j] <- ssCFRRC[j] + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]))^2
      }
      ssCFRRC[j] <- ssCFRRC[j] * I
      
      for (i in 1:I) {
        for (k in 1:K) {
          ssTCFRRC[j] <- ssTCFRRC[j] + (mean(pseudoValues[i, j, k]) - mean(pseudoValues[i, j, ]) - mean(pseudoValues[, j, k]) + mean(pseudoValues[, j, ]))^2
        }
      }
    }
    sourceArrayFRRC <- c("T", "C", "TC")
    dfArrayFRRC <- c(I - 1, K - 1, (I - 1) * (K - 1))
    ssArrayFRRC <- t(cbind(ssTFRRC, ssCFRRC, ssTCFRRC))
    ssTableFRRC <- data.frame(sourceArrayFRRC, dfArrayFRRC, ssArrayFRRC, row.names = NULL)
    colnames(ssTableFRRC) <- c("Source", "DF", readerID)
    
    msArrayFRRC <- ssArrayFRRC
    for (n in 1:3) msArrayFRRC[n, ] <- ssArrayFRRC[n, ]/dfArrayFRRC[n]
    msTableFRRC <- data.frame(sourceArrayFRRC, dfArrayFRRC, msArrayFRRC, row.names = NULL)
    colnames(msTableFRRC) <- c("Source", "DF", readerID)
    
    diffTRMeansFRRC <- array(dim = c(J, choose(I, 2)))
    for (j in 1:J) {
      ii <- 1
      for (i in 1:I) {
        if (i == I) 
          break
        for (ip in (i + 1):I) {
          diffTRMeansFRRC[j, ii] <- fomArray[i, j] - fomArray[ip, j]
          ii <- ii + 1
        }
      }
    }
    diffTRMeansFRRC <- as.vector(t(diffTRMeansFRRC))
    stdErrFRRC <- sqrt(2 * msArrayFRRC[3, ]/K)
    stdErrFRRC <- rep(stdErrFRRC, choose(I, 2))
    dim(stdErrFRRC) <- c(J, choose(I, 2))
    stdErrFRRC <- as.vector(t(stdErrFRRC))
    readerNames <- rep(readerID, choose(I, 2))
    dim(readerNames) <- c(J, choose(I, 2))
    readerNames <- as.vector(t(readerNames))
    trNames <- rep(diffTRName, J)
    dfReaderFRRC <- rep(K - 1, length(stdErrFRRC))
    CIReaderFRRC <- array(dim = c(length(stdErrFRRC), 2))
    tStat <- vector()
    tPr <- vector()
    for (n in 1:length(stdErrFRRC)) {
      tStat[n] <- diffTRMeansFRRC[n]/stdErrFRRC[n]
      tPr[n] <- 2 * pt(abs(tStat[n]), dfReaderFRRC[n], lower.tail = FALSE)
      CIReaderFRRC[n, ] <- sort(c(diffTRMeansFRRC[n] - qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n], diffTRMeansFRRC[n] + qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n]))
    }
    ciDiffTrtEachRdr <- data.frame(Reader = readerNames, Treatment = trNames, Estimate = diffTRMeansFRRC, StdErr = stdErrFRRC, DF = dfReaderFRRC, t = tStat, p = tPr, CI = CIReaderFRRC)
    colnames(ciDiffTrtEachRdr) <- c("Reader", "Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
    if (option == "FRRC")
      return(list(fomArray = fomArray, anovaY = anovaY, anovaYi = msSingleTable, varComp = varComp, 
                  fFRRC = fFRRC, ddfFRRC = ddfFRRC, pFRRC = pFRRC, ciDiffTrtFRRC = ciDiffTrtFRRC, ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
                  ssAnovaEachRdr = ssTableFRRC, msAnovaEachRdr = msTableFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr))
  }
  
  if (option %in% c("RRFC", "ALL")) {
    # ************ RRFC ****************
    if (J > 1) {
      msDenRRFC <- msTR
      fRRFC <- msNum/msDenRRFC
      ddfRRFC <- ((I - 1) * (J - 1))
      pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
      stdErrRRFC <- sqrt(2 * msDenRRFC/J/K)
      tStat <- vector()
      tPr <- vector()
      CIRRFC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRFC
        tPr[i] <- 2 * pt(abs(tStat[i]), ddfRRFC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
        CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
      }
      ciDiffTrtRRFC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRFC, choose(I, 2)), DF = rep(ddfRRFC, choose(I, 2)), t = tStat, p = tPr, CI = CIRRFC)
      colnames(ciDiffTrtRRFC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
      
      dfSingleRRFC <- array(dim = I)
      msDenSingleRRFC <- array(dim = I)
      stdErrSingleRRFC <- array(dim = I)
      CISingleRRFC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRFC[i] <- msRSingle[i]
        dfSingleRRFC[i] <- (J - 1)
        stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J/K)
        CISingleRRFC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
      }
      ciAvgRdrEachTrtRRFC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRFC, DF = dfSingleRRFC, CI = CISingleRRFC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRFC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    } else {
      fRRFC <- NA
      ddfRRFC <- NA
      pRRFC <- NA
      ciDiffTrtRRFC <- NA
      ciAvgRdrEachTrtRRFC <- NA
    }
    if (option == "RRFC")
      return(list(fomArray = fomArray, anovaY = anovaY, anovaYi = msSingleTable, varComp = varComp, 
                  fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
  }
  
  return(list(
    fomArray = fomArray, 
    anovaY = anovaY, 
    anovaYi = msSingleTable, 
    varComp = varComp, 
    fRRRC = fRRRC, 
    ddfRRRC = ddfRRRC, 
    pRRRC = pRRRC, 
    ciDiffTrtRRRC = ciDiffTrtRRRC, 
    ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC, 
    fFRRC = fFRRC, 
    ndf = I - 1, 
    ddfFRRC = ddfFRRC, 
    pFRRC = pFRRC, 
    ciDiffTrtFRRC = ciDiffTrtFRRC, 
    ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
    ssAnovaEachRdr = ssTableFRRC, msAnovaEachRdr = msTableFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr, 
    fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
} 




StORHAnalysis <- function(dataset, FOM = "wJAFROC", alpha = 0.05, covEstMethod = "Jackknife", 
                          nBoots = 200, option = "ALL", VarCompFlag = FALSE, FPFValue = 0.2)  {
  dataType <- dataset$dataType
  if (dataType != "LROC") {
    NL <- dataset$NL
    LL <- dataset$LL
  } else {
    if (FOM == "Wilcoxon"){
      dataset <- DfLroc2Roc(dataset)
      NL <- dataset$NL
      LL <- dataset$LL
    } else if (FOM %in% c("PCL", "ALROC")){
      NL <- dataset$NL
      LL <- dataset$LLCl
    } else stop("incorrect FOM for LROC data")
  }
  lesionNum <- dataset$lesionNum
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  dim(NL) <- c(I, J, K, maxNL)
  dim(LL) <- c(I, J, K2, max(lesionNum))
  
  if (!option %in% c("RRRC", "FRRC", "RRFC", "ALL")){
    errMsg <- sprintf("%s is not an available option.", option)
    stop(errMsg)
  }    
  
  if (!covEstMethod %in% c("Jackknife", "Bootstrap", "DeLong")) {
    errMsg <- paste0(covEstMethod, " is not an allowed covariance estimation method.")
    stop(errMsg)
  }
  
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue = FPFValue)
  trMeans <- rowMeans(fomArray)
  fomMean <- mean(fomArray)
  
  ret <- gpfEstimateVarCov(fomArray, NL, LL, lesionNum, lesionID, 
                           lesionWeight, maxNL, maxLL, FOM, covEstMethod, nBoots, FPFValue = FPFValue)
  var <- ret$var
  cov1 <- ret$cov1
  cov2 <- ret$cov2
  cov3 <- ret$cov3
  
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(fomArray[i, ]) - fomMean)^2
  }
  msT <- J * msT/(I - 1)
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(fomArray[, j]) - fomMean)^2
  }
  msR <- I * msR/(J - 1)
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (fomArray[i, j] - mean(fomArray[i, ]) - mean(fomArray[, j]) + fomMean)^2
    }
  }
  msTR <- msTR/((J - 1) * (I - 1))
  
  varTR <- msTR - var + cov1 + max(cov2 - cov3, 0)
  varR <- (msR - var - (I - 1) * cov1 + cov2 + (I - 1) * cov3 - varTR)/I
  varCovArray <- c(varR, varTR, cov1, cov2, cov3, var)
  nameArray <- c("Var(R)", "Var(T*R)", "COV1", "COV2", "COV3", "Var(Error)")
  varComp <- data.frame(varCov = varCovArray, row.names = nameArray)
  if (VarCompFlag){
    return (varComp)
  }
  
  varSingle <- vector(length = I)
  cov2Single <- vector(length = I)
  for (i in 1:I) {
    fomSingle <- fomArray[i, ]
    nl <- NL[i, , , ]
    ll <- LL[i, , , ]
    dim(fomSingle) <- c(1, J)
    dim(nl) <- c(1, J, K, maxNL)
    dim(ll) <- c(1, J, K2, max(lesionNum))
    ret <- gpfEstimateVarCov(fomSingle, nl, ll, lesionNum, lesionID, 
                             lesionWeight, maxNL, maxLL, FOM, covEstMethod, nBoots, FPFValue = FPFValue)
    varSingle[i] <- ret$var
    if (J > 1) {
      cov2Single[i] <- ret$cov2
    } else {
      cov2Single[i] <- 0
    }
  }
  
  varEchRder <- vector(length = J)
  cov1EchRder <- vector(length = J)
  for (j in 1:J) {
    fomSingle <- fomArray[, j]
    nl <- NL[, j, , ]
    ll <- LL[, j, , ]
    dim(fomSingle) <- c(I, 1)
    dim(nl) <- c(I, 1, K, maxNL)
    dim(ll) <- c(I, 1, K2, max(lesionNum))
    ret <- gpfEstimateVarCov(fomSingle, nl, ll, lesionNum, lesionID, 
                             lesionWeight, maxNL, maxLL, FOM, covEstMethod, nBoots, FPFValue = FPFValue)
    varEchRder[j] <- ret$var
    cov1EchRder[j] <- ret$cov1
  }
  
  msRSingle <- array(0, dim = c(I))
  for (i in 1:I) {
    msRSingle[i] <- sum((fomArray[i, ] - trMeans[i])^2)/(J - 1)
  }
  
  diffTRMeans <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      diffTRMeans[ii] <- trMeans[i] - trMeans[ip]
      diffTRName[ii] <- paste(modalityID[i], modalityID[ip], sep = " - ")
      ii <- ii + 1
    }
  }
  
  msNum <- msT
  
  # ************ RRRC ****************
  if (option %in% c("RRRC", "ALL")) {
    if (J > 1) {
      msDenRRRC <- msTR + max(J * (cov2 - cov3), 0)
      fRRRC <- msNum/msDenRRRC
      ddfRRRC <- msDenRRRC^2/(msTR^2/((I - 1) * (J - 1)))
      pRRRC <- 1 - pf(fRRRC, I - 1, ddfRRRC)
      stdErrRRRC <- sqrt(2 * msDenRRRC/J)
      tStat <- vector()
      tPr <- vector()
      CIRRRC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRRC
        tPr[i] <- 2 * pt(abs(tStat[i]), ddfRRRC, lower.tail = FALSE) # critical correction, noted by user Lucy D'Agostino McGowan
        ci <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRRC) * stdErrRRRC, diffTRMeans[i] + qt(alpha/2, ddfRRRC) * stdErrRRRC))
        if (length(ci) == 0){
          CIRRRC[i, ] <- c(NA, NA)
        }else{
          CIRRRC[i, ] <- ci
        }
      }
      ciDiffTrtRRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRRC, choose(I, 2)), DF = rep(ddfRRRC, choose(I, 2)), t = tStat, p = tPr, CI = CIRRRC)
      colnames(ciDiffTrtRRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
      
      dfSingleRRRC <- array(dim = I)
      msDenSingleRRRC <- array(dim = I)
      stdErrSingleRRRC <- array(dim = I)
      CISingleRRRC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRRC[i] <- msRSingle[i] + max(J * cov2Single[i], 0)
        dfSingleRRRC[i] <- msDenSingleRRRC[i]^2/msRSingle[i]^2 * (J - 1)
        stdErrSingleRRRC[i] <- sqrt(msDenSingleRRRC[i]/J)
        ci <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i], trMeans[i] + qt(alpha/2, dfSingleRRRC[i]) * stdErrSingleRRRC[i]))
        if (length(ci) == 0){
          CISingleRRRC[i, ] <- c(NA, NA)
        }else{
          CISingleRRRC[i, ] <- ci
        }
        
      }
      ciAvgRdrEachTrtRRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRRC, DF = dfSingleRRRC, CI = CISingleRRRC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRRC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    } else {
      fRRRC <- NA
      ddfRRRC <- NA
      pRRRC <- NA
      ciDiffTrtRRRC <- NA
      ciAvgRdrEachTrtRRRC <- NA
    }
    if (option == "RRRC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fRRRC = fRRRC, ddfRRRC = ddfRRRC, pRRRC = pRRRC, ciDiffTrtRRRC = ciDiffTrtRRRC, ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC))
    }
  }
  
  # ************ FRRC ****************
  if (option %in% c("FRRC", "ALL")) {
    if (J > 1) {
      msDenFRRC <- var - cov1 + (J - 1) * (cov2 - cov3)
    } else {
      msDenFRRC <- var - cov1
    }
    fFRRC <- msNum/msDenFRRC
    ddfFRRC <- Inf
    pFRRC <- 1 - pf(fFRRC, I - 1, ddfFRRC)
    stdErrFRRC <- sqrt(2 * msDenFRRC/J)
    tStat <- vector()
    tPr <- vector()
    CIFRRC <- array(dim = c(length(diffTRMeans), 2))
    for (i in 1:length(diffTRMeans)) {
      tStat[i] <- diffTRMeans[i]/stdErrFRRC
      tPr[i] <- 2 * pt(abs(tStat[i]), ddfFRRC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIFRRC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfFRRC) * stdErrFRRC, diffTRMeans[i] + qt(alpha/2, ddfFRRC) * stdErrFRRC))
    }
    ciDiffTrtFRRC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrFRRC, choose(I, 2)), DF = rep(ddfFRRC, choose(I, 2)), t = tStat, p = tPr, CI = CIFRRC)
    colnames(ciDiffTrtFRRC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
    
    dfSingleFRRC <- array(dim = I)
    msDenSingleFRRC <- array(dim = I)
    stdErrSingleFRRC <- array(dim = I)
    CISingleFRRC <- array(dim = c(I, 2))
    for (i in 1:I) {
      msDenSingleFRRC[i] <- varSingle[i] + (J - 1) * cov2Single[i]
      dfSingleFRRC[i] <- Inf
      stdErrSingleFRRC[i] <- sqrt(msDenSingleFRRC[i]/J)
      CISingleFRRC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i], trMeans[i] + qt(alpha/2, dfSingleFRRC[i]) * stdErrSingleFRRC[i]))
    }
    ciAvgRdrEachTrtFRRC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleFRRC, DF = dfSingleFRRC, CI = CISingleFRRC, row.names = NULL)
    colnames(ciAvgRdrEachTrtFRRC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    
    diffTRMeansFRRC <- array(dim = c(J, choose(I, 2)))
    for (j in 1:J) {
      ii <- 1
      for (i in 1:I) {
        if (i == I) 
          break
        for (ip in (i + 1):I) {
          diffTRMeansFRRC[j, ii] <- fomArray[i, j] - fomArray[ip, j]
          ii <- ii + 1
        }
      }
    }
    
    diffTRMeansFRRC <- as.vector(t(diffTRMeansFRRC))
    stdErrFRRC <- sqrt(2 * (varEchRder - cov1EchRder))
    stdErrFRRC <- rep(stdErrFRRC, choose(I, 2))
    dim(stdErrFRRC) <- c(J, choose(I, 2))
    stdErrFRRC <- as.vector(t(stdErrFRRC))
    readerNames <- rep(readerID, choose(I, 2))
    dim(readerNames) <- c(J, choose(I, 2))
    readerNames <- as.vector(t(readerNames))
    trNames <- rep(diffTRName, J)
    dfReaderFRRC <- rep(Inf, length(stdErrFRRC))
    CIReaderFRRC <- array(dim = c(length(stdErrFRRC), 2))
    tStat <- vector()
    tPr <- vector()
    for (n in 1:length(stdErrFRRC)) {
      tStat[n] <- diffTRMeansFRRC[n]/stdErrFRRC[n]
      tPr[n] <- 2 * pt(abs(tStat[n]), dfReaderFRRC[n], lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
      CIReaderFRRC[n, ] <- sort(c(diffTRMeansFRRC[n] - qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n], diffTRMeansFRRC[n] + qt(alpha/2, dfReaderFRRC[n]) * stdErrFRRC[n]))
    }
    ciDiffTrtEachRdr <- data.frame(Reader = readerNames, Treatment = trNames, Estimate = diffTRMeansFRRC, StdErr = stdErrFRRC, DF = dfReaderFRRC, t = tStat, p = tPr, CI = CIReaderFRRC)
    colnames(ciDiffTrtEachRdr) <- c("Reader", "Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
    
    varCovEachRdr <- data.frame(readerID, varEchRder, cov1EchRder)
    colnames(varCovEachRdr) <- c("Reader", "Var", "Cov1")
    if (option == "FRRC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fFRRC = fFRRC, ddfFRRC = ddfFRRC, pFRRC = pFRRC, ciDiffTrtFRRC = ciDiffTrtFRRC, ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, ciDiffTrtEachRdr = ciDiffTrtEachRdr, varCovEachRdr = varCovEachRdr
      ))
    }
  }
  
  # ************ RRFC ****************
  if (option %in% c("RRFC", "ALL")) {
    if (J > 1) {
      msDenRRFC <- msTR
      fRRFC <- msNum/msDenRRFC
      ddfRRFC <- ((I - 1) * (J - 1))
      pRRFC <- 1 - pf(fRRFC, I - 1, ddfRRFC)
      stdErrRRFC <- sqrt(2 * msDenRRFC/J)
      tStat <- vector()
      tPr <- vector()
      CIRRFC <- array(dim = c(length(diffTRMeans), 2))
      for (i in 1:length(diffTRMeans)) {
        tStat[i] <- diffTRMeans[i]/stdErrRRFC
        tPr[i] <- 2 * pt(abs(tStat[i]), ddfRRFC, lower.tail = FALSE)  # critical correction, noted by user Lucy D'Agostino McGowan
        CIRRFC[i, ] <- sort(c(diffTRMeans[i] - qt(alpha/2, ddfRRFC) * stdErrRRFC, diffTRMeans[i] + qt(alpha/2, ddfRRFC) * stdErrRRFC))
      }
      ciDiffTrtRRFC <- data.frame(Treatment = diffTRName, Estimate = diffTRMeans, StdErr = rep(stdErrRRFC, choose(I, 2)), DF = rep(ddfRRFC, choose(I, 2)), t = tStat, p = tPr, CI = CIRRFC)
      colnames(ciDiffTrtRRFC) <- c("Treatment", "Estimate", "StdErr", "DF", "t", "PrGTt", "CILower", "CIUpper")
      
      dfSingleRRFC <- array(dim = I)
      msDenSingleRRFC <- array(dim = I)
      stdErrSingleRRFC <- array(dim = I)
      CISingleRRFC <- array(dim = c(I, 2))
      for (i in 1:I) {
        msDenSingleRRFC[i] <- msRSingle[i]
        dfSingleRRFC[i] <- (J - 1)
        stdErrSingleRRFC[i] <- sqrt(msDenSingleRRFC[i]/J)
        CISingleRRFC[i, ] <- sort(c(trMeans[i] - qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i], trMeans[i] + qt(alpha/2, dfSingleRRFC[i]) * stdErrSingleRRFC[i]))
      }
      ciAvgRdrEachTrtRRFC <- data.frame(Treatment = modalityID, Area = trMeans, StdErr = stdErrSingleRRFC, DF = dfSingleRRFC, CI = CISingleRRFC, row.names = NULL)
      colnames(ciAvgRdrEachTrtRRFC) <- c("Treatment", "Area", "StdErr", "DF", "CILower", "CIUpper")
    } else {
      fRRFC <- NA
      ddfRRFC <- NA
      pRRFC <- NA
      ciDiffTrtRRFC <- NA
      ciAvgRdrEachTrtRRFC <- NA
    }
    if (option == "RRFC"){
      return(list(fomArray = fomArray, msT = msT, msTR = msTR, varComp = varComp, 
                  fRRFC = fRRFC, ddfRRFC = ddfRRFC, pRRFC = pRRFC, ciDiffTrtRRFC = ciDiffTrtRRFC, ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
    }
  }
  
  return(list(
    fomArray = fomArray, 
    msT = msT, 
    msTR = msTR, 
    varComp = varComp, 
    fRRRC = fRRRC, 
    ndf = I - 1, 
    ddfRRRC = ddfRRRC, 
    pRRRC = pRRRC, 
    ciDiffTrtRRRC = ciDiffTrtRRRC, 
    ciAvgRdrEachTrtRRRC = ciAvgRdrEachTrtRRRC, 
    fFRRC = fFRRC, 
    ddfFRRC = ddfFRRC, 
    pFRRC = pFRRC, 
    ciDiffTrtFRRC = ciDiffTrtFRRC, 
    ciAvgRdrEachTrtFRRC = ciAvgRdrEachTrtFRRC, 
    ciDiffTrtEachRdr = ciDiffTrtEachRdr, 
    varCovEachRdr = varCovEachRdr, 
    fRRFC = fRRFC, 
    ddfRRFC = ddfRRFC, 
    pRRFC = pRRFC, 
    ciDiffTrtRRFC = ciDiffTrtRRFC, 
    ciAvgRdrEachTrtRRFC = ciAvgRdrEachTrtRRFC))
} 




#' @importFrom stats runif
gpfEstimateVarCov <- function(fomArray, NL, LL, lesionNum, lesionID, 
                              lesionWeight, maxNL, maxLL, FOM, covEstMethod, nBoots, FPFValue = FPFValue) {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  
  K1 <- K - K2
  if (covEstMethod == "Jackknife") {
    if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
      jkFOMArray <- array(dim = c(I, J, K1))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K1) {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(lesionNum))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM)
          }
        }
      }
    } else if (FOM %in% c("MaxLLF", "HrSe")) {
      jkFOMArray <- array(dim = c(I, J, K2))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K2) {
            nl <- NL[i, j, -(k + K1), ]
            ll <- LL[i, j, -k, ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(lesionNum))
            lesionIDJk <- lesionID[-k, ]
            dim(lesionIDJk) <- c(K2 -1, max(lesionNum))
            lesionWeightJk <- lesionWeight[-k, ]
            dim(lesionWeightJk) <- c(K2 -1, max(lesionNum))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum[-k], lesionIDJk, lesionWeightJk, maxNL, maxLL, K1, K2 - 1, FOM)
          }
        }
      }
    } else {
      jkFOMArray <- array(dim = c(I, J, K))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K) {
            if (k <= K1) {
              nl <- NL[i, j, -k, ]
              ll <- LL[i, j, , ]
              dim(nl) <- c(K - 1, maxNL)
              dim(ll) <- c(K2, max(lesionNum))
              jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum, lesionID, 
                                              lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM, FPFValue = FPFValue)
            } else {
              nl <- NL[i, j, , ]
              ll <- LL[i, j, -(k - K1), ]
              dim(nl) <- c(K, maxNL)
              dim(ll) <- c(K2 - 1, max(lesionNum))
              lesionIDJk <- lesionID[-(k - K1), ]
              dim(lesionIDJk) <- c(K2 -1, max(lesionNum))
              lesionWeightJk <- lesionWeight[-(k - K1), ]
              dim(lesionWeightJk) <- c(K2 -1, max(lesionNum))
              jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum[-(k - K1)], lesionIDJk, 
                                              lesionWeightJk, maxNL, maxLL, K1, K2 - 1, FOM, FPFValue = FPFValue)
            }
          }
        }
      }
    }
    Cov <- ResamplingEstimateVarCovs(jkFOMArray)
    var <- Cov$var * (K - 1)^2/K  # see paper by Efron and Stein
    cov1 <- Cov$cov1 * (K - 1)^2/K
    cov2 <- Cov$cov2 * (K - 1)^2/K
    cov3 <- Cov$cov3 * (K - 1)^2/K
  } else if (covEstMethod == "Bootstrap") {
    if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
      fomBsArray <- array(dim = c(I, J, nBoots))
      for (b in 1:nBoots) {
        kBs <- ceiling(runif(K1) * K1)
        for (i in 1:I) {
          for (j in 1:J) {
            NLbs <- NL[i, j, kBs, ]
            LLbs <- LL[i, j, , ]
            dim(NLbs) <- c(K1, maxNL)
            dim(LLbs) <- c(K2, max(lesionNum))
            fomBsArray[i, j, b] <- gpfMyFOM(NLbs, LLbs, lesionNum, lesionID, lesionWeight, maxNL, maxLL, K1, K2, FOM)
          }
        }
      }
    } else if (FOM %in% c("MaxLLF", "HrSe")) {
      fomBsArray <- array(dim = c(I, J, nBoots))
      for (b in 1:nBoots) {
        kBs <- ceiling(runif(K2) * K2)
        for (i in 1:I) {
          for (j in 1:J) {
            NLbs <- NL[i, j, c(1:K1, (kBs + K1)), ]
            LLbs <- LL[i, j, kBs, ]
            dim(NLbs) <- c(K, maxNL)
            dim(LLbs) <- c(K2, max(lesionNum))
            lesionIDBs <- lesionID[kBs, ]
            dim(lesionIDBs) <- c(K2, max(lesionNum))
            lesionWeightBs <- lesionWeight[kBs, ]
            dim(lesionWeightBs) <- c(K2, max(lesionNum))
            fomBsArray[i, j, b] <- gpfMyFOM(NLbs, LLbs, lesionNum[kBs], lesionIDBs, lesionWeightBs, maxNL, maxLL, K1, K2, FOM)
          }
        }
      }
    } else { # original code had errors; see Fadi RRRC code; Aug 9, 2017 !!dpc!!!
      ## however, following code needs checking
      ##stop("this code needs checking; contact Dr. Chakraborty with dataset and code that lands here; 8/9/2017")
      fomBsArray <- array(dim = c(I, J, nBoots))
      for (b in 1:nBoots) {
        k1bs <- ceiling(runif(K1) * K1)
        k2bs <- ceiling(runif(K2) * K2)
        for (i in 1:I) {
          for (j in 1:J) {
            NLbs <- NL[i, j, c(k1bs, k2bs + K1), ]
            lesionNumbs <- lesionNum[k2bs]            
            LLbs <- LL[i, j, k2bs,1:max(lesionNumbs)] 
            dim(NLbs) <- c(K, maxNL)
            dim(LLbs) <- c(K2, max(lesionNumbs))  
            lesionIDBs <- lesionID[k2bs, ]
            dim(lesionIDBs) <- c(K2, max(lesionNum))
            lesionWeightBs <- lesionWeight[k2bs, ]
            dim(lesionWeightBs) <- c(K2, max(lesionNum))
            fomBsArray[i, j, b] <- gpfMyFOM(NLbs, LLbs, lesionNumbs, lesionIDBs, 
                                            lesionWeightBs, maxNL, maxLL, K1, K2, FOM, FPFValue = FPFValue)
          }
        }
      }
    }
    Cov <- ResamplingEstimateVarCovs(fomBsArray)
    var <- Cov$var
    cov1 <- Cov$cov1
    cov2 <- Cov$cov2
    cov3 <- Cov$cov3
  } else if (covEstMethod == "DeLong") {
    if (!FOM %in% c("Wilcoxon", "HrAuc", "ROI")) 
      stop("DeLong\"s method can only be used for trapezoidal figures of merit.")
    
    if (FOM == "ROI") {
      kI01 <- which(apply((NL[1, 1, , ] != UNINITIALIZED), 1, any))
      numKI01 <- rowSums((NL[1, 1, , ] != UNINITIALIZED))
      I01 <- length(kI01)
      I10 <- K2
      N <- sum((NL[1, 1, , ] != UNINITIALIZED))
      M <- sum(lesionNum)
      V01 <- array(dim = c(I, J, I01, maxNL))
      V10 <- array(dim = c(I, J, I10, max(lesionNum)))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:I10) {
            for (el in 1:lesionNum[k]) {
              V10[i, j, k, el] <- (sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) < LL[i, j, k, el]) 
                                   + 0.5 * sum(as.vector(NL[i, j, , ][NL[i, j, , ] != UNINITIALIZED]) == LL[i, j, k, el]))/N
            }
          }
          for (k in 1:I01) {
            for (el in 1:maxNL) {
              if (NL[i, j, kI01[k], el] == UNINITIALIZED) 
                next
              V01[i, j, k, el] <- (sum(NL[i, j, kI01[k], el] < as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])) 
                                   + 0.5 * sum(NL[i, j, kI01[k], el] == as.vector(LL[i, j, , ][LL[i, j, , ] != UNINITIALIZED])))/M
            }
          }
        }
      }
      s10 <- array(0, dim = c(I, I, J, J))
      s01 <- array(0, dim = c(I, I, J, J))
      s11 <- array(0, dim = c(I, I, J, J))
      for (i in 1:I) {
        for (ip in 1:I) {
          for (j in 1:J) {
            for (jp in 1:J) {
              for (k in 1:I10) {
                s10[i, ip, j, jp] <- (s10[i, ip, j, jp]
                                      + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])])
                                         - lesionNum[k] * fomArray[i, j])
                                      * (sum(V10[ip, jp, k, !is.na(V10[ip, jp, k, ])]) 
                                         - lesionNum[k] * fomArray[ip, jp]))
              }
              for (k in 1:I01) {
                s01[i, ip, j, jp] <- (s01[i, ip, j, jp] 
                                      + (sum(V01[i, j, k, !is.na(V01[i, j, k, ])]) 
                                         - numKI01[kI01[k]] * fomArray[i, j]) 
                                      * (sum(V01[ip, jp, k, !is.na(V01[ip, jp, k, ])]) 
                                         - numKI01[kI01[k]] * fomArray[ip, jp]))
              }
              allAbn <- 0
              for (k in 1:K2) {
                if (all(NL[ip, jp, k + K1, ] == UNINITIALIZED)) {
                  allAbn <- allAbn + 1
                  next
                }                  
                s11[i, ip, j, jp] <- (s11[i, ip, j, jp] 
                                      + (sum(V10[i, j, k, !is.na(V10[i, j, k, ])]) 
                                         - lesionNum[k] * fomArray[i, j]) 
                                      * (sum(V01[ip, jp, k + K1 - allAbn, !is.na(V01[ip, jp, k + K1 - allAbn, ])]) 
                                         - numKI01[K1 + k] * fomArray[ip, jp]))
              }
            }
          }
        }
      }
      s10 <- s10 * I10/(I10 - 1)/M
      s01 <- s01 * I01/(I01 - 1)/N
      s11 <- s11 * K/(K - 1)
      S <- array(0, dim = c(I, I, J, J))
      for (i in 1:I) {
        for (ip in 1:I) {
          for (j in 1:J) {
            for (jp in 1:J) {
              S[i, ip, j, jp] <- s10[i, ip, j, jp]/M + s01[i, ip, j, jp]/N + s11[i, ip, j, jp]/(M * N) + s11[ip, i, jp, j]/(M * N)
            }
          }
        }
      }
    } else {
      # ROC
      V10 <- array(dim = c(I, J, K2))
      V01 <- array(dim = c(I, J, K1))
      for (i in 1:I) {
        for (j in 1:J) {
          nl <- NL[i, j, 1:K1, ]
          ll <- cbind(NL[i, j, (K1 + 1):K, ], LL[i, j, , ])
          dim(nl) <- c(K1, maxNL)
          dim(ll) <- c(K2, maxNL + max(lesionNum))
          fp <- apply(nl, 1, max)
          tp <- apply(ll, 1, max)
          for (k in 1:K2) {
            V10[i, j, k] <- (sum(fp < tp[k]) + 0.5 * sum(fp == tp[k]))/K1
          }
          for (k in 1:K1) {
            V01[i, j, k] <- (sum(fp[k] < tp) + 0.5 * sum(fp[k] == tp))/K2
          }
        }
      }
      s10 <- array(dim = c(I, I, J, J))
      s01 <- array(dim = c(I, I, J, J))
      for (i in 1:I) {
        for (ip in 1:I) {
          for (j in 1:J) {
            for (jp in 1:J) {
              s10[i, ip, j, jp] <- sum((V10[i, j, ] - fomArray[i, j]) * (V10[ip, jp, ] - fomArray[ip, jp]))/(K2 - 1)
              s01[i, ip, j, jp] <- sum((V01[i, j, ] - fomArray[i, j]) * (V01[ip, jp, ] - fomArray[ip, jp]))/(K1 - 1)
            }
          }
        }
      }
      S <- s10/K2 + s01/K1
    }
    Cov <- VarCovs(S)
    var <- Cov$var
    cov1 <- Cov$cov1
    cov2 <- Cov$cov2
    cov3 <- Cov$cov3
  } else stop("incorrect covariance estimation method specified")
  
  return(list(var = var, cov1 = cov1, cov2 = cov2, cov3 = cov3))
} 


VarCovs <- function(covariances) {
  var <- 0
  count <- 0
  I <- dim(covariances)[1]
  J <- dim(covariances)[3]
  for (i in 1:I) {
    for (j in 1:J) {
      var <- var + covariances[i, i, j, j]
      count <- count + 1
    }
  }
  var <- var/count
  
  cov1 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        if (ip != i) {
          cov1 <- cov1 + covariances[i, ip, j, j]
          count <- count + 1
        }
      }
    }
  }
  cov1 <- cov1/count
  
  cov2 <- 0
  count <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (jp in 1:J) {
        if (j != jp) {
          cov2 <- cov2 + covariances[i, i, j, jp]
          count <- count + 1
        }
      }
    }
  }
  # cov2 <- cov2 / (I*J*(J-1)) # OK, DPC
  cov2 <- cov2/count
  
  cov3 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      if (i != ip) {
        for (j in 1:J) {
          for (jp in 1:J) {
            if (j != jp) {
              cov3 <- cov3 + covariances[i, ip, j, jp]
              count <- count + 1
            }
          }
        }
      }
    }
  }
  
  # cov3 <- cov3 / (I*(I-1)*J*(J-1)) # not OK; general advice; better to let computer do the thinking
  cov3 <- cov3/count
  
  return(list(var = var, cov1 = cov1, cov2 = cov2, cov3 = cov3))
} 


ResamplingEstimateVarCovs <- function(resampleMatrix) {
  I <- dim(resampleMatrix)[1]
  J <- dim(resampleMatrix)[2]
  covariances <- array(dim = c(I, I, J, J))
  
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          covariances[i, ip, j, jp] <- cov(resampleMatrix[i, j, ], resampleMatrix[ip, jp, ])
        }
      }
    }
  }
  
  ret <- VarCovs(covariances)
  return(list(var = ret$var, cov1 = ret$cov1, cov2 = ret$cov2, cov3 = ret$cov3))
}

