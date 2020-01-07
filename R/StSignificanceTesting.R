#' Perform DBM or OR significance testing
#' 
#' @description  Performs Dorfman-Berbaum-Metz (DBM) or Obuchowski-Rockette (OR) 
#'    significance testing (with Hillis' improvements), for specified dataset; 
#'    significance testing refers to analysis designed to assign a P-value, 
#'    and other statistics, for 
#'    rejecting the null hypothesis (NH) that the reader-averaged 
#'    figure of merit (FOM) difference between treatments is zero. The results of 
#'    the analysis are better visualized in the text or  
#'    Excel-formatted files produced by \code{\link{UtilOutputReport}}. 
#'
#'  
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}. 
#'     \bold{Must have two or more treatments and two or more readers.} 
#' @param FOM The figure of merit, see \code{\link{UtilFigureOfMerit}}
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' @param alpha The significance level of the test of the null hypothesis that all 
#'    treatment effects are zero; the default is 0.05
#' @param method The significance testing method to be used. There are two options: 
#'    \code{"DBMH"} (the default) or \code{"ORH"}, representing the Dorfman-Berbaum-Metz
#'    and the Obuchowski-Rockette significance testing methods, respectively. 
#' @param covEstMethod The covariance matrix estimation method
#'    in \code{ORH} analysis (for \code{method = "DBMH"} the jackknife is always used).
#'    \itemize{ 
#'    \item \code{"Jackknife"}, the default, 
#'    \item \code{"Bootstrap"}, in which case \code{nBoots} (above) is relevant 
#'    \item \code{"DeLong"}; requires \code{FOM = "Wilcoxon"}, otherwise 
#'    an error results.
#' }   
#' @param nBoots The number of bootstraps (defaults to 200), relevant only if 
#'    \code{covEstMethod = "Bootstrap"} and \code{method = "ORH"} 
#' @param option Determines which factors are regarded as random vs. fixed:
#' \itemize{ 
#'    \item \code{"RRRC"} = random-reader random case, 
#'    \item \code{"FRRC"} = fixed-reader random case, 
#'    \item \code{"RRFC"} = random-reader fixed case, 
#'    \item \code{"ALL"} = outputs the results of \code{"RRRC"}, \code{"FRRC"} 
#'    and \code{"RRFC"} analyses
#' }    
#' @param tempOrgCode, default FALSE; if TRUE, then code from version 0.0.1 of RJafroc
#'    is used (see RJafroc_0.0.1.tar). This is intended to check against errors 
#'    that crept in subsequent to 
#'    the original version as I attempted to improve the organization of the code and the output.
#'    As implicit in the name of this temporary flag, it will eventually be removed. 
#' 
#' @return \strong{For \code{method = "DBMH"}  the returned list has 15 members:}
#' @return \item{fomArray}{Figure of merit array: see return of \code{\link{UtilFigureOfMerit}}}
#' @return \item{anovaY}{ ANOVA table of the pseudovalues, over all treatments}
#' @return \item{anovaYi}{ANOVA table of the pseudovalues, for each treatment}
#' @return \item{varComp}{The variance components of the DBM pseudovalue mode; 
#'    6 values, in the following order: \code{varR}, \code{varC}, 
#'    \code{varTR}, \code{varTC}, \code{varRC} and \code{varErr}}
#' @return \item{FTestStatsRRRC}{Results of the F-test for RRRC \strong{random reader random case}
#'    analysis; contains the following items: \code{fRRRC} - the value of the F-statistic, 
#'    \code{ndfRRRC} - the numerator degrees of freedom, \code{ddfRRRC} - 
#'    the denominator degrees of freedom and \code{pRRRC} - the pvalue}
#' @return \item{ciDiffTrtRRRC}{For RRRC analysis, the confidence intervals and related 
#'    statistics for the FOM differences between pairs of treatments}
#' @return \item{ciAvgRdrEachTrtRRRC}{For RRRC analysis, the confidence intervals 
#'    and related test statistics for rdr. avg. FOM in each treatment}
#' @return \item{FTestStatsFRRC}{As for \code{FTestStatsRRRC} except that this is 
#'    for \strong{fixed-reader random-case} (FRRC) analysis}
#' @return \item{ciDiffTrtFRRC}{For FRRC analysis, the confidence intervals and related 
#'    test statistics for the FOM differences between pairs of treatments}
#' @return \item{ciAvgRdrEachTrtFRRC}{For FRRC analysis, the confidence intervals and 
#'    related tests for rdr. avg. FOM in each treatment}
#' @return \item{msAnovaEachRdrFRRC}{The mean squares table of the ANOVA of the 
#'    pseudovalues for each reader (based on data for the specified reader)}
#' @return \item{ciDiffTrtEachRdrFRRC}{The confidence intervals and related tests of the 
#'    FOM differences between pairs of treatments for each reader}
#' @return \item{FTestStatsRRFC}{As for \code{FTestStatsRRRC} except that this is 
#'    for \strong{random-reader fixed-case} (RRFC) analysis}
#' @return \item{ciDiffTrtRRFC}{For RRFC analysis, the confidence intervals and 
#'    related test statistics for the FOM differences between pairs of treatments}
#' @return \item{ciAvgRdrEachTrtRRFC}{For RRFC analysis, the confidence intervals 
#'    and related tests for reader averaged FOM in each treatment}
#' 
#' 
#' @return \strong{For method = "ORH" the return list has 14 members:}
#' @return \item{fomArray}{Figure of merit array: see return of \code{\link{UtilFigureOfMerit}}}
#' @return \item{meanSquares}{List with 3 members: \code{msT}, \code{msR}, \code{msTR}}
#' @return \item{varComp}{The variance components of the OR figure of merit model; 
#'    6 values, listed in the following order: \code{varR}, \code{varTR}, \code{cov1}, 
#'    \code{cov2}, \code{cov3} and \code{var}}
#' @return \item{FTestStatsRRRC}{Results of the F-test for RRRC \strong{random reader random case}
#'    analysis; contains the following items: \code{fRRRC} - the value of the F-statistic, 
#'    \code{ndfRRRC} - the numerator degrees of freedom, \code{ddfRRRC} - 
#'    the denominator degrees of freedom and \code{pRRRC} - the pvalue}
#' @return \item{ciDiffTrtRRRC}{Same as in \code{DBMH} method}
#' @return \item{ciAvgRdrEachTrtRRRC}{Same as in \code{DBMH} method}
#' @return \item{FTestStatsFRRC}{Same as \code{FTestStatsRRRC}, but 
#'    now treating reader as a fixed effect}
#' @return \item{ciDiffTrtFRRC}{Same as in \code{DBMH} method}
#' @return \item{ciAvgRdrEachTrtFRRC}{Same as in \code{DBMH} method}
#' @return \item{ciDiffTrtEachRdrFRRC}{Same as in \code{DBMH} method}
#' @return \item{varCovEachRdr}{Var and Cov1 
#'    estimates for each reader}
#' @return \item{FTestStatsRRFC}{Same as \code{FTestStatsRRRC}, but 
#'    now treating case as a fixed effect}
#' @return \item{ciDiffTrtRRFC}{Same as in \code{DBMH} method}
#' @return \item{ciAvgRdrEachTrtRRFC}{Same as in \code{DBMH} method}
#' 
#' 
#' @examples
#' StSignificanceTesting(dataset02,FOM = "Wilcoxon", method = "DBMH") 
#' StSignificanceTesting(dataset02,FOM = "Wilcoxon", method = "ORH")
#' 
#' \donttest{
#' StSignificanceTesting(dataset05, FOM = "wAFROC")
#' StSignificanceTesting(dataset05, FOM = "HrAuc", method = "DBMH") 
#' StSignificanceTesting(dataset05, FOM = "SongA1", method = "DBMH") 
#' StSignificanceTesting(dataset05, FOM = "SongA2", method = "DBMH") 
#' StSignificanceTesting(dataset05, FOM = "FOM_wAFROC1", method = "DBMH")
#' StSignificanceTesting(dataset05, FOM = "FOM_AFROC1", method = "DBMH")
#' StSignificanceTesting(dataset05, FOM = "FOM_AFROC", method = "DBMH")
#' } 
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
StSignificanceTesting <- function(dataset, FOM, FPFValue = 0.2, alpha = 0.05, method = "DBMH", 
                                  covEstMethod = "Jackknife", nBoots = 200, option = "ALL", tempOrgCode = FALSE)
{
  
  if (dataset$dataType == "ROI") {
    method <- "ORH"
    covEstMethod <- "DeLong" 
    FOM <- "ROI"
    cat("ROI dataset: forcing method = `ORH`, covEstMethod = `DeLong` and FOM = `ROI`.\n")
  }
  
  if (!option %in% c("RRRC", "FRRC", "RRFC", "ALL")){
    errMsg <- sprintf("%s is not a valid option.", option)
    stop(errMsg)
  }    
  
  if (length(dataset$modalityID) < 2) {
    ErrMsg <- paste0("This analysis requires at least 2 treatments", 
                     "\nUse StSignificanceTestingSingleFixedFactor() for single treatment analysis.")
    stop(ErrMsg)
  }
  
  if (length(dataset$NL[1,,1,1]) < 2) {
    ErrMsg <- paste0("This analysis requires at least 2 readers", 
                     "\nUse StSignificanceTestingSingleFixedFactor() for single reader analysis.")
    stop(ErrMsg)
  }
  
  if (method == "DBMH"){
    if (covEstMethod != "Jackknife") 
      stop("For DBMH method covariance estimation method covEstMethod must be Jackknife")
  }
  
  if (!tempOrgCode) {
    if (method == "DBMH"){
      return(StDBMHAnalysis(dataset, FOM, FPFValue, alpha, option)) # current code
    } else if (method == "ORH"){
      return(StORHAnalysis(dataset, FOM, FPFValue, alpha, covEstMethod, nBoots, option)) # current code
    } else {
      errMsg <- sprintf("%s is not a valid analysis method.", method)
      stop(errMsg)
    }
  } else {
    if (method == "DBMH"){
      return(DBMHAnalysis(dataset, FOM, alpha, option)) # original code: StOldCode.R
    } else if (method == "ORH"){
      return(ORHAnalysis(dataset, FOM, alpha, covEstMethod, nBoots, option)) # original code: StOldCode.R
    } else {
      errMsg <- sprintf("%s is not a valid analysis method.", method)
      stop(errMsg)
      
    }
    
  }
  
}



gpfEstimateVarCov <- function(dataset, FOM, FPFValue, nBoots, covEstMethod) 
{
  
  if (covEstMethod == "Jackknife") {
    
    ret <- varComponentsJackknife(dataset, FOM, FPFValue)
    
  } 
  
  else if (covEstMethod == "Bootstrap") {
    
    ret <- varComponentsBootstrap (dataset, FOM, FPFValue, nBoots)
    
  } 
  
  else if (covEstMethod == "DeLong") {
    
    ret <- varComponentsDeLong (dataset, FOM)
    
  } 
  
  else stop("incorrect covariance estimation method specified")
  
  return(ret)
  
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
  
  ret <- ORVarianceCovariances(covariances)
  return(list(var = ret$var, cov1 = ret$cov1, cov2 = ret$cov2, cov3 = ret$cov3))
}




ORVarianceCovariances <- function(covariances) {
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
  if (count > 0) var <- var/count else var <- NaN
  
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
  if (count > 0) cov1 <- cov1/count else cov1 <- NaN
  
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
  if (count > 0) cov2 <- cov2/count else cov2 <- NaN
  
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
  if (count > 0) cov3 <- cov3/count else cov3 <- NaN
  
  return(list(var = var, cov1 = cov1, cov2 = cov2, cov3 = cov3))
} 




pseudoValueMeanSquares <- function (pseudoValues)
{
  I <- length(pseudoValues[,1,1])
  J <- length(pseudoValues[1,,1])
  K <- length(pseudoValues[1,1,])
  
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
        msTRC <- msTRC + 
          (pseudoValues[i, j, k] - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) - mean(pseudoValues[, j, k]) + 
             mean(pseudoValues[i, , ]) + mean(pseudoValues[, j, ]) + mean(pseudoValues[, , k]) - mean(pseudoValues))^2
      }
    }
  }
  msTRC <- msTRC/((I - 1) * (J - 1) * (K - 1))
  
  mSquares <- data.frame(msT = msT,
                         msR = msR,
                         msC = msC,
                         msTR = msTR,
                         msTC = msTC,
                         msRC = msRC,
                         msTRC = msTRC)
  
  return(mSquares) 
}


# 
# pseudoValues <- function(dataset, FOM, FPFValue) {
#   
#   if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
#     stop("This needs fixing")
#     ret <- jackknifePseudoValuesNormals(dataset, FOM, FPFValue)
#     
#   } else if (FOM %in% c("MaxLLF", "HrSe")) {
#     stop("This needs fixing")
#     
#     ret <- jackknifePseudoValuesAbnormals(dataset, FOM, FPFValue)
#     
#   } else {
#     
#     # ret <- jackknifePseudoValues(dataset, FOM, FPFValue)
#     ret <- UtilPseudoValues(dataset, FOM, FPFValue)
#   }
#   return(ret$jkPseudoValues)
# }
# 

varComponentsJackknife <- function(dataset, FOM, FPFValue) {
  
  K <- length(dataset$NL[1,1,,1])
  # 
  # if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
  # 
  #   ret <- jackknifePseudoValuesNormals(dataset, FOM, FPFValue)
  #   
  # } else if (FOM %in% c("MaxLLF", "HrSe")) {
  # 
  #   ret <- jackknifePseudoValuesAbnormals(dataset, FOM, FPFValue)
  #   
  # } else {
  #   
    ret <- UtilPseudoValues(dataset, FOM, FPFValue)
    
    # ret <- jackknifePseudoValues(dataset, FOM, FPFValue)
    
  # }
  
  CovTemp <- ResamplingEstimateVarCovs(ret$jkFomValues)
  Cov <- list(
    var = CovTemp$var * (K-1)^2/K,
    cov1 = CovTemp$cov1 * (K-1)^2/K,
    cov2 = CovTemp$cov2 * (K-1)^2/K,
    cov3 = CovTemp$cov3 * (K-1)^2/K
  )
  
  return(Cov)
  
}




#' @importFrom stats runif
varComponentsBootstrap <- function(dataset, FOM, FPFValue, nBoots) 
{
  
  NL <- dataset$NL
  LL <- dataset$LL
  lesionVector <- dataset$lesionVector
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  
  I <- length(NL[,1,1,1])
  J <- length(NL[1,,1,1])
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- (K - K2)
  maxNL <- length(NL[1,1,1,])
  maxLL <- length(LL[1,1,1,])
  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    stop("This needs fixing")
    fomBsArray <- array(dim = c(I, J, nBoots))
    for (b in 1:nBoots) {
      kBs <- ceiling(runif(K1) * K1)
      for (i in 1:I) {
        for (j in 1:J) {
          NLbs <- NL[i, j, kBs, ]
          LLbs <- LL[i, j, , ]
          dim(NLbs) <- c(K1, maxNL)
          dim(LLbs) <- c(K2, maxLL)
          fomBsArray[i, j, b] <- gpfMyFOM(NLbs, LLbs, 
                                          lesionVector, lesionID, 
                                          lesionWeight, maxNL, 
                                          maxLL, K1, K2, 
                                          FOM, FPFValue)
        }
      }
    }
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    stop("This needs fixing")
    fomBsArray <- array(dim = c(I, J, nBoots))
    for (b in 1:nBoots) {
      kBs <- ceiling(runif(K2) * K2)
      for (i in 1:I) {
        for (j in 1:J) {
          NLbs <- NL[i, j, c(1:K1, (kBs + K1)), ]
          LLbs <- LL[i, j, kBs, ]
          dim(NLbs) <- c(K, maxNL)
          dim(LLbs) <- c(K2, maxLL)
          lesionIDBs <- lesionID[kBs, ]
          dim(lesionIDBs) <- c(K2, maxLL)
          lesionWeightBs <- lesionWeight[kBs, ]
          dim(lesionWeightBs) <- c(K2, maxLL)
          fomBsArray[i, j, b] <- gpfMyFOM(NLbs, LLbs, 
                                          lesionVector[kBs], lesionIDBs, 
                                          lesionWeightBs, maxNL, maxLL, 
                                          K1, K2, FOM, FPFValue)
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
          lesionVectorbs <- lesionVector[k2bs]            
          LLbs <- LL[i, j, k2bs,1:max(lesionVectorbs)] 
          dim(NLbs) <- c(K, maxNL)
          dim(LLbs) <- c(K2, max(lesionVectorbs))  
          lesionIDBs <- lesionID[k2bs, ]
          dim(lesionIDBs) <- c(K2, maxLL)
          lesionWeightBs <- lesionWeight[k2bs, ]
          dim(lesionWeightBs) <- c(K2, maxLL)
          fomBsArray[i, j, b] <- gpfMyFOM(NLbs, LLbs, lesionVectorbs, lesionIDBs, 
                                          lesionWeightBs, maxNL, maxLL, K1, K2, FOM, FPFValue)
        }
      }
    }
  }
  Cov <- ResamplingEstimateVarCovs(fomBsArray)
  var <- Cov$var
  cov1 <- Cov$cov1
  cov2 <- Cov$cov2
  cov3 <- Cov$cov3
  
  return(list(var = var, cov1 = cov1, cov2 = cov2, cov3 = cov3))
  
}




varComponentsDeLong <- function(dataset, FOM)
{
  
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  NL <- dataset$NL
  LL <- dataset$LL
  lesionVector <- dataset$lesionVector
  
  I <- length(NL[,1,1,1])
  J <- length(NL[1,,1,1])
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- (K - K2)
  maxNL <- length(NL[1,1,1,])
  maxLL <- length(LL[1,1,1,])
  # if ((maxLL != 1) || (maxLL != 1)) stop("dataset error in varComponentsDeLong")
  
  fomArray <- UtilFigureOfMerit(dataset, FOM)
  
  if (!FOM %in% c("Wilcoxon", "HrAuc", "ROI")) 
    stop("DeLong\"s method can only be used for trapezoidal figures of merit.")
  
  if (FOM == "ROI") {
    kI01 <- which(apply((NL[1, 1, , ] != UNINITIALIZED), 1, any))
    numKI01 <- rowSums((NL[1, 1, , ] != UNINITIALIZED))
    I01 <- length(kI01)
    I10 <- K2
    N <- sum((NL[1, 1, , ] != UNINITIALIZED))
    M <- sum(lesionVector)
    V01 <- array(dim = c(I, J, I01, maxNL))
    V10 <- array(dim = c(I, J, I10, maxLL))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:I10) {
          for (el in 1:lesionVector[k]) {
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
                                       - lesionVector[k] * fomArray[i, j])
                                    * (sum(V10[ip, jp, k, !is.na(V10[ip, jp, k, ])]) 
                                       - lesionVector[k] * fomArray[ip, jp]))
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
                                       - lesionVector[k] * fomArray[i, j]) 
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
        dim(ll) <- c(K2, maxNL + maxLL)
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
  Cov <- ORVarianceCovariances(S)
  var <- Cov$var
  cov1 <- Cov$cov1
  cov2 <- Cov$cov2
  cov3 <- Cov$cov3
  
  return(list(var = var, cov1 = cov1, cov2 = cov2, cov3 = cov3))
}
