#' Perform significance testing for single fixed factor analysis
#' 
#' @description  Significance testing for datasets with a single reader in 
#'    multiple (at least two) treatments, or a single treatment with multiple 
#'    (at least two) readers, where reader or treatment, respectively, is 
#'    regarded as a fixed factor and a common case-set, regarded as random, 
#'    is assumed.
#' 
#' @param dataset A single-treatment or single-reader dataset.
#' @param FOM The figure of merit, see \code{\link{UtilFigureOfMerit}}.
#' @param alpha The significance level (\code{alpha}, default 0.05) 
#'    of the test of the null hypothesis that FOMs of all levels of 
#'    the fixed factor are identical.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return The return value is a list containing:
#' @return \item{f}{The observed F-statistic for testing the null 
#'    hypothesis of no treatment effect.}
#' @return \item{ddf}{The denominator degrees of freedom of the F statistic. 
#'    The numerator degrees of freedom is always the number of levels of the 
#'    fixed factor minus one.}
#' @return \item{pValue}{The p-value for rejecting the NH.}
#' @return \item{fomStats}{Statistics for FOM for each level of the fixed factor.}
#' @return \item{diffFomStats}{Statistics for FOM-differences for all distinct 
#'    pairings of the levels of the fixed factor} 
#' 
#' @details This function performs implements Hillis et al. 2005, Eqn. 23. 
#'    Following an overall F-test, reader-pairings are compared using paired 
#'    t-tests. \strong{In order for a specific pairing to be declared 
#'    significant, the F-test must also be significant.}
#' 
#' @examples 
#' ## Create a single treatment ROC dataset with one treatment and four readers
#' singleFactorData <- DfExtractDataset(dataset02, 1, 1:4)
#' 
#' ## Performs single treatment fixed reader analysis
#' StSignificanceTestingSingleFixedFactor(singleFactorData, FOM = "Wilcoxon")
#' 
#' @references 
#' Hillis SL, Obuchowski NA, Schartz KM, Berbaum KS (2005) A comparison of the Dorfman-Berbaum-Metz and Obuchowski-Rockette methods for receiver operating characteristic (ROC) data, 
#' Statistics in Medicine, 24(10), 1579-607.
#' 
#' Hillis SL (2007) A comparison of denominator degrees of freedom methods 
#' for multiple observer ROC studies, Statistics in Medicine. 26:596-619.
#' 
#' Hillis SL (2014) A marginal mean ANOVA approach for analyzing multireader multicase radiological imaging data,
#' Statistics in medicine 33, 330-360.
#' 
#' @importFrom stats t.test
#' 
#' @export

StSignificanceTestingSingleFixedFactor <- function(dataset, FOM, FPFValue = 0.2, alpha = 0.05) { # FPFValue only needed for LROC data
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(dataset$NL)[3]

  MS <- UtilMeanSquares(dataset, FOM, method = "DBMH", FPFValue)
  pseudoValues <- UtilPseudoValues(dataset, FOM, FPFValue)$jkPseudoValues
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  if (I == 1 && J != 1){
    fDbmFixed <- MS$msR / MS$msRC
    ddf <- (J - 1) * (K - 1)
    pValue <- pf(fDbmFixed, J - 1, ddf, lower.tail = FALSE)
    
    iniNA <- rep(NA, J)
    fomStats <- data.frame(reader = iniNA, 
                           Area = iniNA, 
                           stdErr = iniNA, 
                           df = iniNA, 
                           ciLower = iniNA, 
                           ciUpper = iniNA )
    for (j in 1: J){
      stdErr <- sqrt(MS$msCSingleR[j] / K / I)
      df <- K - 1
      ciHalfWidth <- qt(alpha/2, df, lower.tail = FALSE) * stdErr
      fomStats[j, ] <- c(readerID[j], fomArray[j], stdErr, df, fomArray[j] - ciHalfWidth, fomArray[j] + ciHalfWidth)
    }
    names(fomStats) <- c("Reader", "Area", "stdErr", "DF", "CILower", "CIUpper")
    
    nPairs <- choose(J, 2)
    jPair <- 1
    iniNA <- rep(NA, nPairs)
    ret <- data.frame(reader = iniNA, 
                      diff = iniNA, 
                      tVal = iniNA, 
                      df = iniNA, 
                      pVal = iniNA, 
                      ciLower = iniNA, 
                      ciUpper = iniNA )
    for (j in 1:(J - 1)){
      for (jp in (j + 1):J){
        retTmp <- t.test(pseudoValues[1, j, ], pseudoValues[1, jp, ], paired = TRUE, conf.level = 1 - alpha)
        readerName <- paste0(readerID[j], " - ", readerID[jp])
        ret[jPair, 1] <- readerName
        ret[jPair, 2:length(ret[jPair, ])] <- c(retTmp$estimate, retTmp$statistic, retTmp$parameter, retTmp$p.value, retTmp$conf.int[1], retTmp$conf.int[2])
        jPair <- jPair + 1
      }
    }
    names(ret) <- c("Reader", "Difference", "t", "DF", "PrGTt", "CILower", "CIUpper")
  } else if (I != 1 && J == 1){
    fDbmFixed <- MS$msT / MS$msTC
    ddf <- (I - 1) * (K - 1)
    pValue <- pf(fDbmFixed, I - 1, ddf, lower.tail = FALSE)
    
    iniNA <- rep(NA, I)
    fomStats <- data.frame(reader = iniNA, 
                           Area = iniNA, 
                           stdErr = iniNA, 
                           df = iniNA, 
                           ciLower = iniNA, 
                           ciUpper = iniNA )
    for (i in 1: I){
      stdErr <- sqrt(MS$msCSingleT[i] / K / J)
      df <- K - 1
      ciHalfWidth <- qt(alpha/2, df, lower.tail = FALSE) * stdErr
      fomStats[i, ] <- c(modalityID[i], fomArray[i], stdErr, df, fomArray[i] - ciHalfWidth, fomArray[i] + ciHalfWidth)
    }
    names(fomStats) <- c("Modality", "Area", "stdErr", "DF", "CILower", "CIUpper")
    
    nPairs <- choose(I, 2)
    iPair <- 1
    iniNA <- rep(NA, nPairs)
    ret <- data.frame(treatment = iniNA, 
                      diff = iniNA, 
                      tVal = iniNA, 
                      df = iniNA, 
                      pVal = iniNA, 
                      ciLower = iniNA, 
                      ciUpper = iniNA )
    for (i in 1:(I - 1)){
      for (ip in (i + 1):I){
        retTmp <- t.test(pseudoValues[i, 1, ], pseudoValues[ip, 1, ], paired = TRUE, conf.level = 1 - alpha)
        modalityName <- paste0(modalityID[i], " - ", modalityID[ip])
        ret[iPair, 1] <- modalityName
        ret[iPair, 2:length(ret[iPair, ])] <- c(retTmp$estimate, retTmp$statistic, retTmp$parameter, retTmp$p.value, retTmp$conf.int[1], retTmp$conf.int[2])
        iPair <- iPair + 1
      }
    }
    names(ret) <- c("Modality", "Difference", "t", "DF", "PrGTt", "CILower", "CIUpper")
  }else if (I > 1 && J > 1){
    stop("The number of treatments and readers are both greater than 1. Perform MRMC analysis using StSignificanceTesting.")
  }else{
    stop("One of the number of readers and treatments must be 1 and the other one should be greater thant 1.")
  }
  return(list(
    f = fDbmFixed,
    ddf = ddf,
    pValue = pValue,
    fomStats = fomStats,
    diffFomStats = ret
  ))
}
