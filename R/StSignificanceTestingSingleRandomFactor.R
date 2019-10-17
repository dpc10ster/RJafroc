#' Perform significance testing for single fixed factor analysis
#' 
#' @description  Significance testing for datasets with multiple readers in 
#'    a single treatment.
#' 
#' @param dataset A single-treatment multipe reader dataset.
#' @param theta0 The comparison value that the average FOM is compared to.
#' @param FOM The figure of merit, see \code{\link{UtilFigureOfMerit}}.
#' @param FPFValue Only needed for \code{LROC} data; where to evaluate a partial 
#'    curve based figure of merit.
#' @param alpha The significance level (\code{alpha}, default 0.05) 
#'    of the test of the null hypothesis that FOMs of all levels of 
#'    the fixed factor are identical.
#' 
#' @return The return value is a list containing:
#' @return \item{f}{The observed F-statistic for testing the null 
#'    hypothesis of no treatment effect.}
#' @return \item{ddf}{The denominator degrees of freedom of the F statistic. 
#'    The numerator degrees of freedom is always the number of levels of the 
#'    fixed factor minus one.}
#' @return \item{pValue}{The p-value for rejecting the NH.}
#' @return \item{fomStats}{Statistics for FOM for each level of the fixed factor.}
#' 
#' @details This function performs implements Hillis et al. 2005, Eqn. 23. 
#'    Following an overall F-test, reader-pairings are compared using paired 
#'    t-tests.
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

StSignificanceTestingSingleRandomFactor <- function(dataset, theta0, FOM, 
                                                    FPFValue = 0.2, alpha = 0.05) {
  
  if (dataset$dataType == "LROC") stop("Dataset must NOT be LROC")
  
  ret <- gpfEstimateVarCov(dataset, FOM, covEstMethod = "Jackknife")
  varError <- ret$var;  Cov2 <- ret$cov2
  
  NL <- drop(dataset$NL)
  J <- length(NL[,1]) # number of radiologists including CAD reader
  thetajc <- UtilFigureOfMerit(dataset, FOM)
  
  MSR <- 0
  avgFom <- mean(thetajc)
  for (j in 1:J){
    MSR <- MSR + (thetajc[j] - avgFom)^2
  }
  MSR <- MSR / (J - 1)
  
  MSdenOR_single <- MSR + max(J * Cov2, 0)
  DdfhSingle <- MSdenOR_single^2 / (MSR^2 / (J - 1))
  TstatStar <- abs(avgFom - theta0) / sqrt(MSdenOR_single/J)
  pval <- 1 - pt(abs(TstatStar),DdfhSingle) + pt(-abs(TstatStar),DdfhSingle)
  
  CIAvgFom <- array(dim=2)
  CIAvgFom[1] <- qt(alpha/2,df = DdfhSingle)
  CIAvgFom[2] <- qt(1-alpha/2,df = DdfhSingle)
  CIAvgFom <- CIAvgFom * sqrt(MSdenOR_single/J)
  CIAvgFom <- CIAvgFom + avgFom
  return (list (
    fom = thetajc,
    avgFom = avgFom,
    CIAvgFom = CIAvgFom,
    varR = var(as.numeric(thetajc)),
    varError = varError, 
    cov2 = Cov2, 
    Tstat = TstatStar,
    df = DdfhSingle,
    pval = pval
  ))
  return (ret)
  
}





