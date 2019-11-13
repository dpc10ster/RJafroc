#' Significance testing for single random factor
#' 
#' @description  Significance testing for datasets with multiple readers in 
#'    a single treatment, compare average FOM to specified NH value.
#' 
#' @param dataset A single-treatment multipe-reader dataset.
#' @param fomNh The comparison value that the reader average FOM is compared to.
#' @param FOM The figure of merit, see \code{\link{UtilFigureOfMerit}}.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' @param alpha The significance level (\code{alpha}, default 0.05) 
#'    of the test of the null hypothesis that the reader averaged FOMs and the specified
#'    NH value fomNh are identical.
#' 
#' 
#' @return The return value is a list containing:
#' \itemize{
#' \item{\code{fom}}   {Observed reader FOMs.}
#' \item{\code{avgFom}}   {Average reader FOM.} 
#' \item{\code{CIAvgFom}}   {Confidence interval of the reader averaged FOM.}
#' \item{\code{vaR}}   {Reader variance term of the Obuchowski-Rockette model.}
#' \item{\code{cov2}}   {cov2 of the Obuchowski-Rockette model.}
#' \item{\code{var}}   {Error term of the Obuchowski-Rockette model.}
#' \item{\code{Tstat}}   {The observed value of the t-statistic.}
#' \item{\code{df}}   {The degrees of freedom associated with the t-statistic.}
#' \item{\code{pValue}}   {The p-value for rejecting the NH.}
#' }
#' 
#' @details This function performs implements Hillis et al. 2005, Eqn. 23. 
#' 
#' 
#' @examples 
#' ## Create a single treatment ROC dataset with four readers
#' singleFactorData <- DfExtractDataset(dataset02, trts = 1, rdrs = seq(1,4))
#' 
#' ## Perform single-treatment random-reader analysis
#' StSingleTreatmentRandomReader(singleFactorData, fomNh = 0.8, FOM = "Wilcoxon")
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

StSingleTreatmentRandomReader <- function(dataset, fomNh, FOM, FPFValue = 0.2, alpha = 0.05) {
  
  ret <- gpfEstimateVarCov(dataset, FOM, FPFValue, covEstMethod = "Jackknife")
  var <- ret$var;  Cov2 <- ret$cov2
  
  NL <- drop(dataset$NL)
  J <- length(NL[,1]) # number of radiologists
  thetajc <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  MSR <- 0
  avgFom <- mean(thetajc)
  for (j in 1:J){
    MSR <- MSR + (thetajc[j] - avgFom)^2
  }
  MSR <- MSR / (J - 1)
  
  MSdenOR_single <- MSR + max(J * Cov2, 0)
  DdfhSingle <- MSdenOR_single^2 / (MSR^2 / (J - 1))
  TstatStar <- abs(avgFom - fomNh) / sqrt(MSdenOR_single/J)
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
    cov2 = Cov2, 
    var = var, 
    Tstat = TstatStar,
    df = DdfhSingle,
    pval = pval
  ))
  return (ret)
  
}





