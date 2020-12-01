#' RSM ROC/AFROC/wAFROC AUC calculator
#' 
#' @description {Returns the ROC, AFROC and wAFROC AUCs corresponding to 
#'    specified RSM parameters. See also \code{\link{UtilAucPROPROC}}, 
#'    \code{\link{UtilAucBinormal}} and \code{\link{UtilAucCBM}}}
#' 
#' @param mu The mean of the Gaussian distribution for the 
#'    ratings of latent LLs (continuous ratings of lesions that 
#'    are found by the search mechanism).
#' 
#' @param lambda The \emph{intrinsic} Poisson distribution parameter, 
#'    which describes 
#'    the random number of latent NLs (suspicious regions that do not 
#'    correspond to actual lesions) per case.
#' 
#' @param nu The \emph{intrinsic} \code{nu} parameters, 
#'    the success probability of the binomial distribution describing 
#'    the random numbers of latent LLs (suspicious regions that correspond 
#'    to actual lesions) per diseased case.
#' 
#' @param zeta1 The lowest reporting threshold, the default is \code{-Inf}.
#' 
#' @param relWeights The relative weights of the lesions; a vector of 
#'    length \code{maxLL}; if zero, the default, equal weights are assumed.
#'
#'
#' @param lesDistr See \code{\link{PlotRsmOperatingCharacteristics}}.
#'  
#' 
#' @return A list containing the ROC, AFROC and wAFROC AUCs corresponding to the 
#'    specified parameters
#' 
#' 
#' @examples
#' mu <- 1;lambda <- 1;nu <- 1
#' lesDistr <- rbind(c(1, 0.9), c(2, 0.1)) 
#' ## i.e., 90% of dis. cases have one lesion, and 10% have two lesions
#' 
#' UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = -Inf, lesDistr)
#' 
#' relWeights <- c(0.05, 0.95)
#' UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = 0, lesDistr, relWeights)
#' 
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' Chakraborty DP (2006) A search model and figure of merit for observer data acquired according to the free-response 
#' paradigm, Phys Med Biol 51, 3449-3462.
#' 
#' Chakraborty DP (2006) ROC Curves predicted by a model of visual search, Phys Med Biol 51, 3463--3482.
#'
#' 
#' 
#' @export
#' 
UtilAnalyticalAucsRSM <- function (mu, lambda, nu, zeta1 = -Inf, lesDistr, relWeights = 0){
  
  ret <- UtilIntrinsic2PhysicalRSM(mu, lambda, nu)
  lambdaP <- ret$lambdaP
  nuP <- ret$nuP
  
  if (lambdaP < 0) stop("lambdaP has illegal value")
  if (nuP < 0 || nuP > 1) stop("nuP has illegal value")
  
  if (missing(lesDistr)){
    lesDistr <- c(1, 1) # two values
    dim(lesDistr) <- c(1, 2) # convert to 1 row and 2 columns array
  }
  
  specified1DLesDistr <- lesDistr[,2]
  lesWghtDistr <- UtilSpecifyLesionWeightsDistr(specified1DLesDistr, relWeights)
  
  aucwAFROC <- aucAFROC <- aucROC <- rep(NA, length(mu))
  
  maxFPF <- xROC(zeta1, lambdaP)
  maxTPF <- yROC(zeta1, mu, lambdaP, nuP, lesDistr)
  x <- integrate(y_ROC_FPF, 0, maxFPF, mu = mu, lambdaP = lambdaP, nuP = nuP, lesDistr = lesDistr)$value
  aucROC <- x + (1 + maxTPF) * (1 - maxFPF) / 2
  
  maxLLF <- yFROC(zeta1, mu, nuP)
  x <- integrate(y_AFROC_FPF, 0, maxFPF, mu = mu, lambdaP = lambdaP, nuP = nuP)$value
  aucAFROC <- x + (1 + maxLLF) * (1 - maxFPF) / 2
  
  maxwLLF <- ywAFROC(zeta1, mu, nuP, lesDistr, lesWghtDistr)
  x <- integrate(y_wAFROC_FPF, 0, maxFPF, mu = mu, lambdaP = lambdaP, nuP = nuP, lesDistr, lesWghtDistr)$value
  aucwAFROC <- x + (1 + maxwLLF) * (1 - maxFPF) / 2
  
  
  return(list(
    aucROC = aucROC,
    aucAFROC = aucAFROC,
    aucwAFROC = aucwAFROC
  ))
}

