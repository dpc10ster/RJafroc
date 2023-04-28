#' RSM ROC/AFROC/wAFROC AUC calculator
#' 
#' @description {Returns the ROC, AFROC and wAFROC AUCs corresponding to 
#'    specified RSM parameters. See also \code{\link{UtilAucPROPROC}}, 
#'    \code{\link{UtilAucBinormal}} and \code{\link{UtilAucCBM}}}
#' 
#' @param mu The mean of the Gaussian distribution for the 
#'    ratings of latent LLs (continuous ratings of lesions that 
#'    are found by the search mechanism). The NLs are assumed to be distributed
#'    as N(0,1).
#' 
#' @param lambda The RSM lambda parameter.
#' 
#' @param nu The RSM nu parameters.
#' 
#' @param zeta1 The lowest reporting threshold, the default is \code{-Inf}.
#' 
#' @param relWeights The relative weights of the lesions; a vector of 
#'    length \code{maxLL}; if zero, the default, equal weights are assumed.
#'
#'
#' @param lesDistr The lesion distribution 1D array, i.e., the probability
#'    mass function (pmf) of the numbers of lesions for diseased cases.
#'  
#' 
#' @return The ROC, AFROC and wAFROC AUCs corresponding to the 
#'    specified parameters
#' 
#' 
#' @examples
#' mu <- 1;lambda <- 1;nu <- 0.9
#' lesDistr <- c(0.9, 0.1) 
#' ## i.e., 90% of dis. cases have one lesion, and 10% have two lesions
#' relWeights <- c(0.05, 0.95)
#' ## i.e., lesion 1 has weight 5 percent while lesion two has weight 95 percent
#' 
#' UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = -Inf, lesDistr)
#' UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 = -Inf, lesDistr, relWeights)
#' 
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
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
  
  maxLL <- length(lesDistr)
  lesWghtDistr <- UtilLesWghtsLD(UtilLesDistr(lesDistr), relWeights)
  
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  if (missing(lesDistr)){
    lesDistr <- 1
  } 
  
  aucwAFROC <- aucAFROC <- aucROC <- rep(NA, length(mu))
  
  maxFPF <- xROC(zeta1, lambda)
  maxTPF <- yROC(zeta1, mu, lambda, nu, lesDistr)
  x <- integrate(y_ROC_FPF, 0, maxFPF, mu = mu, lambda = lambda, nu = nu, lesDistr = lesDistr)$value
  aucROC <- x + (1 + maxTPF) * (1 - maxFPF) / 2
  
  maxLLF <- RSM_LLF(zeta1, mu, nu)
  x <- integrate(y_AFROC_FPF, 0, maxFPF, mu = mu, lambda = lambda, nu = nu)$value
  aucAFROC <- x + (1 + maxLLF) * (1 - maxFPF) / 2
  
  
  # following is original Cpp implementation
  maxwLLF <- ywAFROC(zeta1, mu, nu, lesDistr, lesWghtDistr)
  x <- integrate(y_wAFROC_FPF, 0, maxFPF, mu = mu, lambda = lambda, nu = nu, lesDistr, relWeights)$value
  aucwAFROC <- x + (1 + maxwLLF) * (1 - maxFPF) / 2
  
  
  return(list(
    aucROC = aucROC,
    aucAFROC = aucAFROC,
    aucwAFROC = aucwAFROC
  ))
}

