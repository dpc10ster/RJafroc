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
#' @param lambdaP The \emph{physical} lambda prime parameter, 
#'    which describes 
#'    the random number of latent NLs (suspicious regions that do not 
#'    correspond to actual lesions) per case.
#' 
#' @param nuP The \emph{physical} \code{nuP} parameters, 
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
#' @param lesDistr The lesion distribution 1D array, i.e., the probability
#'    mass function (pmf) of the numbers of lesions for diseased cases.
#'  
#' 
#' @param tempTest Testing the cpp code, default 0, reserved for developer.
#'  
#' 
#' @return A list containing the ROC, AFROC and wAFROC AUCs corresponding to the 
#'    specified parameters
#' 
#' 
#' @examples
#' mu <- 1;lambdaP <- 1;nuP <- 0.9
#' lesDistr <- c(0.9, 0.1) 
#' ## i.e., 90% of dis. cases have one lesion, and 10% have two lesions
#' relWeights <- c(0.05, 0.95)
#' ## i.e., lesion 1 has weight 5 percent while lesion two has weight 95 percent
#' 
#' UtilAnalyticalAucsRSM(mu, lambdaP, nuP, zeta1 = -Inf, lesDistr)
#' UtilAnalyticalAucsRSM(mu, lambdaP, nuP, zeta1 = -Inf, lesDistr, relWeights)
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
UtilAnalyticalAucsRSM <- function (mu, lambdaP, nuP, zeta1 = -Inf, lesDistr, relWeights = 0, tempTest = 0){
  
  maxLL <- length(lesDistr)
  lesWghtDistr <- UtilLesionWeightsMatrixLesDistr(lesDistr, relWeights)
  
  # bug fix 12/26/21
  if (lambdaP < 0) stop("Incorrect value for lambdaP\n")
  if (nuP < 0) stop("Incorrect value for nuP\n")
  if (nuP > 1) stop("Incorrect value for nuP\n")

  if (missing(lesDistr)){
    lesDistr <- 1
  } 
  
  aucwAFROC <- aucAFROC <- aucROC <- rep(NA, length(mu))
  
  maxFPF <- xROC(zeta1, lambdaP)
  maxTPF <- yROC(zeta1, mu, lambdaP, nuP, lesDistr)
  x <- integrate(y_ROC_FPF, 0, maxFPF, mu = mu, lambdaP = lambdaP, nuP = nuP, lesDistr = lesDistr)$value
  aucROC <- x + (1 + maxTPF) * (1 - maxFPF) / 2
  
  maxLLF <- RSM_yFROC(zeta1, mu, nuP)
  x <- integrate(y_AFROC_FPF, 0, maxFPF, mu = mu, lambdaP = lambdaP, nuP = nuP)$value
  aucAFROC <- x + (1 + maxLLF) * (1 - maxFPF) / 2

 
  if (tempTest == 1) {
    # dpc 01/05/22 these comments were added while converting code to formula for chapter 21-optim-op-point
    # needed formula for wAFROC ordinate, I know this is crazy, Einstein would never have done it this way :(
    # finished see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
    # checked from Console that following two give same results with following code
    # UtilAnalyticalAucsRSM(mu = 2, lambdaP = 1, nuP = 0.9, zeta1 = -3, 
    # lesDistr = c(0.1, 0.4, 0.4, 0.1), relWeights =  c(0.2, 0.3, 0.1, 0.5))
    # $aucROC
    # [1] 0.9698827
    # $aucAFROC
    # [1] 0.8566404
    # $aucwAFROC
    # [1] 0.8448053
    # following is R implementation
    # see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
    # see test_RSM-formulae.R
    # contextStr <- "testing weights code with max 4 lesions per case: Cpp vs R"
    # contextStr <- "testing weights code with max 4 lesions per case, random values: Cpp vs R"
    # contextStr <- "testing weights code with max 10 lesions per case, random values: Cpp vs R"
    maxwLLF <- ywAFROC_R(zeta1, mu, nuP, lesDistr, lesWghtDistr)
  } else {
    # following is original Cpp implementation
    maxwLLF <- ywAFROC(zeta1, mu, nuP, lesDistr, lesWghtDistr)
  }
  x <- integrate(y_wAFROC_FPF, 0, maxFPF, mu = mu, lambdaP = lambdaP, nuP = nuP, lesDistr, lesWghtDistr)$value
  aucwAFROC <- x + (1 + maxwLLF) * (1 - maxFPF) / 2
  
  
  return(list(
    aucROC = aucROC,
    aucAFROC = aucAFROC,
    aucwAFROC = aucwAFROC
  ))
}

