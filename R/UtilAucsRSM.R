#' RSM ROC/AFROC AUC calculator
#' 
#' @description {Returns the ROC and AFROC AUCs corresponding to 
#'    specified RSM parameters. See also \code{\link{UtilAucPROPROC}}, 
#'    \code{\link{UtilAucBinormal}} and \code{\link{UtilAucCBM}}}
#' 
#' @param mu The mean(s) of the Gaussian distribution(s) for the 
#'    ratings of latent LLs (continuous ratings of lesions that 
#'    are found by the search mechanism)
#' 
#' @param lambdaP The Poisson distribution parameter(s), which describes 
#'    the random number of latent NLs (suspicious regions that do not 
#'    correspond to actual lesions) per case; these are the 
#'    \emph{physical} parameters.
#' 
#' @param nuP The \emph{physical} \code{nuP} parameters, each of which is 
#'    the success probability of the binomial distribution(s) describing 
#'    the random number of latent LLs (suspicious regions that correspond 
#'    to actual lesions) per diseased case.
#' 
#' @param lesDistr See \code{\link{PlotRsmOperatingCharacteristics}}.
#' 
#' @details The RSM parameters (\code{mu}, \code{lambdaP} and 
#'    \code{nuP}) can be vectors, provided they are of the same length; 
#'    the first parameter of each array is used, followed by the second, 
#'    etc; a common lesion distribution is assumed.
#' 
#' @return A list containing the ROC and AFROC AUCs corresponding to the 
#'    specified parameters
#' 
#' @examples
#' mu <- 1;lambdaP <- 1;nuP <- 1
#' lesDistr <- rbind(c(1, 0.9), c(2, 0.1)) 
#' ## i.e., 90% of dis. cases have one lesion, and 10% have two lesions
#' UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucROC
#' UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucAFROC
#'
#' mu <- c(1,2);lambdaP <- c(1,0.5);nuP <- c(1, 0.8)
#' lesDistr <- rbind(c(1, 0.9), c(2, 0.1))
#' ## i.e., 90% of dis. cases have one lesion, and 10% have two lesions
#' UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucROC
#' UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucAFROC
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
UtilAucsRSM <- function (mu, lambdaP, nuP, lesDistr){
  if (!all(c(length(mu) == length(lambdaP), length(mu) == length(nuP))))
    stop("Parameters have different lengths.")
  
  if (missing(lesDistr)){
    lesDistr <- c(1, 1)
    dim(lesDistr) <- c(1, 2)
  }
  
  aucROC <- rep(NA, length(mu))
  aucAFROC <- aucROC
  for (i in 1:length(mu)){
    if (lambdaP[i] < 0) stop("lambdaP has illegal value")
    if (nuP[i] < 0 || nuP[i] > 1) stop("nuP has illegal value")
    
    maxFPF <- xROC(-20, lambdaP[i])
    maxTPF <- yROC(-20, mu[i], lambdaP[i], nuP[i], lesDistr)
    AUC <- integrate(intROC, 0, maxFPF, mu = mu[i], lambdaP = lambdaP[i], nuP = nuP[i], lesDistr = lesDistr)$value
    aucROC[i] <- AUC + (1 + maxTPF) * (1 - maxFPF) / 2
    
    maxLLF <- yFROC(-20, mu[i], nuP[i])
    AUC <- integrate(intAFROC, 0, maxFPF, mu = mu[i], lambdaP = lambdaP[i], nuP = nuP[i])$value
    aucAFROC[i] <- AUC + (1 + maxLLF) * (1 - maxFPF) / 2
  }
  return(list(
    aucROC = aucROC,
    aucAFROC = aucAFROC
  ))
}

