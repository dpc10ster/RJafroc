#' Convert from intrinsic to physical RSM parameters 
#' 
#' Convert \strong{intrinsic} RSM parameters \eqn{lambda} and \eqn{nu} 
#'    correspond to the \strong{physical} RSM parameters \eqn{lambda'} 
#'    and \eqn{nu'}. The physical parameters are more meaningful but they 
#'    depend on \eqn{mu}. The intrinsic parameters are independent of 
#'    \eqn{mu}. See book for details.
#'    
#' 
#' @param mu The mean of the Gaussian distribution for the ratings of latent 
#'    LLs, i.e. continuous ratings of lesions that were found by the search 
#'    mechanism ~ N(\eqn{\mu},1). The corresponding distribution for the 
#'    ratings of latent NLs is N(0,1).
#' 
#' @param lambda The Poisson \emph{intrinsic} parameter, related to \eqn{\lambda}', 
#'    the latter is the mean of the Poisson distribution of numbers of latent NLs 
#'    (suspicious regions that do not correspond to actual lesions) per case.
#' 
#' @param nu The \emph{intrinsic} \eqn{\nu} parameter; the corresponding 
#'    \emph{physical} parameter is the success probability of the binomial 
#'    distribution of random numbers of latent LLs (suspicious regions that 
#'    correspond to actual lesions) per diseased case, i.e., the chance that 
#'    a lesion is "found".
#'
#' @return A list containing \eqn{\lambda}' and \eqn{\nu}'
#' 
#' @details RSM is the Radiological Search Model described in the book. 
#'   A latent mark becomes an actual mark if the corresponding rating 
#'   exceeds the lowest reporting threshold \eqn{\zeta}1. See also
#'   \code{\link{UtilPhysical2IntrinsicRSM}}.
#' 
#' @references
#' Chakraborty DP (2006) A search model and figure of merit for observer data acquired according to the free-response 
#' paradigm, Phys Med Biol 51, 3449--3462.
#' 
#' Chakraborty DP (2006) ROC Curves predicted by a model of visual search, Phys Med Biol 51, 3463--3482.
#'
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @examples
#' mu <- 2;lambda <- 20;nu <- 1.1512925 
#' lambdaP <- UtilIntrinsic2PhysicalRSM(mu, lambda, nu)$lambdaP 
#' nuP <- UtilIntrinsic2PhysicalRSM(mu, lambda, nu)$nuP 
#' ## note that the physical values are only constrained to be positive, but the physical variable nuP
#' ## must obey 0 <= nuP <= 1
#' 
#' 
#' @export
UtilIntrinsic2PhysicalRSM <- function(mu, lambda, nu) {

lambdaP <- lambda / mu
nuP <- 1 - exp(-nu * mu)

return (list(
  lambdaP = lambdaP,
  nuP = nuP))
}


