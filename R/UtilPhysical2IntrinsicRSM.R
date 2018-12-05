#' Convert from physical to intrinsic RSM parameters 
#' 
#' Convert \strong{physical} RSM parameters \eqn{\lambda}' and \eqn{\nu}' to the 
#'    \strong{intrinsic} RSM parameters \eqn{\lambda} and \eqn{\nu}. The physical 
#'    parameters are more meaningful but they depend on \eqn{\mu}. The intrinsic 
#'    parameters are independent of \eqn{\mu}. See book for details.
#' 
#' @param mu The mean of the Gaussian distribution for the ratings of latent LLs, 
#'    i.e. continuous ratings of lesions that were found by the search mechanism 
#'    ~ N(\eqn{\mu},1). The corresponding distribution for the ratings of 
#'    latent NLs is N(0,1)
#'    
#' @param lambdaP The Poisson \emph{physical} parameter, which describes the 
#'    distribution of random numbers of latent NLs (suspicious regions that do
#'    not correspond to actual lesions) per case; the mean of these random 
#'    numbers asymptotically approaches lambdaP
#' 
#' @param nuP The \emph{physical} \eqn{\nu} parameter; it is the success probability
#'    of the binomial distribution describing the random number of latent LLs 
#'    (suspicious regions that correspond to actual lesions) per diseased case
#'
#' @return A list containing \eqn{\lambda} and \eqn{\nu}, the physical parameters
#' 
#' @details RSM is the Radiological Search Model described in the book. A latent mark 
#'    becomes an actual mark if the corresponding rating exceeds the lowest reporting 
#'    threshold zeta1. See also \code{\link{UtilIntrinsic2PhysicalRSM}}.
#' 
#' @references
#' Chakraborty DP (2006) A search model and figure of merit for observer data acquired according to the free-response 
#' paradigm, Phys Med Biol 51, 3449-3462.
#' 
#' Chakraborty DP (2006) ROC Curves predicted by a model of visual search, Phys Med Biol 51, 3463--3482.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @examples
#' mu <- 2;lambdaP <- 10;nuP <- 0.9
#' lambda <- UtilPhysical2IntrinsicRSM(mu, lambdaP, nuP)$lambda 
#' nu <- UtilPhysical2IntrinsicRSM(mu, lambdaP, nuP)$nu 
#' ## note that the physical values are only constrained to be positive, e.g., nu is not constrained
#' ## to be between 0 and one.
#' 
#' @export
UtilPhysical2IntrinsicRSM <- function(mu, lambdaP, nuP) {
  
  lambda <- lambdaP * mu
  nu <- -log(1-nuP)/mu
  
  return (list(
    lambda = lambda,
    nu = nu))
}


