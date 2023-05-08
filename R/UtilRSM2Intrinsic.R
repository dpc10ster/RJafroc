#' Convert from physical to intrinsic RSM parameters 
#' 
#' Convert \strong{physical} RSM parameters \eqn{\lambda_i}' and \eqn{\nu_i}' to the 
#'    \strong{intrinsic} RSM parameters \eqn{\lambda_i} and \eqn{\nu_i}. The physical 
#'    parameters are more meaningful but they depend on \eqn{\mu}. The intrinsic 
#'    parameters are independent of \eqn{\mu}. See book for details.
#' 
#' @param mu The mean of the Gaussian distribution for the ratings of latent LLs, 
#'    i.e. continuous ratings of lesions that were found by the search mechanism 
#'    ~ N(\eqn{\mu},1). The corresponding distribution for the ratings of 
#'    latent NLs is N(0,1)
#'    
#' @param lambda The Poisson \eqn{\lambda_i} parameter, which describes the 
#'    distribution of random numbers of latent NLs (suspicious regions that do
#'    not correspond to actual lesions) per case; the mean of these random 
#'    numbers asymptotically approaches lambda
#' 
#' @param nu The \eqn{\nu_i} parameter; it is the success probability
#'    of the binomial distribution describing the random number of latent LLs 
#'    (suspicious regions that correspond to actual lesions) per diseased case
#'
#' @return A list containing \eqn{\lambda_i} and \eqn{\nu_i}, the RSM search parameters
#' 
#' @details RSM is the Radiological Search Model described in the book. A latent mark 
#'    becomes an actual mark if the corresponding rating exceeds the lowest reporting 
#'    threshold zeta1. See also \code{\link{Util2Physical}}.
#' 
#' @references
#' Chakraborty DP (2006) A search model and figure of merit for observer data acquired according to the free-response 
#' paradigm, Phys Med Biol 51, 3449-3462.
#' 
#' Chakraborty DP (2006) ROC Curves predicted by a model of visual search, Phys Med Biol 51, 3463--3482.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @examples
#' mu <- 2;lambda <- 10;nu <- 0.9
#' lambda_i <- Util2Intrinsic(mu, lambda, nu)$lambda_i 
#' nu_i <- Util2Intrinsic(mu, lambda, nu)$nu_i 
#' ## note that the physical values are only constrained to be positive, e.g., nu_i is not constrained
#' ## to be between 0 and one.
#' 
#' @export
Util2Intrinsic<- function(mu, lambda, nu) {
  
  lambda_i <- lambda * mu
  nu_i <- -log(1-nu)/mu
  
  return (list(
    lambda_i = lambda_i,
    nu_i = nu_i))
}


