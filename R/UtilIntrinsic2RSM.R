#' Convert from intrinsic to physical RSM parameters 
#' 
#' Convert \strong{intrinsic} RSM parameters \eqn{lambda_i} and \eqn{nu_i} 
#'    correspond to the \strong{physical} RSM parameters \eqn{lambda_i'} 
#'    and \eqn{nu_i'}. The physical parameters are more meaningful but they 
#'    depend on \eqn{mu}. The intrinsic parameters are independent of 
#'    \eqn{mu}. See book for details.
#'    
#' 
#' @param mu The mean of the Gaussian distribution for the ratings of latent 
#'    LLs, i.e. continuous ratings of lesions that were found by the search 
#'    mechanism ~ N(\eqn{\mu},1). The corresponding distribution for the 
#'    ratings of latent NLs is N(0,1).
#' 
#' @param lambda_i The \emph{intrinsic} Poisson lambda_i parameter.
#' 
#' @param nu_i The \emph{intrinsic} Binomial nu_i parameter.
#'
#' @return A list containing \eqn{\lambda} and \eqn{\nu}
#' 
#' @details RSM is the Radiological Search Model described in the book. 
#'   See also \code{\link{UtilRSM2Intrinsic}}.
#' 
#' @references
#' Chakraborty DP (2006) A search model and figure of merit for observer data acquired according to the free-response 
#' paradigm, Phys Med Biol 51, 3449--3462.
#' 
#' Chakraborty DP (2006) ROC Curves predicted by a model of visual search, Phys Med Biol 51, 3463--3482.
#'
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @examples
#' mu <- 2;lambda_i <- 20;nu_i <- 1.1512925 
#' lambda <- UtilIntrinsic2RSM(mu, lambda_i, nu_i)$lambda 
#' nu <- UtilIntrinsic2RSM(mu, lambda_i, nu_i)$nu 
#' ## note that the physical values are only constrained to be positive, but the physical variable nu
#' ## must obey 0 <= nu <= 1
#' 
#' 
#' @export
UtilIntrinsic2RSM <- function(mu, lambda_i, nu_i) {

lambda <- lambda_i / mu
nu <- 1 - exp(-nu_i * mu)

return (list(
  lambda = lambda,
  nu = nu))
}


