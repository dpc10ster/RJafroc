#' @title Binormal model AUC function
#' 
#' @description {Returns the Binormal model ROC-AUC corresponding to 
#'    specified parameters. See also \code{\link{UtilAucsRSM}}, \code{\link{UtilAucPROPROC}} 
#'    and \code{\link{UtilAucCBM}}}
#' 
#' @param a The \code{a} parameter of the binormal model (separation of 
#'    non-diseased and diseased pdfs)
#' @param b The \code{b} parameter of the binormal model (std. dev. of 
#'    non-diseased diseased pdf; diseased pdf has unit std. dev)
#' 
#' 
#' @return Binormal model-predicted ROC-AUC
#' 
#' @examples
#' a <- 2;b <- 0.7
#' UtilAucBinormal(a,b)
#' 
#' @references 
#' Dorfman DD, Alf E (1969) Maximum-Likelihood Estimation of Parameters of Signal-Detection Theory and 
#' Determination of Confidence Intervals - Rating-Method Data, Journal of Mathematical Psychology. 6:487-496.
#' 
#'  
#' @importFrom stats pnorm
#' 
#' @export
#' 
UtilAucBinormal <- function (a, b){
  auc <- pnorm(a/sqrt(1+b^2))
  return (auc)
}
