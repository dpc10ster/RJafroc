#' @title CBM AUC function
#' 
#' @description {Returns the CBM ROC-AUC
#' See also \code{\link{UtilAucsRSM}}, \code{\link{UtilAucPROPROC}} and \code{\link{UtilAucBinormal}}}
#' 
#' @param mu The \code{mu} parameter of CBM (separation of non-diseased 
#'    and diseased pdfs)
#' @param alpha The \code{alpha} parameter of CBM, i.e., the fraction 
#'    of diseased cases on which the disease is visible
#' 
#' 
#' @return CBM-predicted ROC-AUC for the specified parameters
#' 
#' @examples
#' mu <- 2;alpha <- 0.8
#' UtilAucCBM(mu,alpha)
#' 
#' 
#' @references 
#' Dorfman DD, Berbaum KS (2000) A contaminated binormal model for ROC data: Part II. 
#' A formal model, Acad Radiol 7:6 427--437.
#' 
#'  
#' @importFrom stats pnorm
#' 
#' @export
#' 
UtilAucCBM <- function (mu, alpha){
  auc <- (1 - alpha) * 0.5 + alpha * pnorm(mu / sqrt(2))
  return (auc)
}
