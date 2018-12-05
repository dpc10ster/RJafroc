#' @title PROPROC AUC function
#' 
#' @description {Returns the PROPROC ROC-AUC corresponding to specified 
#'    parameters. See also \code{\link{UtilAucsRSM}}, \code{\link{UtilAucBinormal}} 
#'    and \code{\link{UtilAucCBM}}}
#' 
#' @param c1 The c-parameter of the PROPROC model, since \strong{c is a reserved 
#'    function in R}. 
#' @param da The da-parameter of the PROPROC model.
#' 
#' @return PROPROC model-predicted ROC-AUC for the specified parameters
#' 
#' @examples
#' c1 <- .2;da <- 1.5
#' UtilAucPROPROC(c1,da)
#' 
#' 
#' @references 
#' Metz CE, Pan X (1999) Proper Binormal ROC Curves: Theory and Maximum-Likelihood Estimation, J Math Psychol 43(1):1-33.
#'  
#' @importFrom mvtnorm pmvnorm
#' @importFrom stats pnorm
#' 
#' @export
#' 
#' 
UtilAucPROPROC <- function (c1, da){
  # Metz and Pan Journal of Mathematical Psychology 43, 1?33 (1999)
  rho2 <- -(1-c1^2)/(1+c1^2)
  corr <- diag(2)
  corr[lower.tri(corr)] <- rho2
  corr[upper.tri(corr)] <- rho2
  lower <- rep(-Inf,2)
  upper <- c(-da/sqrt(2),0)
  mean <- rep(0,2)
  aucProproc <- pnorm(da/sqrt(2))+2*pmvnorm(lower, upper, mean, corr)  #Eqn. 36 Metz and Pan
  return (as.numeric(aucProproc))
}

