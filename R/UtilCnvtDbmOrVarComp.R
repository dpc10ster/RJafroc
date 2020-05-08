#' Convert between DBM and OR variance components
#' 
#' \code{UtilDBM2ORVarComp} converts from DBM variance components to OR 
#'    variance components, while \code{UtilOR2DBMVarComp} converts from
#'    OR to DBM variance components.
#'    
#' @param \code{K} Total number of cases
#'     
#' @param \code{DBMVarComp} DBM variance components, a list or data.frame 
#'    containing varR, varC, varTR, varTC, varRC and varError 
#' 
#' @param \code{ORVarComp} OR variance components, a list or data.frame 
#'    containing varR, varTR, Cov1, Cov2, Cov3 and Var
#'    
#' @return \code{UtilDBM2ORVarComp} returns the equivalent OR variance components
#' 
#' @return \code{UtilOR2DBMVarComp} returns the equivalent DBM variance components
#' 
#' @examples 
#' DBMVarComp <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH")$DBMVarComp
#' UtilDBM2ORVarComp(114, DBMVarComp)
#' 
#' ORVarComp <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH")$ORVarComp
#' UtilOR2DBMVarComp(114, ORVarComp)
#' 
#' 
#' @export  

UtilDBM2ORVarComp <- function(K, DBMVarComp){
  
  # compared to Hillis mm paper, Table III, lower half
  varR <- DBMVarComp$varR # OK
  varTR <- DBMVarComp$varTR # OK
  Var <- (DBMVarComp$varC + DBMVarComp$varTC + DBMVarComp$varRC + DBMVarComp$varErr)/K # OK
  Cov1 <- (DBMVarComp$varC + DBMVarComp$varRC)/K # OK
  Cov2 <- (DBMVarComp$varC + DBMVarComp$varTC)/K # OK
  Cov3 <- (DBMVarComp$varC)/K # OK
  
  return(data.frame(varR = varR, 
                    varTR = varTR, 
                    Cov1 = Cov1, 
                    Cov2 = Cov2, 
                    Cov3 = Cov3, 
                    Var = Var,
                    "stringsAsFactors" = FALSE)
  )
  
}


#' 
#' @export  
#' 
UtilOR2DBMVarComp <- function(K, ORVarComp){
  
  # compared to Hillis mm paper, Table III, upper half
  varR <- ORVarComp$varR # OK
  varTR <- ORVarComp$varTR # OK
  varC <- (ORVarComp$cov3)*K # OK
  varTC <- (ORVarComp$cov2 - ORVarComp$cov3)*K # OK
  varRC <- (ORVarComp$cov1 - ORVarComp$cov3)*K # OK
  varErr <- (ORVarComp$var - ORVarComp$cov1 - ORVarComp$cov2 + ORVarComp$cov3)*K # OK
  
  return(data.frame(varR = varR, 
                    varC = varC, 
                    varTR = varTR, 
                    varTC = varTC, 
                    varRC = varRC, 
                    varErr = varErr,
                    "stringsAsFactors" = FALSE)
  )
  
}
