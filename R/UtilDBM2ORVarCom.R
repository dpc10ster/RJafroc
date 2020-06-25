#' Convert from DBM to OR variance components
#' 
#' @description \code{UtilDBM2ORVarCom} converts from DBM variance components to OR 
#'    variance components
#'    
#' @param K Total number of cases
#' @param DBMVarCom DBM variance components, a data.frame 
#'    containing VarR, VarC, VarTR, VarTC, VarRC and VarErr 
#' 
#' @return \code{UtilDBM2ORVarCom} returns the equivalent OR Variance components
#' 
#' 
#' @examples 
#' DBMVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH")$ANOVA$VarCom
#' UtilDBM2ORVarCom(114, DBMVarCom)
#' 
#' ORVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH")$ANOVA$VarCom
#' UtilOR2DBMVarCom(114, ORVarCom)
#' 
#' 
#' @export  


UtilDBM2ORVarCom <- function(K, DBMVarCom){
  
  # compared to Hillis mm paper, Table III, lower half
  VarR <- DBMVarCom["VarR",1] # OK
  VarTR <- DBMVarCom["VarTR",1] # OK
  Var <- (DBMVarCom["VarC",1] + DBMVarCom["VarTC",1] + DBMVarCom["VarRC",1] + DBMVarCom["VarErr",1])/K # OK
  Cov1 <- (DBMVarCom["VarC",1] + DBMVarCom["VarRC",1])/K # OK
  Cov2 <- (DBMVarCom["VarC",1] + DBMVarCom["VarTC",1])/K # OK
  Cov3 <- (DBMVarCom["VarC",1])/K # OK
  
  VarCom <- data.frame(VarCom = c(VarR, VarTR, Cov1, Cov2, Cov3, Var), 
                       Rhos = c(NA, NA, Cov1/Var, Cov2/Var, Cov3/Var, NA),
                       row.names = c("VarR", "VarTR", "Cov1", "Cov2", "Cov3", "Var"),
                       stringsAsFactors = FALSE)
  
  return(VarCom)
  
}

