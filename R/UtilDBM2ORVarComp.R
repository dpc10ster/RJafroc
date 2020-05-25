#' Convert from DBM to OR variance components
#' 
#' @description \code{UtilDBM2ORVarComp} converts from DBM variance components to OR 
#'    variance components
#'    
#' @param K Total number of cases
#' @param DBMVarComp DBM variance components, a data.frame 
#'    containing VarR, VarC, VarTR, VarTC, VarRC and VarErr 
#' 
#' @return \code{UtilDBM2ORVarComp} returns the equivalent OR Variance components
#' 
#' 
#' @examples 
#' DBMVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH")$ANOVA$VarCom
#' UtilDBM2ORVarComp(114, DBMVarCom)
#' 
#' ORVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH")$ANOVA$VarCom
#' UtilOR2DBMVarComp(114, ORVarCom)
#' 
#' 
#' @export  


UtilDBM2ORVarComp <- function(K, DBMVarComp){
  
  # compared to Hillis mm paper, Table III, lower half
  VarR <- DBMVarComp["VarR",1] # OK
  VarTR <- DBMVarComp["VarTR",1] # OK
  Var <- (DBMVarComp["VarC",1] + DBMVarComp["VarTC",1] + DBMVarComp["VarRC",1] + DBMVarComp["VarErr",1])/K # OK
  Cov1 <- (DBMVarComp["VarC",1] + DBMVarComp["VarRC",1])/K # OK
  Cov2 <- (DBMVarComp["VarC",1] + DBMVarComp["VarTC",1])/K # OK
  Cov3 <- (DBMVarComp["VarC",1])/K # OK
  
  VarCom <- data.frame(VarCom = c(VarR, VarTR, Cov1, Cov2, Cov3, Var), 
                       Rhos = c(NA, NA, Cov1/Var, Cov2/Var, Cov3/Var, NA),
                       row.names = c("VarR", "VarTR", "Cov1", "Cov2", "Cov3", "Var"),
                       stringsAsFactors = FALSE)
  
  return(VarCom)
  
}

