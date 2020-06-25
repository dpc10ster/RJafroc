#' Convert from OR to DBM variance components
#' 
#' @description \code{UtilOR2DBMVarCom} converts from OR to DBM variance components.
#'    
#' @param K Total number of cases
#'     
#' @param ORVarCom OR variance components, a data.frame 
#'    containing VarR, VarTR, Cov1, Cov2, Cov3 and Var
#'    
#' @return \code{UtilOR2DBMVarCom} returns the equivalent DBM variance components
#' 
#' @examples 
#' DBMVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH")$ANOVA$VarCom
#' UtilDBM2ORVarCom(114, DBMVarCom)
#' 
#' ORVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH")$ANOVA$VarCom
#' UtilOR2DBMVarCom(114, ORVarCom)
#' 
#' 
#' 
#' @export  
#' 
UtilOR2DBMVarCom <- function(K, ORVarCom){
  
  # compared to Hillis mm paper, Table III, upper half
  VarR <- ORVarCom["VarR",1] # OK
  VarC <- (ORVarCom["Cov3",1])*K # OK
  VarTR <- ORVarCom["VarTR",1] # OK
  VarTC <- (ORVarCom["Cov2",1] - ORVarCom["Cov3",1])*K # OK
  VarRC <- (ORVarCom["Cov1",1] - ORVarCom["Cov3",1])*K # OK
  VarErr <- (ORVarCom["Var",1] - ORVarCom["Cov1",1] - ORVarCom["Cov2",1] + ORVarCom["Cov3",1])*K # OK

  return(data.frame(Estimates = c(VarR, VarC, VarTR, VarTC, VarRC, VarErr),
                    row.names = c("VarR", "VarC", "VarTR", "VarTC", "VarRC", "VarErr"),
                    "stringsAsFactors" = FALSE)
  )
  
}
