#' Convert from OR to DBM variance components
#' 
#' @description \code{UtilOR2DBMVarComp} converts from OR to DBM variance components.
#'    
#' @param K Total number of cases
#'     
#' @param ORVarComp OR variance components, a data.frame 
#'    containing VarR, VarTR, Cov1, Cov2, Cov3 and Var
#'    
#' @return \code{UtilOR2DBMVarComp} returns the equivalent DBM variance components
#' 
#' @examples 
#' DBMVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH")$ANOVA$VarCom
#' UtilDBM2ORVarComp(114, DBMVarCom)
#' 
#' ORVarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH")$ANOVA$VarCom
#' UtilOR2DBMVarComp(114, ORVarCom)
#' 
#' 
#' 
#' @export  
#' 
UtilOR2DBMVarComp <- function(K, ORVarComp){
  
  # compared to Hillis mm paper, Table III, upper half
  VarR <- ORVarComp["VarR",1] # OK
  VarC <- (ORVarComp["Cov3",1])*K # OK
  VarTR <- ORVarComp["VarTR",1] # OK
  VarTC <- (ORVarComp["Cov2",1] - ORVarComp["Cov3",1])*K # OK
  VarRC <- (ORVarComp["Cov1",1] - ORVarComp["Cov3",1])*K # OK
  VarErr <- (ORVarComp["Var",1] - ORVarComp["Cov1",1] - ORVarComp["Cov2",1] + ORVarComp["Cov3",1])*K # OK

  return(data.frame(Estimates = c(VarR, VarC, VarTR, VarTC, VarRC, VarErr),
                    row.names = c("VarR", "VarC", "VarTR", "VarTC", "VarRC", "VarErr"),
                    "stringsAsFactors" = FALSE)
  )
  
}
