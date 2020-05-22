#' Convert between DBM and OR variance components
#' 
#' \code{UtilDBM2ORVarComp} converts from DBM variance components to OR 
#'    variance components, while \code{UtilOR2DBMVarComp} converts from
#'    OR to DBM variance components.
#'    
#' @param \code{K} Total number of cases
#'     
#' @param \code{DBMVarComp} DBM variance components, a list or data.frame 
#'    containing varR, varC, varTR, varTC, varRC and varErr 
#' 
#' @param \code{ORVarComp} OR variance components, a list or data.frame 
#'    containing varR, varTR, Cov1, Cov2, Cov3 and Var
#'    
#' @return \code{UtilDBM2ORVarComp} returns the equivalent OR variance components
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
#' @export  

UtilDBM2ORVarComp <- function(K, DBMVarComp){
  
  # compared to Hillis mm paper, Table III, lower half
  varR <- DBMVarComp["varR",1] # OK
  varTR <- DBMVarComp["varTR",1] # OK
  Var <- (DBMVarComp["varC",1] + DBMVarComp["varTC",1] + DBMVarComp["varRC",1] + DBMVarComp["varErr",1])/K # OK
  Cov1 <- (DBMVarComp["varC",1] + DBMVarComp["varRC",1])/K # OK
  Cov2 <- (DBMVarComp["varC",1] + DBMVarComp["varTC",1])/K # OK
  Cov3 <- (DBMVarComp["varC",1])/K # OK
  
  VarCom <- data.frame(VarCom = c(varR, varTR, Cov1, Cov2, Cov3, Var), 
                       Rhos = c(NA, NA, Cov1/Var, Cov2/Var, Cov3/Var, NA),
                       row.names = c("varR", "varTR", "Cov1", "Cov2", "Cov3", "Var"),
                       stringsAsFactors = FALSE)
  
  return(VarCom)
  
}


#' 
#' @export  
#' 
UtilOR2DBMVarComp <- function(K, ORVarComp){
  
  # compared to Hillis mm paper, Table III, upper half
  varR <- ORVarComp["varR",1] # OK
  varC <- (ORVarComp["Cov3",1])*K # OK
  varTR <- ORVarComp["varTR",1] # OK
  varTC <- (ORVarComp["Cov2",1] - ORVarComp["Cov3",1])*K # OK
  varRC <- (ORVarComp["Cov1",1] - ORVarComp["Cov3",1])*K # OK
  varErr <- (ORVarComp["Var",1] - ORVarComp["Cov1",1] - ORVarComp["Cov2",1] + ORVarComp["Cov3",1])*K # OK

  return(data.frame(Estimates = c(varR, varC, varTR, varTC, varRC, varErr),
                    row.names = c("varR", "varC", "varTR", "varTC", "varRC", "varErr"),
                    "stringsAsFactors" = FALSE)
  )
  
}
