UtilDBM2ORVarComp <- function(K, DBMVarComp){
  
  varR <- DBMVarComp$varR
  varTR <- DBMVarComp$varTR
  varErr <- (DBMVarComp$varC + DBMVarComp$varTC + DBMVarComp$varRC + DBMVarComp$varErr)/K
  Cov1 <- (DBMVarComp$varC + DBMVarComp$varRC)/K
  Cov2 <- (DBMVarComp$varC + DBMVarComp$varTC)/K
  Cov3 <- (DBMVarComp$varC)/K
  
  return(list (
    varR = varR,
    varTR = varTR,
    Cov1 = Cov1,
    Cov2 = Cov2,
    Cov3 = Cov3,
    varErr = varErr
  ) )
  
}
