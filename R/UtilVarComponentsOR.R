UtilVarComponentsOR <- function (dataset, FOM = FOM, covEstMethod, nBoots, FPFValue = FPFValue)
{
  ret <- ExtractRatings (dataset, FOM)
  NL <- ret$NL
  LL <- ret$LL
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  
  ret <- gpfEstimateVarCov(dataset, FOM, FPFValue = FPFValue, nBoots, covEstMethod)
  var <- ret$var
  cov1 <- ret$cov1
  cov2 <- ret$cov2
  cov3 <- ret$cov3
  
  # calculate varR and varTR
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue = FPFValue)
  fomMean <- mean(fomArray)
  
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(fomArray[i, ]) - fomMean)^2
  }
  msT <- J * msT/(I - 1)
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(fomArray[, j]) - fomMean)^2
  }
  msR <- I * msR/(J - 1)
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (fomArray[i, j] - mean(fomArray[i, ]) - mean(fomArray[, j]) + fomMean)^2
    }
  }
  msTR <- msTR/((J - 1) * (I - 1))
  
  meanSquares <- list(msT = msT,
                      msR = msR,
                      msTR = msTR)
  
  varTR <- msTR - var + cov1 + max(cov2 - cov3, 0)
  varR <- (msR - var - (I - 1) * cov1 + cov2 + (I - 1) * cov3 - varTR)/I
  varCovArray <- c(varR, varTR, cov1, cov2, cov3, var)
  nameArray <- c("Var(R)", "Var(T*R)", "COV1", "COV2", "COV3", "Var(Error)")
  varComp <- data.frame(varCov = varCovArray, row.names = nameArray)
  
  # done
  return(list(
    varComp = varComp,
    meanSquares = meanSquares
  ))
  
}


ExtractRatings <- function(dataset, FOM)
{
  dataType <- dataset$dataType
  
  if (dataType != "LROC") {
    NL <- dataset$NL
    LL <- dataset$LL
  } else {
    if (FOM == "Wilcoxon"){
      dataset <- DfLroc2Roc(dataset)
      NL <- dataset$NL
      LL <- dataset$LL
    } else if (FOM %in% c("PCL", "ALROC")){
      NL <- dataset$NL
      LL <- dataset$LLCl
    } else stop("incorrect FOM for LROC data")
  }
  return(list(
    NL = NL,
    LL = LL
  ))
  
}