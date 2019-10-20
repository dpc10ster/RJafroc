UtilVarComponentsOR <- function (dataset, FOM = FOM, covEstMethod, nBoots, FPFValue)
{
  ret <- dataset2ratings (dataset, FOM)

  I <- dim(ret$zjk1)[1]
  J <- dim(ret$zjk1)[2]
  
  ret <- gpfEstimateVarCov(dataset, FOM, FPFValue, nBoots, covEstMethod)
  var <- ret$var
  cov1 <- ret$cov1
  cov2 <- ret$cov2
  cov3 <- ret$cov3
  
  # calculate varR and varTR
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
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
  
  meanSquares <- data.frame(msT = msT, 
                            msR = msR, 
                            msTR = msTR)
  
  varTR <- msTR - var + cov1 + max(cov2 - cov3, 0)
  varR <- (msR - var - (I - 1) * cov1 + cov2 + (I - 1) * cov3 - varTR)/I
  # VarComp <- c(varR, varTR, cov1, cov2, cov3, var)
  # nameArray <- c("Var(R)", "Var(T*R)", "COV1", "COV2", "COV3", "Var(Error)")
  varComp <- data.frame(varR = varR,
                        varTR = varTR,
                        cov1 = cov1,
                        cov2 = cov2,
                        cov3 = cov3,
                        var = var)

  # done
  return(list(
    varComp = varComp,
    meanSquares = meanSquares
  ))
  
}

