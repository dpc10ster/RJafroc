#' Utility for Obuchowski-Rockette variance components
#' 
#' @param dataset The dataset object
#' @param FOM The figure of merit
#' @param covEstMethod The covariance estimation method, "jackknife" 
#'    (the default) or "bootstrap".
#' @param nBoots  The number of bootstraps, defaults to 200
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list object containing the variance components.
#'   
#' @details The variance components are obtained using \link{StSignificanceTesting} 
#'     with \code{method = "ORH"}.
#' 
#' @examples 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$varComp
#'
#'   
#' @export
#' 
UtilVarComponentsOR <- function (dataset, FOM, FPFValue = 0.2, covEstMethod = "Jackknife", nBoots = 200)
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




#' Utility for Dorfman-Berbaum-Metz variance components
#' 
#' @param dataset The dataset object
#' @param FOM The figure of merit
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list object containing the variance components.
#'   
#' 
#' @examples 
#' UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")
#'
#'   
#' @export
#' 
UtilVarComponentsDBM <- function (dataset, FOM, FPFValue = 0.2)
{
  NL <- dataset$NL
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  
  psVals <- UtilPseudoValues(dataset, FOM, FPFValue)$jkPseudoValues
  
  mSquares <- pseudoValueMeanSquares(psVals)
  msR <- mSquares$msR
  msC <- mSquares$msC
  msTR <- mSquares$msTR
  msTC <- mSquares$msTC
  msRC <- mSquares$msRC
  msTRC <- mSquares$msTRC
  
  varR <- (msR - msTR - msRC + msTRC)/(I * K)
  varC <- (msC - msTC - msRC + msTRC)/(I * J)
  varTR <- (msTR - msTRC)/K
  varTC <- (msTC - msTRC)/J
  varRC <- (msRC - msTRC)/I
  varErr <- msTRC
  
  varComp <- data.frame(varR = varR, 
                        varC = varC, 
                        varTR = varTR, 
                        varTC = varTC, 
                        varRC = varRC, 
                        varErr = varErr)
  
  return(list(varComp = varComp,
              mSquares = mSquares,
              psVals = psVals
  ))
}
