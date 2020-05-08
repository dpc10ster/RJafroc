#' Utility for Obuchowski-Rockette variance components
#' 
#' @param dataset The dataset object
#' @param FOM The figure of merit
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' @param covEstMethod The covariance estimation method, "jackknife" 
#'     (the default) or "bootstrap".
#' @param nBoots  Only needed for bootstrap covariance estimation method. The number 
#'     of bootstraps, defaults to 200.
#' @param seed  Only needed for the bootstrap covariance estimation method. The initial 
#'     seed for the random number generator, the default is \code{NULL}, as if no seed 
#'     has been specified. 
#' 
#' @return A list object containing the variance components.
#'   
#' @details The variance components are obtained using \link{StSignificanceTesting} 
#'     with \code{method = "ORH"}.
#' 
#' @examples 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$varComp 
#' ## uses the default jackknife for covEstMethod
#'
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon", 
#'    covEstMethod = "bootstrap", nBoots = 2000, seed = 100)$varComp 
#' 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon", 
#'    covEstMethod = "DeLong")$varComp 
#'   
#' @export
#' 
UtilVarComponentsOR <- function (dataset, FOM, FPFValue = 0.2, 
                                 covEstMethod = "jackknife", nBoots = 200, seed = NULL)
{
  
  I <- dim(dataset$NL)[1]
  J <- dim(dataset$NL)[2]
  
  ret <- gpfEstimateVarCov(dataset, FOM, FPFValue, nBoots, covEstMethod, seed)
  Var <- ret$Var
  Cov1 <- ret$Cov1
  Cov2 <- ret$Cov2
  Cov3 <- ret$Cov3
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  # single modality mean squares
  fomArray_i <- array(dim = I)
  for (i in 1:I) fomArray_i[i] <-  mean(fomArray[i,])
  msR_i <- array(0, dim = I)
  for (i in 1:I) {
    for (j in 1:J) {
      msR_i[i] <- msR_i[i] + (mean(fomArray[i, j]) - fomArray_i[i] )^2
    }
    msR_i[i] <- msR_i[i]/(J - 1)
  }
  
  modID <- as.vector(dataset$modalityID)
  msREachTrt <- data.frame(Source = "R", DF = J-1)
  colNames <- c("Source", "DF")
  for (i in 1:I) {
    msREachTrt <- cbind(msREachTrt, data.frame(msR_i[i]))
    colNames <- c(colNames, paste0("trt", modID[i]))
  }
  colnames(msREachTrt) <- colNames
  
  # calculate varR and varTR
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
  
  meanSquares <- t(meanSquares)
  colnames(meanSquares) <- "Estimates"
  
  varTR <- msTR - Var + Cov1 + max(Cov2 - Cov3, 0)
  varR <- (msR - Var - (I - 1) * Cov1 + Cov2 + (I - 1) * Cov3 - varTR)/I
  varComp <- data.frame(varR = varR,
                        varTR = varTR,
                        Cov1 = Cov1,
                        Cov2 = Cov2,
                        Cov3 = Cov3,
                        Var = Var,
                        stringsAsFactors = FALSE)
  varComp <- t(varComp)
  colnames(varComp) <- "Estimates"

  rhoOR <- data.frame(rho1 = as.character(round(Cov1/Var,5)), 
                      rho2 = as.character(round(Cov2/Var,5)), 
                      rho3 = as.character(round(Cov3/Var,5)), 
                      stringsAsFactors = FALSE)
  
  # done
  return(list(
    varComp = varComp,
    rhoOR = rhoOR,
    meanSquares = meanSquares,
    msREachTrt = msREachTrt
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
                        varErr = varErr,
                        stringsAsFactors = FALSE)
  
  varComp <- t(varComp)
  colnames(varComp) <- "Estimates"
  
  # done
  return(list(varComp = varComp,
              mSquares = mSquares,
              psVals = psVals
  ))
}
