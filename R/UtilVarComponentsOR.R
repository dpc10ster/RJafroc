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
#' @return A list object containing the following \code{data.frames}: 
#'     \itemize{
#'     \item{\code{foms}}: the figures of merit for different treatment-reader combinations 
#'     \item{\code{TRanova}}: the OR treatment-reader ANOVA table 
#'     \item{\code{VarCom}}: the OR variance-components \code{Cov1}, \code{Cov2}, 
#'     \code{Cov3}, \code{Var} and correlations \code{rho1}, \code{rho2} and \code{rho3} 
#'     \item{\code{IndividualTrt}}: the individual treatment mean-squares, \code{Var} and \code{Cov2} values
#'     \item{\code{IndividualRdr}}: the individual reader mean-squares, \code{Var} and \code{Cov1} values
#'     }
#'   
#' @details The variance components are obtained using \link{StSignificanceTesting} 
#'     with \code{method = "ORH"}.
#' 
#' @examples 
#' ## uses the default jackknife for covEstMethod
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$foms 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$TRanova 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$VarCom 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$IndividualTrt 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$IndividualRdr 
#'
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon", 
#'    covEstMethod = "bootstrap", nBoots = 2000, seed = 100)$VarCom 
#' 
#' UtilVarComponentsOR(dataset02, FOM = "Wilcoxon", covEstMethod = "DeLong")$VarCom 
#'   
#' @export
#' 
UtilVarComponentsOR <- function (dataset, FOM, FPFValue = 0.2, 
                                 covEstMethod = "jackknife", nBoots = 200, seed = NULL)
{
  
  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
  
  # Foms is local value; 
  # `as.matrix` is absolutely necessary if following `mean()` function is to work
  Foms <- as.matrix(UtilFigureOfMerit(dataset, FOM, FPFValue))
  
  fomMean <- mean(Foms[,]) # this fails if `Foms` is a dataframe; true for `mean` and `median`
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (mean(Foms[i, ]) - fomMean)^2
  }
  msT <- J * msT/(I - 1)
  
  msR <- 0
  for (j in 1:J) {
    msR <- msR + (mean(Foms[, j]) - fomMean)^2
  }
  msR <- I * msR/(J - 1)
  
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (Foms[i, j] - mean(Foms[i, ]) - mean(Foms[, j]) + fomMean)^2
    }
  }
  msTR <- msTR/((J - 1) * (I - 1))
  
  msArray <- c(msT, msR, msTR)
  dfArray <- c(I - 1, J - 1, (I - 1) * (J - 1))
  ssArray <- msArray * dfArray
  
  TRanova <- data.frame("SS" = ssArray, 
                        "DF" = dfArray, 
                        "MS" = msArray,
                        stringsAsFactors = FALSE)  
  rownames(TRanova) <- c("T", "R", "TR")
  
  # single treatment msR_i ############################################################
  msR_i <- array(0, dim = I)
  for (i in 1:I) {
    for (j in 1:J) {
      msR_i[i] <- msR_i[i] + (Foms[i, j] -  mean(Foms[i,]))^2
    }
  }
  msR_i <- msR_i/(J - 1)
  
  # for (i in 1:I) {
  #   if (dataset$design != "SPLIT-PLOT") {
  #     cov2EachTrt <- vector(length = I)
  #   } else  {
  #     cov2EachTrt <- rep(0, I)
  #   }
  # }
  cov2EachTrt <- vector(length = I)
  varEachTrt <- vector(length = I)
  for (i in 1:I) {
    dsi <- DfExtractDataset(dataset, trts = i)
    ret <- selectCovEstMethod(dsi, FOM, FPFValue, nBoots, covEstMethod, seed)
    varEachTrt[i] <- ret$Var
    cov2EachTrt[i] <- ret$Cov2
  }
  
  modID <- as.vector(dataset$descriptions$modalityID)
  IndividualTrt <- data.frame(DF = rep(J-1, I), 
                              msREachTrt = msR_i, 
                              varEachTrt = varEachTrt, 
                              cov2EachTrt = cov2EachTrt, 
                              row.names = paste0("trt", modID),
                              stringsAsFactors = FALSE)
  # } else IndividualTrt <- NA # these are not defined for split-plot datasets
  
  # single reader msT_j ###############################################################
  msT_j <- array(0, dim = J)
  for (j in 1:J) {
    for (i in 1:I) {
      msT_j[j] <- msT_j[j] + (mean(Foms[i, j]) -  mean(Foms[,j]))^2
    }
    msT_j[j] <- msT_j[j]/(I - 1)
  }
  
  varEachRdr <- vector(length = J)
  cov1EachRdr <- vector(length = J)
  for (j in 1:J) {
    dsj <- DfExtractDataset(dataset, rdrs = j)
    ret <- selectCovEstMethod(dsj, FOM, FPFValue, nBoots, covEstMethod, seed)
    varEachRdr[j] <- ret$Var
    cov1EachRdr[j] <- ret$Cov1
  }
  
  rdrID <- as.vector(dataset$descriptions$readerID)
  IndividualRdr <- data.frame(DF = rep(I-1, J), 
                              msTEachRdr = msT_j, 
                              varEachRdr = varEachRdr, 
                              cov1EachRdr = cov1EachRdr, 
                              row.names = paste0("rdr", rdrID),
                              stringsAsFactors = FALSE)
  
  #####################################################################################
  ret <- selectCovEstMethod(dataset, FOM, FPFValue, nBoots, covEstMethod, seed)
  Var <- ret$Var
  Cov1 <- ret$Cov1
  Cov2 <- ret$Cov2
  Cov3 <- ret$Cov3
  
  # TBA Need citation here for next two equations
  VarTR <- msTR - Var + Cov1 + max(Cov2 - Cov3, 0)
  VarR <- (msR - Var - (I - 1) * Cov1 + Cov2 + (I - 1) * Cov3 - VarTR)/I
  VarCom <- data.frame(Estimates = c(VarR, VarTR, Cov1, Cov2, Cov3, Var), 
                       Rhos = c(NA, NA, Cov1/Var, Cov2/Var, Cov3/Var, NA),
                       row.names = c("VarR", "VarTR", "Cov1", "Cov2", "Cov3", "Var"),
                       stringsAsFactors = FALSE)
  return(list(
    TRanova = TRanova,
    VarCom = VarCom,
    IndividualTrt = IndividualTrt,
    IndividualRdr = IndividualRdr
  ))
  
}

