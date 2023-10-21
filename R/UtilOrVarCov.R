#' Obuchowski-Rockette variance components for dataset
#' 
#' @param dataset Factorial or cross-modality dataset
#' 
#' @param FOM Figure of merit
#' 
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC":
#'     the \code{FPFValue} at which to evaluate a partial curve based figure of merit. 
#'     The default is \code{FPFValue} = 0.2.
#'     
#' @param covEstMethod The covariance estimation method, "jackknife" 
#'     (the default) or "bootstrap" or "DeLong" ("DeLong" is applicable only for 
#'     FOM = "Wilcoxon").
#'     
#' @param nBoots  Only needed for bootstrap covariance estimation method. The number 
#'     of bootstraps, defaults to 200.
#'     
#' @param seed  Only needed for the bootstrap covariance estimation method. The initial 
#'     seed for the random number generator, the default is \code{NULL}, for random seed. 
#'     
#' @return A list containing the following \code{data.frames}: 
#'     \itemize{
#'     \item{\code{foms}}: the figures of merit for different modality-reader combinations 
#'     \item{\code{TRanova}}: the OR modality-reader ANOVA table 
#'     \item{\code{VarCom}}: the OR variance-components \code{Cov1}, \code{Cov2}, 
#'     \code{Cov3}, \code{Var} and correlations \code{rho1}, \code{rho2} and \code{rho3} 
#'     \item{\code{IndividualTrt}}: the individual modality mean-squares, \code{Var} and \code{Cov2} values
#'     \item{\code{IndividualRdr}}: the individual reader mean-squares, \code{Var} and \code{Cov1} values
#'     }
#'   
#' @details The variance components are identical to those obtained using 
#'     \link{StSignificanceTesting} with \code{method = "OR"}.
#' 
#' @examples 
#' ## use the default jackknife for covEstMethod
#' vc <- UtilOrVarCov(dataset02, FOM = "Wilcoxon")
#' str(vc) 
#'
#' UtilOrVarCov(dataset02, FOM = "Wilcoxon", 
#'    covEstMethod = "bootstrap", nBoots = 2000, seed = 100)$VarCom 
#' 
#' UtilOrVarCov(dataset02, FOM = "Wilcoxon", covEstMethod = "DeLong")$VarCom
#' 
#' UtilOrVarCov(datasetXModality, FOM = "wAFROC") 
#'   
#' @export
#' 
UtilOrVarCov <- function (dataset, FOM, FPFValue = 0.2, 
                          covEstMethod = "jackknife", nBoots = 200, seed = NULL)
{
  
  if (dataset$descriptions$design == "FCTRL") { 
    # factorial dataset, one treatment factor
    
    modalityID <- dataset$descriptions$modalityID
    readerID <- dataset$descriptions$readerID
    
    foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    ret <- UtilPseudoValues(dataset, FOM, FPFValue)
    jkFomValues <- ret$jkFomValues
    
    # following call gets all the needed variance and covariance components
    VarCovALL <- OrVarCov(jkFomValues, modalityID, readerID, covEstMethod)
    
    ret <- OrFinalOutput(foms, 
                         ret$jkFomValues, 
                         VarCovALL, 
                         modalityID, 
                         readerID)
    
  } else {
    # cross-modality factorial dataset, two treatment factors
    # 
    dsX <- dataset
    modalityID1 <- dsX$descriptions$modalityID1
    modalityID2 <- dsX$descriptions$modalityID2
    modalityID <- list(modalityID2, modalityID1)
    readerID <- dsX$descriptions$readerID
    
    fomsTemp <- UtilFigureOfMerit(dsX, FOM, FPFValue)
    foms <- FomAvgXModality(dsX, fomsTemp)
    
    ret <- UtilPseudoValues(dsX, FOM, FPFValue)
    jkFomValues <- ret$jkFomValues
    
    # following call gets all the needed variance and covariance components
    VarCovALL <- OrVarCov(jkFomValues, modalityID, readerID, covEstMethod)
     ret <- OrFinalOutputX(foms, 
                          VarCovALL, 
                          modalityID, 
                          readerID)
    
  }  
}


