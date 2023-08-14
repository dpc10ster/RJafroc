#' Utility for estimating Obuchowski-Rockette variance components for crossed modality factorial datasets
#' 
#' @param dsX The crossed modality factorial dsX object
#' @param FOM The figure of merit
#'     
#' @return A list containing the following \code{data.frames}: 
#'     \itemize{
#'     \item{\code{foms}}: the figures of merit for different modality-reader combinations 
#'     \item{\code{TRanova1}}: the OR modality-reader ANOVA table 
#'     \item{\code{VarCom}}: the OR variance-components \code{Cov1}, \code{Cov2}, 
#'     \code{Cov3}, \code{Var} and correlations \code{rho1}, \code{rho2} and \code{rho3} 
#'     \item{\code{IndividualTrt}}: the individual modality mean-squares, \code{Var} and \code{Cov2} values
#'     \item{\code{IndividualRdr}}: the individual reader mean-squares, \code{Var} and \code{Cov1} values
#'     }
#'   
#' 
#' @examples 
#' ## vc <- UtilVarComponentsORX(datasetXModality, FOM = "wAFROC")
#'
#' @export
#' 
#' @references
#' Thompson JD, Chakraborty DP, Szczepura K, et al. (2016) Effect of reconstruction 
#' methods and x-ray tube current-time product  on nodule detection in an 
#' anthropomorphic thorax phantom: a crossed-modality JAFROC observer study. 
#' Medical Physics. 43(3):1265-1274.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' 
UtilVarComponentsORX <- function (dsX, FOM)
{
  
  if (dsX$descriptions$design != "FCTRL-X-MOD") stop("This functions requires a crossed modality factorial dataset")  
  
  I1 <- dim(dsX$ratings$NL)[1]
  I2 <- dim(dsX$ratings$NL)[2]
  J <- dim(dsX$ratings$NL)[3]
  
  ret <- UtilFigureOfMeritX(dsX, FOM)
  fomArray1 <- data.matrix(ret$avg1)
  fomArray2 <- data.matrix(ret$avg2)
  
  fomMean <- mean(fomArray1)
  #fomMean2 <- mean(fomArray2) same
  ds <- DsConvertX2Std(dsX, 1)
  
  
  
  st <- StMeanSquaresEtcX(ds, fomArray1, FOM)
  
  
}


DsConvertX2Std <- function(dsX, trt) {
  
  I1 <- length(dsX$ratings$NL[,1,1,1,1])
  I2 <- length(dsX$ratings$NL[1,,1,1,1])
  J <- length(dsX$ratings$NL[1,1,,1,1])
  K <- length(dsX$ratings$NL[1,1,1,,1])
  K2 <- length(dsX$ratings$LL[1,1,1,,1])
  K1 <- K - K2
  maxNL <- length(dsX$ratings$NL[1,1,1,1,])
  maxLL <- length(dsX$ratings$LL[1,1,1,1,])
  
  
  # fix NL ratings
  if (trt == 1) {
    NL <- dsX$ratings$NL[1,,,,]
    dim(NL) <- c(I2,J,K,maxNL)
  } else {
      NL <- dsX$ratings$NL[2,,,,]
      dim(NL) <- c(I1,J,K,maxNL)
  }
  
  # fix LL ratings
  if (trt == 1) {
    LL <- dsX$ratings$LL[1,,,,]
    dim(LL) <- c(I2,J,K2,maxLL)
  } else {
    LL <- dsX$ratings$LL[2,,,,]
    dim(LL) <- c(I1,J,K2,maxLL)
  }
  
  
  if (trt == 1 ) modalityID <- as.character(seq(1:I2)) else modalityID <- as.character(seq(1:I1))
  
  ds <- convert2dataset(NL, 
                        LL, 
                        LL_IL = NA, 
                        perCase = dsX$lesions$perCase, 
                        IDs = dsX$lesions$IDs, 
                        weights = dsX$lesions$weights,
                        fileName = dsX$descriptions$fileName, 
                        type = dsX$descriptions$type, 
                        name = dsX$descriptions$name, 
                        truthTableStr= dsX$descriptions$truthTableStr, 
                        design = "FCTRL", 
                        modalityID = modalityID, 
                        readerID = dsX$descriptions$readerID)
  return(ds)
  
}




