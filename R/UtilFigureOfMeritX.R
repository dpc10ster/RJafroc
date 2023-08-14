#' Empirical figures of merit for cross-modality dataset
#' 
#' @description  Calculate specified empirical figure of merit
#'    for each modality-reader combination in a cross-modality dataset
#' 
#' @param dsX The cross-modality dataset to be analyzed.
#' 
#' @param FOM The figure of merit; the default is \code{"wAFROC"}
#' 
#' @return Two data frames: 
#' * \code{c(I2, J)} data frame, FOMs averaged over the first modality, where the row names are modality IDS of the 
#'    second modality 
#' * \code{c(I1, J)} data frames, FOMs averaged over the second modality, where the row names are modality IDs of the 
#'    first modality, 
#' * The column names are the \code{readerID}'s.
#' 
#' @details The allowed FOMs depend on the \code{dataType} field of the 
#'    \code{dataset} object.  See \code{\link{UtilFigureOfMerit}}.   
#'    
#' 
#'
#' @examples
#' 
#' UtilFigureOfMeritX(datasetXModality, FOM = "wAFROC")
#' 
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
#' @importFrom dplyr between  
#' @export

UtilFigureOfMeritX <- function(dsX, FOM = "wAFROC") { 
  
  dataType <- dsX$descriptions$type
  if ((dataType == "ROC") && !(FOM %in% c("Wilcoxon"))) {
    errMsg <- paste0("Must use Wilcoxon figure of merit with ROC data.")
    stop(errMsg)
  }
  
  if (dataType %in% c("ROI", "LROC")) {
    stop("ROI or LROC **cross-modality analysis** is not supported.\n")
  }
  
  if ((dataType %in% c("FROC")) && (FOM == "Wilcoxon"))
    stop("Cannot use `Wilcoxon` FOM with `FROC` data.")
  
  NL <- dsX$ratings$NL
  LL <- dsX$ratings$LL
  
  I1 <- dim(NL)[1]
  I2 <- dim(NL)[2]
  J <- dim(NL)[3]
  K <- dim(NL)[4]
  K2 <- dim(LL)[4]
  K1 <- K - K2  
  maxNL <- dim(NL)[5]
  maxLL <- dim(LL)[5]
  
  if ((K1 == 0) && !(FOM %in% c("AFROC1", "wAFROC1"))) {
    errMsg <- paste0("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.")
    stop(errMsg)
  }
  
  fomArray <- array(dim = c(I1, I2, J))
  if (((dataType == "FROC") && 
       (FOM %in% c("HrAuc", "AFROC", "wAFROC", "AFROC1", "wAFROC1"))) || 
      ((dataType == "ROC") && (FOM %in% c("Wilcoxon")))) {
    
    for (i1 in 1:I1) {
      for (i2 in 1:I2) {
        for (j in 1:J) {
          fomArray[i1, i2, j] <- MyFom_ij(NL[i1, i2, j, , ], 
                                          LL[i1, i2, j, , ], 
                                          dsX$lesions$perCase, 
                                          dsX$lesions$IDs, 
                                          dsX$lesions$weights, 
                                          maxNL, 
                                          maxLL, 
                                          K1, 
                                          K2, 
                                          FOM)
        }
      }
    }
  } else stop("FOM is not consistent with data type\n")  
  
  modalityID1 <- dsX$descriptions$modalityID1
  modalityID2 <- dsX$descriptions$modalityID2
  readerID <- dsX$descriptions$readerID
  fomArray1 <- apply(fomArray, c(2,3), mean) # average over first modality
  fomArray2 <- apply(fomArray, c(1,3), mean) # average over second modality
  rownames(fomArray1) <- paste("trt", sep = "-", c(modalityID2))
  rownames(fomArray2) <- paste("trt", sep = "-", c(modalityID1))
  colnames(fomArray1) <- paste("rdr", sep = "-", readerID)
  colnames(fomArray2) <- paste("rdr", sep = "-", readerID)
  
  return(list(
    avg1 = as.data.frame(fomArray1),
    avg2 = as.data.frame(fomArray2)
  ))
} 


