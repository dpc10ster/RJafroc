#' Calculate empirical figures of merit for cross-modality dataset
#' 
#' @description  Calculate the specified empirical figure of merit
#'    for each treatment-reader combination in the cross-modality dataset
#' 
#' @param dsX The cross-modality dataset to be analyzed
#' 
#' @param FOM The figure of merit; the default is \code{"wAFROC"}
#' 
#' @return A \code{c(I1, I2, J)} data frame, where the row names are \code{modalityID}'s of the 
#'    treatments and column names are the \code{readerID}'s of the readers.
#' 
#' @details The allowed FOMs depend on the \code{dataType} field of the 
#'    \code{dataset} object.  
#' 
#'    \strong{For \code{dataset$descriptions$type = "ROC"} only \code{FOM = "Wilcoxon"} is allowed}.
#'    \strong{For \code{dataset$descriptions$type = "FROC"} the following FOMs are allowed}:
#'    \itemize{ 
#'    \item \code{FOM = "AFROC1"} (use only if zero normal cases)
#'    \item \code{FOM = "AFROC"} 
#'    \item \code{FOM = "wAFROC1"} (use only if zero normal cases)
#'    \item \code{FOM = "wAFROC"} (the default) 
#'    \item \code{FOM = "HrAuc"} 
#'    \item \code{FOM = "SongA1"} 
#'    \item \code{FOM = "SongA2"}  
#'    \item \code{FOM = "HrSe"} (an example of an end-point based FOM)
#'    \item \code{FOM = "HrSp"} (another example)
#'    \item \code{FOM = "MaxLLF"} (do:)
#'    \item \code{FOM = "MaxNLF"} (do:)
#'    \item \code{FOM = "MaxNLFAllCases"} (do:) 
#'    \item \code{FOM = "ExpTrnsfmSp"}  
#'    } 
#'    \code{"MaxLLF"}, \code{"MaxNLF"} and \code{"MaxNLFAllCases"}
#'    correspond to ordinate, and abscissa, respectively, of the highest point 
#'    on the FROC operating characteristic obtained by counting all the marks. 
#'    The \code{"ExpTrnsfmSp"} FOM is described in the paper by Popescu. 
#'    Given the large number of FOMs possible with FROC data, it is appropriate 
#'    to make a recommendation: \strong{it is recommended that one use the wAFROC FOM
#'    whenever possible. If the dataset has no non-diseased cases one should use the
#'    the wAFROC1 FOM}.
#'    
#' 
#'
#' @examples
#' 
#' ##UtilFigureOfMeritX(datasetXModality, FOM = "wAFROC")
#' 
#' 
#' @references
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' Chakraborty DP, Berbaum KS (2004) Observer studies involving detection and localization: modeling, analysis, and validation, 
#' Medical Physics, 31(8), 1--18.
#' 
#' Song T, Bandos AI, Rockette HE, Gur D (2008) On comparing methods for discriminating between actually negative and actually positive subjects 
#' with FROC type data, Medical Physics 35 1547--1558.
#' 
#' Popescu LM (2011) Nonparametric signal detectability evaluation using an exponential transformation of the FROC curve, 
#' Medical Physics, 38(10), 5690. 
#' 
#' Obuchowski NA, Lieber ML, Powell KA (2000) Data Analysis for Detection and Localization of Multiple Abnormalities 
#' with Application to Mammography, Acad Radiol, 7:7 553--554.
#' 
#' Swensson RG (1996) Unified measurement of observer performance in detecting and localizing target objects on images, 
#' Med Phys 23:10, 1709--1725.

#' @importFrom dplyr between  
#' @export

UtilFigureOfMeritX <- function(dsX, FOM = "wAFROC") { 
  
  dataType <- dsX$descriptions$type
  if ((dataType == "ROC") && !(FOM %in% c("Wilcoxon"))) {
    errMsg <- paste0("Must use Wilcoxon figure of merit with ROC data.")
    stop(errMsg)
  }
  
  if (dataType %in% c("ROI", "LROC")) {
    stop("ROI or LROC cross-modality analysis is not supported.\n")
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
    #  FOM = AFROC1, wAFROC1 not yet tested 8/7/23
    for (i1 in 1:I1) {
      for (i2 in 1:I2) {
        for (j in 1:J) {
          fomArray[i1, i2,j] <- MyFom_ij(NL[i1, i2, j, , ], LL[i1, i2, j, , ], perCase, IDs, weights, maxNL, maxLL, K1, K2, FOM)
        }
      }
    }
  } else stop("FOM is not consistent with data type\n")  
  
  modalityID1 <- dsX$descriptions$modalityID1
  modalityID2 <- dsX$descriptions$modalityID2
  readerID <- dsX$descriptions$readerID
  rownames(fomArray) <- paste("trt", sep = "", c(modalityID1, modalityID2))
  colnames(fomArray) <- paste("rdr", sep = "", readerID)
  return(as.data.frame(fomArray))
} 


