#' Calculate empirical figures of merit (FOMs) for specified dataset
#' 
#' @description  Calculate the specified empirical figure of merit
#' for each treatment-reader combination in the ROC, FROC, ROI or LROC dataset
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}
#' @param FOM The figure of merit to be used in the calculation. 
#'    The default is \code{"wAFROC"}
#' @param FPFValue Only needed for LROC data; where to evaluate a partial 
#'    curve based figure of merit. The default is 0.2.
#' 
#' @return An \code{c(I, J)} array, where the row names are the IDs of the 
#'    treatments and column names are the IDs of the readers.
#' 
#' @details The allowed FOMs depend on the type of dataset (i.e., \code{dataType} 
#'    field of dataset object). 
#'    For \strong{ROC datasets:} only \code{"Wilcoxon"} is allowed.
#'    For \strong{FROC datasets:} The following FOMs are allowed:
#'    \code{"AFROC1"}, 
#'    \code{"AFROC"}, 
#'    \code{"wAFROC1"}, 
#'    \code{"wAFROC"} (the default), 
#'    \code{"HrAuc"}, 
#'    \code{"SongA1"}, 
#'    \code{"SongA2"},  
#'    \code{"HrSe"}, 
#'    \code{"HrSp"}, 
#'    \code{"MaxLLF"}, 
#'    \code{"MaxNLF"}, 
#'    \code{"MaxNLFAllCases"}, 
#'    \code{"ExpTrnsfmSp"}, and 
#'    \code{"ROI"}. 
#'    The \code{"MaxLLF"}, \code{"MaxNLF"} and \code{"MaxNLFAllCases"} FOMs 
#'    correspond to ordinate, and abscissa, respectively, of the highest point 
#'    on the FROC operating characteristic obtained by counting all the marks). 
#'    The \code{"ExpTrnsfmSp"} FOM is described in the paper by Popescu. 
#'    Given the large number of FOMs possible with FROC data, it is appropriate 
#'    to make a recommendation: \strong{it is recommended that one use the wAFROC FOM.}
#'    For \strong{LROC datasets:} The following FOMs are allowed:\code{"Wilcoxon"} for 
#'    ROC data inferred from LROC data, which ignores localization information; 
#'    or \code{"PCL"} or \code{"ALROC"}, in which case one needs to specify an 
#'    additional argument, \code{FPFValue}: the desired FPF at which to evaluate 
#'    PCL or ALROC; the default is 0.2.
#' 
#'
#' @examples
#' # ROC data
#' UtilFigureOfMerit(dataset = dataset02, FOM = "Wilcoxon") 
#' # FROC dataset, converted to ROC, Wilcoxon FOM
#' UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "Wilcoxon") 
#' # FROC dataset, default wAFROC FOM
#' UtilFigureOfMerit(dataset = dataset01) 
#' #LROC data
#' UtilFigureOfMerit(dataset = datasetCadLroc, FOM = "ALROC", FPFValue = 0.2) 
#' #ROI data
#' UtilFigureOfMerit(datasetROI) 
#' 
#' @references
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
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

#' @import dplyr  
#' @export

UtilFigureOfMerit <- function(dataset, FOM = "wAFROC", FPFValue = 0.2) { # dpc
  
  dataType <- dataset$dataType
  if (dataType == "ROI" && FOM != "ROI") {
    cat("Incorrect FOM supplied, changing to 'ROI'\n")
    FOM <- "ROI"
  }
 
  if (!(dataType %in% c("ROC", "LROC")) && FOM == "Wilcoxon")
    stop("Cannot use `Wilcoxon` FOM with `FROC` or `ROI` data.")
  
  if (dataType != "ROI" && FOM == "ROI") {
    errMsg <- paste0("Only ROI data can be analyzed using ROI figure of merit.")
    stop(errMsg)
  }
  
  if (dataType == "LROC") {
    if (!between(FPFValue, 0, 1)) stop("FPFValue is outside valid range")
    if (FOM %in% c("Wilcoxon", "ALROC", "PCL")) {
      NL <- dataset$NL
      LL <- dataset$LLCl
      #fomArray <- lroc2fomMrmc (dataset, FOM, FPFValue)
    } else stop("Incorrect FOM specified for LROC data")
    #return(fomArray)
  } else {
    NL <- dataset$NL
    LL <- dataset$LL
  }
  
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2  
  
  if (K1 == 0 && !(FOM %in% c("JAFROC1", "wJAFROC1", "AFROC1", "wAFROC1"))) {
    errMsg <- paste0("Only JAFROC1/AFROC1 or wJAFROC1/wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.")
    stop(errMsg)
  }
  
  lesionNum <- dataset$lesionNum
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  fomArray <- array(dim = c(I, J))
  for (i in 1:I) {
    for (j in 1:J) {
      nl <- NL[i, j, , ]
      ll <- LL[i, j, , ]
      dim(nl) <- c(K, maxNL)
      dim(ll) <- c(K2, max(lesionNum))
      fomArray[i, j] <- gpfMyFOM(nl, ll, lesionNum, lesionID, lesionWeight, maxNL, maxLL, K1, K2, FOM, FPFValue = FPFValue)
    }
  }
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  rownames(fomArray) <- paste("Trt", modalityID, sep = " - ")
  colnames(fomArray) <- paste("Rdr", readerID, sep = " - ")
  return(fomArray)
} 


Wilcoxon <- function (zk1, zk2)
{
  
  K1 = length(zk1)
  K2 = length(zk2)
  
  W <- 0
  for (k1 in 1:K1) {
    W <- W + sum(zk1[k1] < zk2)
    W <- W + 0.5 * sum(zk1[k1] == zk2)
  }
  W <- W/K1/K2
  
  return (W)
  
}

