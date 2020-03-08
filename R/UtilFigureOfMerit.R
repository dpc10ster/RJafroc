#' Calculate empirical figures of merit (FOMs) for specified dataset
#' 
#' @description  Calculate the specified empirical figure of merit
#'    for each treatment-reader combination in the 
#'    ROC, FROC, ROI or LROC dataset
#' 
#' @param dataset The dataset to be analyzed, \code{\link{RJafroc-package}}
#' @param FOM The figure of merit; the default is \code{"wAFROC"}
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return An \code{c(I, J)} array, where the row names are \code{modalityID}'s of the 
#'    treatments and column names are the \code{readerID}'s of the readers.
#' 
#' @details The allowed FOMs depend on the \code{dataType} field of the 
#'    \code{dataset} object.  
#' 
#'    \strong{For \code{dataset$design = "SPLIT-PLOT"}, end-point based 
#'    FOMs (e.g., "MaxLLF") are not allowed}.
#'    \strong{For \code{dataset$dataType = "ROC"} only \code{FOM = "Wilcoxon"} is allowed}.
#'    \strong{For \code{dataset$dataType = "FROC"} the following FOMs are allowed}:
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
#'    whenever possible.}
#'    
#'    For \strong{\code{dataType = "ROI"} dataset only \code{FOM = "ROI"} is allowed}.
#'    
#'    For \strong{\code{dataType = "LROC"}} dataset the following FOMs are allowed:
#'    \itemize{
#'    \item \code{FOM = "Wilcoxon"} for ROC data inferred from LROC data 
#'    \item \code{FOM = "PCL"} the probability of correct localization at specified \code{FPFValue}
#'    \item \code{FOM = "ALROC"} the area under the LROC from zero to specified \code{FPFValue} 
#'    }
#'    \code{FPFValue} The FPF at which to evaluate \code{PCL} or \code{ALROC}; 
#'       the default is 0.2; only needed for LROC data.
#' 
#'
#' @examples
#' UtilFigureOfMerit(dataset02, FOM = "Wilcoxon") # ROC data
#' UtilFigureOfMerit(DfFroc2Roc(dataset01), FOM = "Wilcoxon") # FROC dataset, converted to ROC
#' UtilFigureOfMerit(dataset01) # FROC dataset, default wAFROC FOM
#' UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon") #LROC data
#' UtilFigureOfMerit(datasetCadLroc, FOM = "PCL") #LROC data
#' UtilFigureOfMerit(datasetCadLroc, FOM = "ALROC") #LROC data
#' UtilFigureOfMerit(datasetROI, FOM = "ROI") #ROI data
#' \donttest{ # these are meant to illustrate conditions which will throw an error
#' ## UtilFigureOfMerit(dataset02, FOM = "wAFROC") #error
#' ## UtilFigureOfMerit(dataset01, FOM = "Wilcoxon") #error
#' }
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

#' @importFrom dplyr between  
#' @export

# v.1.3.1.9000: added SPLIT-PLOT capability 
UtilFigureOfMerit <- function(dataset, FOM = "wAFROC", FPFValue = 0.2) { # dpc

  dataType <- dataset$dataType
  if (dataType == "ROC" && FOM != "Wilcoxon") {
    errMsg <- paste0("Must use Wilcoxon figure of merit with ROC data.")
    stop(errMsg)
  }
  
  if (dataType == "ROI" && FOM != "ROI") {
    cat("Incorrect FOM supplied for ROI data, changing to 'ROI'\n")
    FOM <- "ROI"
  }
  
  if (!(dataType %in% c("ROC", "LROC")) && FOM == "Wilcoxon")
    stop("Cannot use `Wilcoxon` FOM with `FROC` or `ROI` data.")
  
  if (dataType != "ROI" && FOM == "ROI") {
    errMsg <- paste0("Only ROI data can be analyzed using ROI figure of merit.")
    stop(errMsg)
  }
  
  if (dataType == "LROC") {
    if (FOM != "Wilcoxon") { 
      if (!between(FPFValue, 0, 1)) stop("FPFValue is outside valid range")
    }
  }
  
  if (dataType == "LROC") {
    if (FOM %in% c("Wilcoxon", "ALROC", "PCL")) {
      if (dataType != "LROC") {
        NL <- dataset$NL
        LL <- dataset$LL
      } else {
        if (FOM == "Wilcoxon"){
          datasetRoc <- DfLroc2Roc(dataset)
          NL <- datasetRoc$NL
          LL <- datasetRoc$LL
        } else if (FOM %in% c("PCL", "ALROC")){
          NL <- dataset$NL
          LL <- dataset$LLCl
        } else stop("incorrect FOM for LROC data")
      }
    } else stop("Incorrect FOM specified for LROC data")
  } else {
    NL <- dataset$NL
    LL <- dataset$LL
  }
  
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2  
  
  if (K1 == 0 && !(FOM %in% c("FOM_AFROC1", "FOM_wAFROC1"))) {
    errMsg <- paste0("Only FOM_AFROC1 or FOM_wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.")
    stop(errMsg)
  }

  if (length(dataset) == 12) {
    design <- dataset$design
    t <- dataset$truthTableStr
  } else if (length(dataset) %in% c(8,9)) {
    design <- "CROSSED"
  } 
  
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  fomArray <- array(dim = c(I, J))
  for (i in 1:I) {
    for (j in 1:J) {
      if (design == "SPLIT-PLOT") {
        k1_j_sub <- !is.na(t[1,j,,1]) | !is.na(t[1,j,,2])
        k2_j_sub <- !is.na(t[1,j,,2])[(K1+1):K]
        nl_j <- NL[i, j, k1_j_sub, ]
        lV_j <- dataset$lesionVector[k2_j_sub]
        maxLL_j <- max(lV_j)
        ll_j <- LL[i, j, k2_j_sub, 1:maxLL_j]
        k1j <- sum(!is.na(t[1,j,,1]))
        k2j <- sum(!is.na(t[1,j,,2]))
        lID_j <- dataset$lesionID[k2_j_sub,1:maxLL_j, drop = FALSE]
        lW_j <- dataset$lesionWeight[k2_j_sub,1:maxLL_j, drop = FALSE]
        dim(nl_j) <- c(k1j+k2j, maxNL)
        dim(ll_j) <- c(k2j, maxLL_j)
        fomArray[i, j] <- gpfMyFOM(nl_j, ll_j, lV_j, lID_j, lW_j, maxNL, maxLL_j, k1j, k2j, FOM, FPFValue)
        next
      } else if (design == "CROSSED"){
        nl_j <- NL[i, j, , ]
        ll_j <- LL[i, j, , ]
        dim(nl_j) <- c(K, maxNL)
        dim(ll_j) <- c(K2, maxLL)
        fomArray[i, j] <- gpfMyFOM(nl_j, ll_j, dataset$lesionVector, dataset$lesionID, dataset$lesionWeight, maxNL, maxLL, K1, K2, FOM, FPFValue)
      } else stop("Incorrect design, must be SPLIT-PLOT or CROSSED")
    }
  }
  
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  rownames(fomArray) <- paste("Trt", sep = "", modalityID)
  colnames(fomArray) <- paste("Rdr", sep = "", readerID)
  return(fomArray)
} 


