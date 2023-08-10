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
#' @return An \code{c(I, J)} dataframe, where the row names are \code{modalityID}'s of the 
#'    treatments and column names are the \code{readerID}'s of the readers.
#' 
#' @details The allowed FOMs depend on the \code{dataType} field of the 
#'    \code{dataset} object.  
#' 
#'    \strong{For \code{dataset$descriptions$design = "SPLIT-PLOT-C"}, end-point based 
#'    FOMs (e.g., "MaxLLF") are not allowed}.
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

# v.1.3.1.9000: added SPLIT-PLOT-C capability 
UtilFigureOfMerit <- function(dataset, FOM = "wAFROC", FPFValue = 0.2) { # dpc
  
  dataType <- dataset$descriptions$type
  if ((dataType == "ROC") && !(FOM %in% c("Wilcoxon"))) {
    errMsg <- paste0("Must use Wilcoxon figure of merit with ROC data.")
    stop(errMsg)
  }
  
  if ((dataType == "ROI") && (FOM != "ROI")) {
    cat("Incorrect FOM supplied for ROI data, changing to 'ROI'\n")
    FOM <- "ROI"
  }
  
  if ((dataType %in% c("FROC", "ROI")) && (FOM == "Wilcoxon"))
    stop("Cannot use `Wilcoxon` FOM with `FROC` or `ROI` data.")
  
  if ((dataType != "ROI") && (FOM == "ROI")) {
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
        NL <- dataset$ratings$NL
        LL <- dataset$ratings$LL
      } else {
        if (FOM == "Wilcoxon"){
          datasetRoc <- DfLroc2Roc(dataset)
          NL <- datasetRoc$ratings$NL
          LL <- datasetRoc$ratings$LL
        } else if (FOM %in% c("PCL", "ALROC")){
          NL <- dataset$ratings$NL
          LL <- dataset$ratings$LL
        } else stop("incorrect FOM for LROC data")
      }
    } else stop("Incorrect FOM specified for LROC data")
  } else {
    NL <- dataset$ratings$NL
    LL <- dataset$ratings$LL
  }
  
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2  
  
  if ((K1 == 0) && !(FOM %in% c("AFROC1", "wAFROC1"))) {
    errMsg <- paste0("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.")
    stop(errMsg)
  }
  
  design <- dataset$descriptions$design
  t <- dataset$descriptions$truthTableStr
  
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  fomArray <- array(dim = c(I, J))
  for (i in 1:I) {
    for (j in 1:J) {
      if (design == "SPLIT-PLOT-A") {
        if (all(is.na(t[i,j,,1]))) next # if t[] for all normal   cases for selected i,j are NAs, skip 
        if (all(is.na(t[i,j,,2]))) next # if t[] for all abnormal cases for selected i,j are NAs, skip 
        k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) # see comments for SPLIT-PLOT-C
        k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K] # ditto:
        nl_ij <- NL[i, j, k1_ij_sub, ]
        perCase_ij <- dataset$lesions$perCase[k2_ij_sub]
        maxLL_ij <- max(perCase_ij)
        ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL_ij]
        k1ij <- sum(!is.na(t[i,j,,1]))
        k2ij <- sum(!is.na(t[i,j,,2]))
        lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        lW_ij <- dataset$lesions$weights[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        dim(nl_ij) <- c(k1ij+k2ij, maxNL)
        dim(ll_ij) <- c(k2ij, maxLL_ij)
        fomArray[i, j] <- MyFom_ij(nl_ij, ll_ij, perCase_ij, lID_ij, lW_ij, maxNL, maxLL_ij, k1ij, k2ij, FOM, FPFValue)
        next
      } else if (design == "SPLIT-PLOT-C") {
        if (all(is.na(t[i,j,,1]))) next # if t[] for all normal   cases for selected i,j are NAs, skip 
        if (all(is.na(t[i,j,,2]))) next # if t[] for all abnormal cases for selected i,j are NAs, skip 
        # k1 refers to normal   case k-indices
        # k2 refers to abnormal case k-indices
        k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) # k1-indices of all cases meeting the i,j criteria
        k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K] # k2-indices of all cases meeting the i,j criteria
        nl_ij <- NL[i, j, k1_ij_sub, ] # NL ratings for all cases meeting the i,j criteria
        perCase_ij <- dataset$lesions$perCase[k2_ij_sub] # perCase indices for all abnormal cases meeting the i,j criteria
        maxLL_ij <- max(perCase_ij)
        ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL_ij]
        k1ij <- sum(!is.na(t[i,j,,1]))
        k2ij <- sum(!is.na(t[i,j,,2]))
        lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        lW_jj <- dataset$lesions$weights[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        dim(nl_ij) <- c(k1ij+k2ij, maxNL)
        dim(ll_ij) <- c(k2ij, maxLL_ij)
        fomArray[i, j] <- MyFom_ij(nl_ij, ll_ij, perCase_ij, lID_ij, lW_jj, maxNL, maxLL_ij, k1ij, k2ij, FOM, FPFValue)
        next
      } else if (design == "FCTRL"){
        nl_ij <- NL[i, j, , ]
        ll_ij <- LL[i, j, , ]
        dim(nl_ij) <- c(K, maxNL)
        dim(ll_ij) <- c(K2, maxLL)
        fomArray[i, j] <- MyFom_ij(nl_ij, ll_ij, dataset$lesions$perCase, dataset$lesions$IDs, dataset$lesions$weights, maxNL, maxLL, K1, K2, FOM, FPFValue)
      } else stop("Incorrect design, must be SPLIT-PLOT-A, SPLIT-PLOT-C or FCTRL")
    }
  }
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  rownames(fomArray) <- paste("trt", sep = "", modalityID)
  colnames(fomArray) <- paste("rdr", sep = "", readerID)
  return(as.data.frame(fomArray))
  #return(data.matrix(as.data.frame(fomArray))) causes tests to fail
} 


