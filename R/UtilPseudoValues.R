#' Calculate pseudovalues
#' 
#' Calculates \strong{centered} jackknife pseudovalues AND jackknife FOM values, 
#'    for factorial, split-plot-a and split-plot-c study designs
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}};
#'    must be factorial, or split-plot-a and split-plot-c.
#' @param FOM The figure of merit to be used in the calculation. 
#'    The default is \code{"FOM_wAFROC"}. See \code{\link{UtilFigureOfMerit}}.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list containing two \code{c(I, J, K)} arrays containing the pseudovalues
#'    and the jackknife FOM values of the datasets (a third returned value 
#'    is for internal use)
#' 
#' @examples
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")$jkPseudoValues[1,1,1:10]
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")$jkFomValues[1,1,1:10]
#' UtilPseudoValues(dataset05, FOM = "wAFROC")$jkPseudoValues[1,1,1:10]
#' UtilPseudoValues(dataset05, FOM = "wAFROC")$jkFomValues[1,1,1:10]
#' 
#' @export

# UtilPseudoValues.R had errors (prior to v1.3.1) insofar as it was 
# dropping the 1 dimension in 
# IDs and weights; was affecting StSingleModality when used with 
# wAFROC FOM. This part of the code needs further checking; 
# no essential changes made in MyFOM.cpp and MyFom_ij.R.
# v.1.3.1.9000: added SPLIT-PLOT-C capability
# 
UtilPseudoValues <- function(dataset, FOM, FPFValue = 0.2) {
  dataType <- dataset$descriptions$type
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

  if (dataset$descriptions$design %in% c("FCTRL", "SPLIT-PLOT-A", "SPLIT-PLOT-C")) {
    
    ret <- PseudoValues(dataset, FOM, FPFValue)
    
  } else stop("Unrecognized study design: should be FCTRL, SPLIT-PLOT-A or SPLIT-PLOT-C")
  
  # ret <- (list(
  #   jkPseudoValues = jkPseudoValues, 
  #   jkFomValues = jkFomValues,
  #   caseTransitions = NULL
  # ))
  # 
  return(ret)

}

