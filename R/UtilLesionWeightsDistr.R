#' Lesion weights distribution 
#' 
#' @description The lesion weights  distribution, assuming equal weights. 
#'    Needed to plot empirical curves for FROC data (ROC, wAFROC).
#' 
#' @param dataset The supplied dataset.
#'
#' @return Two characteristics of an FROC dataset, apart from the 
#'    ratings, affect the FOM: the distribution of lesion per case and the 
#'    distribution of lesion weights. This function addresses the weights. 
#'    The distribution of lesions is addressed in \link{UtilLesionDistr}. 
#'    \code{lesWghtDistr} is an [1:nRow,1:(maxLL+1)] array, where 
#'    \code{nRow} is the number of \bold{unique} values of lesions per case 
#'    in the dataset. The first column enumerates the number of lesions per 
#'    case, while the remaining columns contain the weights (equal weighting assumed). 
#'    Missing values are filled with \code{-Inf}. This parameter is not to be 
#'    confused with the \code{lesionWeight} list member in an FROC dataset, 
#'    which enumerates the weights of lesions on \bold{individual} cases. See 
#'    \link{PlotRsmOperatingCharacteristics} for a function that depends on 
#'    \code{lesWghtDistr}. See Chapter00Vignette2 for a fuller explanation. 
#' 
#' @examples
#' UtilLesionWeightsDistr (dataset11) # FROC data
#' UtilLesionWeightsDistr (dataset02) # ROC data
#' UtilLesionWeightsDistr (datasetCadLroc) # LROC data
#' 
#' @export
UtilLesionWeightsDistr <- function(dataset)
{ 
  lesDistr <- UtilLesionDistr(dataset)
  maxLL <- max(lesDistr[,1])
  # first column contains the number of lesions
  # remaining columns contain the weights; equal weighting assumed
  lesWghtDistr <- array(-Inf, dim = c(length(lesDistr[,1]), maxLL+1))
  lesWghtDistr[,1] <- lesDistr[,1]
  for (i in 1:length(lesDistr[,1])) lesWghtDistr[i,2:(lesDistr[i,1]+1)] <- 1/lesDistr[i,1]
  return(lesWghtDistr)
}