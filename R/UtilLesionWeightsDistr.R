#' Lesion weights distribution 
#' 
#' @description Computes the lesion weights  distribution, assuming equal weights. 
#'    Needed to plot empirical curves for FROC data.
#' 
#' @param dataset The supplied dataset.
#'
#' @return The lesion weights distribution, an [1:maxLL,1:maxLL] array. 
#'    The probability mass function of the 
#'    lesion weights for diseased cases. \code{maxLL} is the maximum number of lesions in
#'    the dataset. The 1st row contains the weight of the 
#'    lesion on cases with one lesion only, necessarily 1, assuming the dataset 
#'    has cases with only one lesion; the remaining elements 
#'    of the row are \code{-Inf}. The 2nd row contains the weights of the 2 lesions 
#'    on cases with 2 lesions only, the remaining elements of the row, if any, 
#'    are \code{-Inf}, assuming the dataset 
#'    has cases with two lesion. Excluding the \code{-Inf}, each row must sum to 1. 
#'    The default is equal weighting, e.g., weights are 1/3, 1/3, 1/3 on row 3, 
#'    assuming the dataset has cases with three lesions.
#'    This parameter is not to be confused with the lesionWeight list member in an FROC
#'    dataset which enumerates the weights of lesions on individual cases. See 
#'    \link{PlotRsmOperatingCharacteristics}.
#' 
#' @examples
#' UtilLesionWeightsDistr (dataset01) # FROC data
#' UtilLesionWeightsDistr (dataset02) # ROC data
#' UtilLesionWeightsDistr (datasetCadLroc) # LROC data
#' 
#' @export
UtilLesionWeightsDistr <- function(dataset)
{ 
  lesDistr <- UtilLesionDistr(dataset)
  dim1 <- max(lesDistr[,1])
  lesWghtDistr <- matrix(-Inf, nrow = dim1, ncol = dim1)
  for (row in 1:
       length(lesDistr[,1])){
    nLes <- lesDistr[row, 1]
    lesWghtDistr[nLes, 1:nLes] <- 1/nLes
  }
  return(lesWghtDistr)
}