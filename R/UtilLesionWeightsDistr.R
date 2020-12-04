#' Lesion weights distribution 
#' 
#' 
#' @description The lesion weights distribution of a dataset or the specified 
#'    lesion weights distribution as a one-dimensional array.. 
#'    
#' 
#' @param datasetOrmaxLL A dataset, e.g., \code{dataset01}, or the maximum 
#'    number of lesions in the dataset, \code{maxLL}. 
#'
#'
#' @param relWeights The relative weights of the lesions; a vector of 
#'    length equal to \code{length(maxLL)}. The default is zero, in which case
#'    equal weights are assumed.
#'
#' 
#' @return lesWghtDistr The lesion weights distribution.
#' 
#' 
#' @details Two characteristics of an FROC dataset, apart from the 
#'    ratings, affect the FOM: the distribution of lesion per case and the 
#'    distribution of lesion weights. This function addresses the weights. 
#'    The distribution of lesions is addressed in \link{UtilLesionDistr}. 
#'    \code{lesWghtDistr} is an [1:nRow,1:(maxLL+1)] array, where 
#'    \code{nRow} is the number of \bold{unique} values of lesions per case 
#'    in the dataset. The first column enumerates the number of lesions per 
#'    case, while the remaining columns contain the weights. 
#'    Missing values are filled with \code{-Inf}. This parameter is not to be 
#'    confused with the \code{lesionWeight} list member in an FROC dataset, 
#'    which enumerates the weights of lesions on \bold{individual} cases. See 
#'    \link{PlotRsmOperatingCharacteristics} for a function that depends on 
#'    \code{lesWghtDistr}. See TBA Chapter00Vignette2 for a fuller explanation.
#'    The underlying assumption is that lesion 1 is the same type across all 
#'    diseased cases, lesion 2 is the same type across all diseased cases, 
#'    ..., etc. This allows assignment of weights independent of the case index.
#'    In the third example below, `relWeights` = [0.2, 0.4, 0.1, 0.3] means that
#'    on cases with one lesion the weight of lesion 1 is unity, on cases with two 
#'    lesions the weight of the first lesion to that of the second lesion is
#'    in the ratio 0.2:0.4, i.e., lesion 2 is twice as important as lesion 1. 
#'    On cases with 4 lesions the weights are in the ratio 0.2 : 0.4 : 0.1 : 0.3.
#'    There are no cases with 3 lesions in this example. Of course, on any case
#'    the weights sum to unity.
#' 
#' @examples
#' UtilLesionWeightsDistr (dataset01) # FROC data
#' UtilLesionWeightsDistr (dataset02) # ROC data
#' 
#' maxLL <- 4
#' relWeights <- c(0.2, 0.4, 0.1, 0.3)
#' UtilLesionWeightsDistr (maxLL, relWeights) 
#' 
#' 
#' 
#' 
#' @export
UtilLesionWeightsDistr <- function(datasetOrmaxLL, relWeights = 0)
{ 
  if (is.list(datasetOrmaxLL) && (length(datasetOrmaxLL) == 3)) { 
    # a dataset has been supplied
    dataset <- datasetOrmaxLL
    lesDistr <- UtilLesionDistr(dataset)
    maxLL <- max(lesDistr[,1])
    # first column contains the number of lesions
    # remaining columns contain the weights; equal weighting assumed
    if (relWeights[1] == 0)  {
      lesWghtDistr <- array(-Inf, dim = c(length(lesDistr[,1]), maxLL+1))
      lesWghtDistr[,1] <- lesDistr[,1]
      for (i in 1:length(lesDistr[,1])) lesWghtDistr[i,2:(lesDistr[i,1]+1)] <- 1/lesDistr[i,1]
    } else {
      if (length(relWeights) != maxLL) stop("relWeights array must be of length maxLL")
      for (i in 1:maxLL) {
        lesWghtDistr[i,2:(i+1)] <- relWeights[(2:(i+1))-1] / sum(relWeights[(2:(i+1))-1])
      }
    }
  } else if (is.numeric(datasetOrmaxLL) && (length(datasetOrmaxLL) == 1)) {
    # maxLL has been supplied
    maxLL <- datasetOrmaxLL
    lesWghtDistr <- array(-Inf, dim = c(maxLL, maxLL+1))
    lesWghtDistr[,1] <- seq(1:maxLL)
    # following assures equal weights
    if (relWeights[1] == 0)  {
      for (i in 1:maxLL) {
        lesWghtDistr[i,2:(i+1)] <- 1/i
      }
    } else {
      if (length(relWeights) != maxLL) stop("relWeights array must be of length maxLL")
      for (i in 1:maxLL) {
        lesWghtDistr[i,2:(i+1)] <- relWeights[(2:(i+1))-1] / sum(relWeights[(2:(i+1))-1])
      }
    }
  } else stop("Unknown argment type in UtilLesionWeightsDistr")
  
  return(lesWghtDistr)
}