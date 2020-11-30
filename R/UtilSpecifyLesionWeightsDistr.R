#' Specify lesion weight distribution array
#' 
#' @description {Specify the lesion weights, the underlying assumption being that 
#'    lesion 1 is the same type across all diseased cases, lesion 2 is the same 
#'    type across all diseased cases, ..., etc. This allows assignment of weights 
#'    independent of the case index.}
#' 
#' @param specified1DLesDistr The specified lesion distribution array. For example, 
#'    c(0.1, 0.2, 0, 0.7) specifies 10 percent of diseased cases have one lesion,
#'    20 percent have two lesions, none have 3 lesions and 70 percent have 
#'    four lesions. See example below.
#'
#'   
#' @param relWeights The relative weights of the lesions; a vector of 
#'    length maxLL; if zero, the default, equal weights are assumed.
#' 
#' 
#' @return lesWghtDistr The lesion weights distribution array
#' 
#' 
#' @note This function is distinct from `UtilLesionWeightsDistr()` which gets 
#'    the lesion weights array for a dataset.
#' 
#' @examples
#' specified1DLesDistr <- c(0.1, 0.2, 0, 0.7)
#' relWeights <- c(0.2, 0.4, 0.1, 0.3)
#' UtilSpecifyLesionWeightsDistr (specified1DLesDistr, relWeights) 
#'
#' 
#' 
#' @export
#' 
UtilSpecifyLesionWeightsDistr <- function (specified1DLesDistr, relWeights = 0){
  
  if (sum(specified1DLesDistr) != 1) stop("specified1DLesDistr must sum to unity.")
  maxLL <- length(specified1DLesDistr)
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
  
  return(lesWghtDistr)
}


#' Convert the 1D lesion distribution array to the standard 2D form
#' @description {#' Specify lesion weight distribution array.}
#' 
#' @param specified1DLesDistr The specified lesion distribution array. For example, 
#'    c(0.1, 0.2, 0, 0.7) specifies 10 percent of diseased cases have one lesion,
#'    20 percent have two lesions, none have 3 lesions and 70 percent have 
#'    four lesions. See example below.
#'
#'   
#' 
#' @return lesDistr The lesion distribution array in standard 2D form
#' 
#' 
#' 
#' @export
#' 
Convert2lesDistr <- function (specified1DLesDistr) {
  
  lesDistr <- t(rbind(seq(1:length(specified1DLesDistr)), specified1DLesDistr))  
  colnames(lesDistr) <- NULL
  rownames(lesDistr) <- NULL
  return(lesDistr)
}
