#' @name UtilLesionWeightsMatrix
#' @aliases UtilLesionWeightsMatrixDataset
#' @aliases UtilLesionWeightsMatrixLesDistr
#' 
#' @title Determine lesion weights distribution 2D matrix
#' 
#' @description Determine the lesion weights distribution 2D matrix of a dataset or 
#'    manually specify the lesion weights distribution 2D matrix. 
#'    
#'
#' @param dataset A dataset object. 
#'
#' @param lesDistr A unit sum vector of length equal to the maximum number of 
#'    lesions per diseased case, specifying the relative frequency of lesions
#'    per dis. case in the dataset. For example, \code{c(0.8, 0.15, 0.05)} specifies
#'    a dataset in which 80 percent of the dis. cases have one lesion per dis. case,
#'    15 percent have two lesions per dis. case and 5 percent have three lesions per
#'    dis. case. As another example, \code{c(0.8, 0.15, 0, 0.05)} specifies
#'    a dataset in which 80 percent of the dis. cases have one lesion per dis. case,
#'    15 percent have two lesions per dis. case, there are no cases with three 
#'    lesions per dis. case and 5 percent have four lesions per dis. case. 
#'
#'
#' @param relWeights The relative weights of the lesions: a unit sum vector of 
#'    length equal to the maximum number of lesions per dis. case. For example, 
#'    \code{c(0.2, 0.4, 0.1, 0.3)} means that on cases with one lesion 
#'    the weight of the lesion is unity, on cases with two 
#'    lesions the ratio of the weight of the first lesion to that of the second 
#'    lesion is 0.2:0.4, i.e., lesion 2 is twice as important as lesion 1. 
#'    On cases with 4 lesions the weights are in the ratio 0.2:0.4:0.1:0.3.
#'    The default is zero, in which case equal lesion weights are assumed.
#'
#' @return lesWghtDistr The 2D lesion weights distribution matrix. The first 
#'    column enumerates the number of lesions per case, while the remaining 
#'    columns contain the weights. 
#'    Missing values are filled with \code{-Inf}. Not to be 
#'    confused with the \code{lesionWeight} list member in an FROC dataset, 
#'    which enumerates the weights of lesions on \bold{individual} cases. 
#' 
#' 
#' @details Two characteristics of an FROC dataset, apart from the 
#'    ratings, affect the FOM: the distribution of lesion per case and the 
#'    distribution of lesion weights. This function addresses the weights. 
#'    The distribution of lesions is addressed in \link{UtilLesionDistrVector}. 
#'    See 
#'    \link{PlotRsmOperatingCharacteristics} for a function that depends on 
#'    \code{lesWghtDistr}. 
#'    The underlying assumption is that lesion 1 is the same type across all 
#'    diseased cases, lesion 2 is the same type across all diseased cases, 
#'    ..., etc. This allows assignment of weights independent of the case index.
#' 
#' @examples
#' UtilLesionWeightsMatrixDataset (dataset01) # FROC data
#' 
#' ##      [,1] [,2] [,3]
#' ##[1,]    1  1.0 -Inf
#' ##[2,]    2  0.5  0.5
#' 
#' UtilLesionWeightsMatrixDataset (dataset02) # ROC data
#' 
#' ##      [,1] [,2]
#' ##[1,]    1  1
#' 
#' @rdname UtilLesionWeightsMatrix
#' 
#' @export
UtilLesionWeightsMatrixDataset <- function(dataset, relWeights = 0)
{ 
  if (is.list(dataset) && (length(dataset) == 3)) { 
    # a dataset has been supplied
    lesDistr <- UtilLesionDistrVector(dataset)
    maxLL <- length(lesDistr)
    if (relWeights[1] == 0) {
      relWeights <- rep(1/maxLL, maxLL)
    }
    lesWghtDistr <- UtilLesionWeightsMatrixLesDistr(lesDistr, relWeights)
  } else stop("Unknown argment type in UtilLesionWeightsMatrixDataset")
  
  return(lesWghtDistr)
}

#' @examples
#' ## Example 1: dataset with 1 to 4 lesions per case, with frequency as per first argument
#' UtilLesionWeightsMatrixLesDistr (c(0.6, 0.2, 0.1, 0.1), c(0.2, 0.4, 0.1, 0.3))
#' 
#' ##       [,1]  [,2]      [,3]      [,4]   [,5]
#' ##[1,]    1 1.0000000      -Inf      -Inf -Inf 
#' ##[2,]    2 0.3333333 0.6666667      -Inf -Inf
#' ##[3,]    3 0.2857143 0.5714286 0.1428571 -Inf
#' ##[4,]    4 0.2000000 0.4000000 0.1000000  0.3
#' 
#' ## Explanation 
#' ##> c(0.2)/sum(c(0.2))
#' ##[1] 1 ## (weights for cases with 1 lesion)
#' ##> c(0.2, 0.4)/sum(c(0.2, 0.4))
#' ##[1] 0.3333333 0.6666667 ## (weights for cases with 2 lesions)
#' ##> c(0.2, 0.4, 0.1)/sum(c(0.2, 0.4, 0.1))
#' ##[1] 0.2857143 0.5714286 0.1428571 ## (weights for cases with 3 lesions)
#' ##> c(0.2, 0.4, 0.1, 0.3)/sum(c(0.2, 0.4, 0.1, 0.3))
#' ##[1] 0.2000000 0.4000000 0.1000000  0.3 ## (weights for cases with 4 lesions)
#' 
#' 
#' ## Example2 : dataset with *no* cases with 3 lesions per case
#' UtilLesionWeightsMatrixLesDistr (c(0.1, 0.7, 0, 0.2), c(0.4, 0.3, 0.2, 0.1))
#' 
#' ##       [,1]  [,2]      [,3]    [,4]
#' ##[1,]    1 1.0000000      -Inf  -Inf
#' ##[2,]    2 0.5714286 0.4285714  -Inf
#' ##[3,]    4 0.5000000 0.3750000 0.125
#' 
#' ## Explanation: note that row with 3 lesions per case does not occur 
#' ##> c(0.4)/sum(c(0.4))
#' ##[1] 1 ## (weights for cases with 1 lesion)
#' ##> c(0.4, 0.3)/sum(c(0.4, 0.3))
#' ##[1] 0.5714286 0.4285714 ## (weights for cases with 2 lesions)
#' ##> c(0.4, 0.3, 0.1)/sum(c(0.4, 0.3, 0.1))
#' ##[1] 0.500 0.375 0.125 ## (weights for cases with 4 lesions)
#' 
#' @rdname UtilLesionWeightsMatrix
#' 
#' @export
UtilLesionWeightsMatrixLesDistr <- function(lesDistr, relWeights = 0)
{ 
  if (length(which(lesDistr != 0)) == 0) stop("lesDistr cannot have all zeroes")
  
  maxLL <- length(lesDistr)
  if (relWeights[1] == 0) {
    relWeights <- rep(1/maxLL, maxLL)
  }
  
  if (maxLL == 1) {
    if ((relWeights[1] != 0) && (relWeights[1] != 1))
      stop("Incorrect value for relWeights: should be 0 or 1")
    lesWghtDistr <- array(1, dim = c(1,2)) 
    return(lesWghtDistr)
  }
  # maxLL > 1
  
  if ((sum(lesDistr) != 1) || (sum(relWeights) != 1)) stop("lesDistr and relWeights arrays must each sum to unity")
  
  if (length(which(lesDistr != 0)) < length(lesDistr)) {
    lesWghtDistr <- array(-Inf, dim = c(length(which(lesDistr != 0)), length(which(lesDistr != 0))+1))
    lesWghtDistr[,1] <- which(lesDistr != 0)
  } else {
    lesWghtDistr <- array(-Inf, dim = c(maxLL, maxLL+1))
    lesWghtDistr[,1] <- seq(1:maxLL)
  }
  
  # following assures equal weights
  if (relWeights[1] == 0)  {
    for (i in 1:maxLL) {
      lesWghtDistr[i,2:(i+1)] <- 1/i
    }
  } else {
    #if (length(relWeights) != maxLL) stop("relWeights array must be of length maxLL")
    relWeightsIndx <- relWeights[which(lesDistr != 0)]
    for (i in 1:nrow(lesWghtDistr)) {
      lesWghtDistr[i,2:(i+1)] <- relWeightsIndx[(2:(i+1))-1] / sum(relWeightsIndx[(2:(i+1))-1])
    }
  }
  
  return(lesWghtDistr)
}