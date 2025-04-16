#' @name UtilLesWghts
#' @aliases UtilLesWghtsDS
#' @aliases UtilLesWghtsLD
#' 
#' @title Lesion weights distribution matrix
#' 
#' @description Determine the lesion weights distribution 2D matrix of a dataset or 
#'    manually specify the lesion weights distribution. 
#'    
#'
#' @param dsOrArr A dataset object or a 1D-array, see \link{UtilLesDistr}. 
#'
#' @param LDOrArr The lesion distribution (LD) dataframe object produced by 
#'    \link{UtilLesDistr} or a 1D-array. 
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
#' @return The lesion distribution (LD) dataframe object produced by 
#'    \link{UtilLesDistr} or a 1D-array. 
#' 
#' 
#' @details Two characteristics of an FROC dataset, apart from the 
#'    ratings, affect the FOM: the distribution of lesion per case and the 
#'    distribution of lesion weights. This function addresses the weights. 
#'    The distribution of lesions is addressed in \link{UtilLesDistr}. 
#'    See 
#'    \link{PlotRsmOpChrs} for a function that depends on 
#'    \code{lesWghtDistr}. 
#'    The underlying assumption is that lesion 1 is the same type across all 
#'    diseased cases, lesion 2 is the same type across all diseased cases, 
#'    ..., etc. This allows assignment of weights independent of the case index.
#' 
#' @examples
#' UtilLesWghtsDS (dataset01) # FROC data
#' 
#' ##      [,1] [,2] [,3]
#' ##[1,]    1  1.0 -Inf
#' ##[2,]    2  0.5  0.5
#' 
#' UtilLesWghtsDS (dataset02) # ROC data
#' 
#' ##      [,1] [,2]
#' ##[1,]    1  1
#' 
#' UtilLesWghtsDS(c(0.7,0.2,0.1)) # only frequencies supplied
#' ## relWeights defaults to zero
#' 
#' @export
UtilLesWghtsDS <- function(dsOrArr, relWeights = 0)
{ 

  if ((is.list(dsOrArr) && (length(dsOrArr) == 3)) || is.vector(dsOrArr)) { 
    # a dataset or vector object has been supplied
    LD <- UtilLesDistr(dsOrArr)
    maxLL <- max(LD$lesID)
    if (relWeights[1] == 0) {
      relWeights <- rep(1/maxLL, maxLL)
    }
    lesWghtDistr <- UtilLesWghtsLD(LD, relWeights)
  } else stop("Unknown argment type in UtilLesWghtsDS")
  
  return(lesWghtDistr)
}

#' @examples
#' ## Dataset with 1 to 4 lesions per case, with frequency as per first argument
#' 
#' UtilLesWghtsLD (c(0.6, 0.2, 0.1, 0.1), c(0.2, 0.4, 0.1, 0.3))
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
#' UtilLesWghtsLD (c(0.1, 0.7, 0.0, 0.2), c(0.4, 0.3, 0.2, 0.1)) 
#' 
#' ## Weights are included for non-existent `lesionID` = 3 but corresponding frequency will be zero
#' ##      [,1]       [,2]       [,3]       [,4] [,5]
#' ## [1,]    1 1.00000000       -Inf       -Inf -Inf
#' ## [2,]    2 0.57142857 0.42857143       -Inf -Inf
#' ## [3,]    3 0.44444444 0.33333333 0.22222222 -Inf
#' ## [4,]    4 0.40000000 0.30000000 0.20000000  0.1
#' 
#' 
#' UtilLesWghtsDS(dataset05, relWeights = c(0.78723404, 0.17021277, 0.04255319))
#' ##      [,1]       [,2]       [,3]       [,4]
#' ## [1,]    1 1.00000000       -Inf       -Inf
#' ## [2,]    2 0.82222222 0.17777778       -Inf
#' ## [3,]    3 0.78723404 0.17021277 0.04255319
#' 
#' 
#' @rdname UtilLesWghts
#' 
#' @export
UtilLesWghtsLD <- function(LDOrArr, relWeights = 0)
{ 
  if (is.vector(LDOrArr)) LD <- UtilLesDistr(LDOrArr) else LD <- LDOrArr
  
  if (!isTRUE( all.equal( sum(LD$Freq), 1.0 )))
    stop("lesDistr array must sum to unity")
  
  maxLL <- max(LD$lesID)
  if (maxLL == 1) {
    relWeights <- 1
    lesWghtDistr <- array(1, dim = c(1,2)) 
    return(lesWghtDistr)
  }
  
  if (missing(relWeights) || (length(relWeights) == 1) || (length(relWeights) != maxLL)) {
    relWeights <- rep(1/maxLL, maxLL)
  }

  if (!isTRUE( all.equal( sum(relWeights), 1.0 )))
    stop("relWeights arrays must sum to unity")

  lesWghtDistr <- array(-Inf, dim = c(maxLL, maxLL+1))
  
  i1 <- 1
  for (i in 1:maxLL) {
    if (length(which(LD$lesID == i)) == 1) {
      lesWghtDistr[i1,1] <- i
      lesWghtDistr[i1,2:(i+1)] <- relWeights[1:i] / sum(relWeights[1:i])
      i1 <- i1 + 1
    }
  }
  
  # lesWghtDistr1 <- lesWghtDistr
  # for (i in maxLL:1) {
  #   if (!any(is.finite(lesWghtDistr[i,]))) {
  #     lesWghtDistr1 <- lesWghtDistr1[-i,]
  #   }
  # }
  
  return(lesWghtDistr)
}