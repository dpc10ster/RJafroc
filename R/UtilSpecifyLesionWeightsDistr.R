#' Specify lesion weight distribution array
#' 
#' @description {specify the lesion weight distribution array}
#' 
#' @param maxLL The maximum number of lesions per case in the dataset.
#' 
#' @param relWeights The relative weights of the lesions; a vector of 
#'    length maxLL; if zero, the default, equal weights are assumed.
#' 
#' @return lesWghtDistr The lesion weights distribution array
#' 
#' 
#' @examples
#'  
#'
#' 
#' 
#' @export
#' 
UtilSpecifyLesionWeightsDistr <- function (maxLL, relWeights = 0){
  
  lesWghtDistr <- array(-Inf, dim = c(maxLL, maxLL+1))
  lesWghtDistr[,1] <- seq(1:maxLL)
  # following assures equal weights
  if (relWeights[1] == 0)  {
    for (i in 1:maxLL) lesWghtDistr[i,2:(lesDistr[i,1]+1)] <- 1/lesDistr[i,1]
  } else {
    if (length(relWeights) != maxLL) stop("relWeights array must be of length maxLL")
    # relWeights <- relWeights / sum(relWeights)
    for (i in 1:maxLL) {
      lesWghtDistr[i,2:(lesDistr[i,1]+1)] <- relWeights[(2:(lesDistr[i,1]+1))-1] / sum(relWeights[(2:(lesDistr[i,1]+1))-1])
    }
    # lesWghtDistr[2,2] <- 0.9
    # lesWghtDistr[2,3] <- 0.1
  }
  
  return(lesWghtDistr)
}




