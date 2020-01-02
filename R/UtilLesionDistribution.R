#' Lesion distribution  matrix 
#' 
#' @description Extracts the lesion distribution matrix for a dataset.
#' 
#' @param dataset The supplied dataset
#'
#' @return The lesion distribution matrix
#' 
#' @details The lesion distribution matrix has Lmax rows and two columns. 
#'    The first column contains the integers 1, 2, ..., Lmax and the second 
#'    column contains the fraction of diseased cases with the number of lesions 
#'    per case specified in the first column.
#' 
#' 
#' @examples
#' UtilLesionDistribution (dataset01) # FROC data
#' UtilLesionDistribution (dataset02) # ROC data
#' UtilLesionDistribution (datasetCadLroc) # LROC data
#' 
#' @export
UtilLesionDistribution <- function(dataset)
{  
  lesionVector <- dataset$lesionVector
  nLes <- sort(unique(lesionVector))
  lesDistr1 <- table(lesionVector)
  lesDistr <- matrix(-Inf, nrow = max(lesionVector), ncol = 2)
  for (i in nLes) {
    lesDistr[i,1] <- attributes(lesDistr1)
  }
  if (length(lesDistr) == 1) {
    lesDistr <- c(lesionVector[1], 1)
    dim(lesDistr) <- c(1, 2)
  }else{
    lesDistr <- t(rbind(as.numeric(unlist(attr(lesDistr, "dimnames"))), as.vector(lesDistr)))
  }
  lesDistr[,2] <- lesDistr[,2]/sum(lesDistr[,2])
  return(lesDistr)
}