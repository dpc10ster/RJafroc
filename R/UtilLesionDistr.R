#' Lesion distribution in the dataset 
#' 
#' @description The lesion distribution for a dataset.
#' 
#' @param dataset The dataset
#'
#' @return The lesion distribution array
#' 
#' @details Two characteristics of an FROC dataset, apart from the 
#'    ratings, affect the FOM: the distribution of lesion per case and the 
#'    distribution of lesion weights. This function addresses the first. 
#'    The weights are addressed by \link{UtilLesionWeightsDistr}. \code{lesDistr} 
#'    is an [1:nRow,2] array, where \code{nRow} is the number of \bold{unique} 
#'    values of lesions per case in the dataset. The first column of the array contains 
#'    the number of lesions per case. The second column contains the corresponding 
#'    fraction of diseased cases. See \link{PlotRsmOperatingCharacteristics} for a 
#'    function that depends on \code{lesDistr}. See Chapter00Vignette2 for more details.
#' 
#' 
#' @examples
#' UtilLesionDistr (dataset01) # FROC data
#' UtilLesionDistr (dataset02) # ROC data
#' UtilLesionDistr (datasetCadLroc) # LROC data
#' 
#' @export 
UtilLesionDistr <- function(dataset)
{  
  lesionNum <- dataset$lesions$perCase
  lesDistr <- table(lesionNum)
  if (length(lesDistr) == 1) {
    lesDistr <- c(lesionNum[1], 1)
    dim(lesDistr) <- c(1, 2)
  }else{
    lesDistr <- t(rbind(as.numeric(unlist(attr(lesDistr, "dimnames"))), as.vector(lesDistr)))
  }
  lesDistr[,2] <- lesDistr[,2]/sum(lesDistr[,2])
  return(lesDistr)
}
