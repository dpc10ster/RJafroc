#' Lesion distribution  matrix 
#' 
#' @description The lesion distribution matrix for a dataset.
#' 
#' @param dataset The dataset
#'
#' @return The lesion distribution matrix
#' 
#' @details lesDistr The lesion distribution matrix, an [1:nRow,2] array, where
#'    nRow is the number of \bold{unique} values of lesions per case 
#'    in the dataset. The first column contains the number of lesions. 
#'    The second column contains the fraction of diseased cases with the number 
#'    of lesions indicated in the first column. See 
#'    \link{PlotRsmOperatingCharacteristics} for a function that depends on 
#'    lesDistr. See Chapter00Vignette2 for more details.
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
  lesionNum <- dataset$lesionVector
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
