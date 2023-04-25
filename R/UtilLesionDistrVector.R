#' Get the lesion distribution vector of a dataset 
#' 
#' @description The lesion distribution vector for a dataset.
#' 
#' @param dataset A dataset object. 
#'
#' @return lesDistr The 1D lesion distribution array.
#' 
#' @details Two characteristics of an FROC dataset, apart from ratings, 
#'    affect the FOM: the distribution of lesions per case and the distribution 
#'    of lesion weights. This function addresses the distribution of lesions per 
#'    case. The distribution of weights is addressed in \link{UtilLesionWeightsMatrix}. 
#'    \code{lesDistr} is a 1D-array containing the fraction of \bold{unique} 
#'    values of lesions per diseased case in the dataset. For ROC or LROC data 
#'    this vector is \code{c(1)}, since all diseased cases contain one lesion. 
#'    For FROC data the length of the vector equals the maximum number of lesions 
#'    per diseased case. The first entry is the fraction of dis. cases containing 
#'    one lesion, the second entry is the fraction of dis. cases containing two lesions, etc. 
#'    See \link{PlotRsmOperatingCharacteristics} for a function that depends on 
#'    \code{lesDistr}.
#' 
#' 
#' @examples
#' UtilLesionDistrVector(dataset01)$lesDistr # FROC dataset ## [1] 0.93258427 0.06741573
#' UtilLesionDistrVector(dataset02)$lesDistr # ROC dataset  ## 1
#' 
#' @export
 
UtilLesionDistrVector <- function(dataset)
{ 
  if (is.list(dataset) && (length(dataset) == 3)) {
    lesionNum <- dataset$lesions$perCase
    lesDistr <- table(lesionNum)
    if (length(lesDistr) == 1) {
      lesDistr <- data.frame(lesionNum = 1, Freq = 1)
    } else {
      lesDistr <- as.data.frame(lesDistr)
      lesDistr[2] <- lesDistr[2]/sum(lesDistr[2])
    }
  } else stop("Must supply dataset argument to LesionDistrVector()")
  return(list(
    lesNum = as.integer(unlist(as.vector(lesDistr[1]))),
    lesDistr = as.numeric(unlist(as.vector(lesDistr[2])))
  ))
}
