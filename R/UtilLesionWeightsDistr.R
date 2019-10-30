#' Lesion weights distribution 
#' 
#' @description Computes the lesion weights  distribution, assuming equal weights.
#' 
#' @param dataset The supplied dataset.
#'
#' @return The lesion weights distribution, see \code{\link{PlotRsmOperatingCharacteristics}}
#' 
#' 
#' @examples
#' UtilLesionWeightsDistr (dataset01) # FROC data
#' UtilLesionWeightsDistr (dataset02) # ROC data
#' UtilLesionWeightsDistr (datasetCadLroc) # LROC data
#' 
#' @export
UtilLesionWeightsDistr <- function(dataset)
{ 
  lesDistr <- UtilLesionDistribution(dataset)
  lesWghtDistr <- matrix(-Inf, nrow = nrow(lesDistr), ncol = nrow(lesDistr))
  for (l in 1:nrow(lesDistr)){
    nLes <- lesDistr[l, 1]
    lesWghtDistr[l, 1:nLes] <- 1/nLes
  }
  return(lesWghtDistr)
}