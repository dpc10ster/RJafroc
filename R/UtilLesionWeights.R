#' Lesion weights matrix 
#' 
#' @description Computes the lesion weights  matrix, assuming equal weights.
#' 
#' @usage UtilLesionWeights(lesDistr) 
#' 
#' @param lesDistr The supplied lesion distribution matrix
#'
#' @return The lesion weights matrix, see \link{PlotRsmOperatingCharacteristics}
#' 
#' 
#' @examples
#' UtilLesionWeights (UtilLesionDistribution(dataset01)) # FROC data
#' UtilLesionWeights (UtilLesionDistribution(dataset02)) # ROC data
#' UtilLesionWeights (UtilLesionDistribution(datasetCadLroc)) # LROC data
#' 
#' @export
UtilLesionWeights <- function(lesDistr)
{  
  lesionWeights <- matrix(-Inf, nrow = nrow(lesDistr), ncol = nrow(lesDistr))
  for (l in 1:nrow(lesDistr)){
    nLes <- lesDistr[l, 1]
    lesionWeights[l, 1:nLes] <- 1/nLes
  }
  return(lesionWeights)
}