#' RSM fitted model for FROC sample size
#' 
#' @param dataset The \strong{pilot} dataset object representing a NH 
#'     (ROC or FROC) dataset.
#' 
#' @param lesDistr A 1D array containing the probability mass function of 
#'     number of lesions per diseased case in the \strong{pivotal FROC} 
#'     study. 
#' 
#' @return A list containing: 
#'    \itemize{ 
#'    \item \code{muMed}, the median mu parameter of the NH model. 
#'    \item \code{lambdaMed}, the median lambda parameter of the NH model.  
#'    \item \code{nuMed}, the median nu parameter of the NH model. 
#'    \item \code{scaleFactor}, the scaling factor that multiplies 
#'       the ROC effect size to get wAFROC effect size.
#'    \item \code{R2}, the R2 of the fit.
#' }   
#'   
#' @details If dataset is FROC, it is converted to an ROC dataset. The dataset 
#'     is automatically binned. The search model is used to fit each 
#'     treatment-reader combination. The median  value for each parameter is 
#'     computed and returned by the function (3 values). These are used 
#'     to compute predicted wAFROC and ROC FOMS over a range of values of deltaMu, 
#'     which are fitted by a straight line constrained to pass through the origin.
#'     The scale factor and R2 are returned. The scaling factor is the value
#'     by which the ROC effect size must be multiplied to get the wAFROC effect size. 
#'     See \url{https://dpc10ster.github.io/RJafrocQuickStart/froc-sample-size.html} 
#'     for vignettes explaining the FROC sample size estimation procedure. 
#' 
#' @examples
#'  
#' \donttest{
#' ## Examples with CPU or elapsed time > 5s
#' ## user system elapsed
#' ## SsFrocNhRsmModel 8.102  0.023   8.135
#'  
#' ## SsFrocNhRsmModel(dataset02, c(0.7, 0.2, 0.1))
#' ## the next one should match the vignette 
#' ## SsFrocNhRsmModel(DfExtractDataset(dataset04, trts = c(1,2)), c(0.69, 0.2, 0.11))
#' }
#' 
#' @importFrom stats median   
#' @export
#' 

SsFrocNhRsmModel <- function (dataset, lesDistr) {
  
  if (!(dataset$descriptions$type %in% c("ROC", "FROC"))) stop("Dataset must be ROC or FROC")
  if (dataset$descriptions$type == "FROC") rocData <- DfFroc2Roc(dataset) else rocData <- dataset
  
  # bin the dataset
  # if dataset is already binned, this does not hurt
  rocData <- DfBinDataset(rocData, opChType = "ROC")
  
  #if (sum(lesDistr) != 1.0) {
  # as per Peter's suggestion 
  # https://github.com/dpc10ster/RJafroc/issues/76#issuecomment-1189540505
  if ( !isTRUE( all.equal( sum(lesDistr), 1.0 ) ) ) { 
    errMsg <- "The lesion distribution vector must sum to unity:"
    stop(errMsg)
  }  
  
  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
  
  RsmParms <- array(dim = c(I,J,3))
  for (i in 1:I) {
    for (j in 1:J)  {
      temp <- FitRsmRoc(rocData, trt = i, rdr = j, lesDistr)
      RsmParms[i,j,1] <- temp[[1]]
      RsmParms[i,j,2] <- temp[[2]]
      RsmParms[i,j,3] <- temp[[3]]
    }
  }
  
  muMed <- median(RsmParms[,,1]) 
  lambdaPMed <- median(RsmParms[,,2]) # these are physical parameters
  nuPMed <- median(RsmParms[,,3]) # do:
  
  temp <- UtilPhysical2IntrinsicRSM(muMed, lambdaPMed, nuPMed)
  lambdaMed <- temp$lambda
  nuMed <- temp$nu

  # calculate NH values for ROC-AUC and wAFROC-AUC
  aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                              lesDistr = lesDistr, OpChType = "ROC")$aucROC
  aucAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                                lesDistr = lesDistr, OpChType = "wAFROC")$aucwAFROC
  
  # following calculates effect sizes: ROC-ES and wAFROC-ES
  deltaMu <- seq(0.01, 0.2, 0.01) # values of deltaMu to scan below
  esRoc <- array(dim = length(deltaMu));eswAfroc <- array(dim = length(deltaMu))
  for (i in 1:length(deltaMu)) {
    esRoc[i] <- PlotRsmOperatingCharacteristics(muMed + deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                  lesDistr, OpChType = "ROC")$aucROC - aucRocNH
    eswAfroc[i] <- PlotRsmOperatingCharacteristics(muMed+ deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                     lesDistr, OpChType = "wAFROC")$aucwAFROC - aucAfrocNH
  }
  
  scaleFactor<-lm(eswAfroc~-1+esRoc) # fit values to straight line thru origin
  
  # convert to intrinsic parameters
  temp <- UtilPhysical2IntrinsicRSM(muMed, lambdaPMed, nuPMed)
  lambdaMed <- temp$lambda
  nuMed <- temp$nu
  
  return(list(
    muMed = muMed,
    lambdaMed = lambdaMed,
    nuMed = nuMed,
    scaleFactor = as.numeric(scaleFactor$coefficients),
    R2 = summary(scaleFactor)$r.squared
  ))
}