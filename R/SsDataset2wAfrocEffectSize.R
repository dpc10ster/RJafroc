#' Computing wAFROC effect size scaling factor for an ROC or FROC dataset
#' 
#' @param dataset The \strong{pilot} dataset object, assumed to be a NH ROC 
#'     or FROC dataset; if FROC, it is converted to ROC.
#' 
#' @param lesionHistogram An array containing the fractional distribution of 
#'     number of lesions per diseased case in the \strong{pivotal} FROC study; 
#'     must sum to unity.
#' 
#' @return A list containing the linear regression coefficient for model
#'     lm(eswAfroc ~ -1 + esRoc); the scaling factor by which ROC effect size must be 
#'     multiplied to get the wAFROC effect size and the R2 of the lm() fit.
#'   
#' @details If dataset is FROC, it is converted to an ROC dataset. The search model 
#'     is used to fit each treatment-reader combination in the pilot dataset. The median 
#'     value for each parameter is computed. These are used
#'     to compute predicted wAFROC and ROC FOMS over a range of values of deltaMu, 
#'     which are fitted by a straight line constrained to pass throught the origin.
#'     The slope (scaling factor) and R2 are returned.
#' 
#' @examples 
#' SsDataset2wAfrocEffectSize(dataset02, c(0.7, 0.2, 0.1))
#' SsDataset2wAfrocEffectSize(DfExtractDataset(dataset04, trts = c(1,2)), c(0.69, 0.2, 0.11))
#'
#' @importFrom stats median   
#' @export
#' 

SsDataset2wAfrocEffectSize <- function (dataset, lesionHistogram) {
  
  if (!(dataset$dataType %in% c("ROC", "FROC"))) stop("Dataset must be ROC or FROC")
  if (dataset$dataType == "FROC") rocData <- DfFroc2Roc(dataset) else rocData <- dataset
  if (sum(lesionHistogram) != 1) stop("The lesion distribution vector must sum to unity")
  
  I <- dim(dataset$NL)[1]
  J <- dim(dataset$NL)[2]
  maxLL <- length(lesionHistogram)
  
  lesDistr <- array(c(seq(1, maxLL), lesionHistogram), dim = c(maxLL, 2))
  
  lesWghtDistr <- matrix(-Inf, nrow = nrow(lesDistr), ncol = nrow(lesDistr))
  for (l in 1:nrow(lesDistr)){
    nLes <- lesDistr[l, 1]
    lesWghtDistr[l, 1:nLes] <- 1/nLes
  }
  
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
  lambdaPMed <- median(RsmParms[,,2])
  nuPMed <- median(RsmParms[,,3])
  
  temp <- UtilPhysical2IntrinsicRSM(muMed, lambdaPMed, nuPMed)
  lambdaMed <- temp$lambda
  nuMed <- temp$nu
  
  # calculate NH values for ROC-AUC and wAFROC-AUC
  aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                              lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, type = "ROC")$aucROC
  aucAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
                                                lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, type = "wAFROC")$aucwAFROC
  
  # following calculates effect sizes: ROC-ES and wAFROC-ES
  deltaMu <- seq(0.01, 0.2, 0.01) # values of deltaMu to scan below
  esRoc <- array(dim = length(deltaMu));eswAfroc <- array(dim = length(deltaMu))
  for (i in 1:length(deltaMu)) {
    esRoc[i] <- PlotRsmOperatingCharacteristics(muMed + deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                  lesDistr, lesWghtDistr = lesWghtDistr, type = "ROC")$aucROC - aucRocNH
    eswAfroc[i] <- PlotRsmOperatingCharacteristics(muMed+ deltaMu[i], lambdaMed, nuMed, lesDistr = 
                                                     lesDistr, lesWghtDistr = lesWghtDistr, type = "wAFROC")$aucwAFROC - aucAfrocNH
  }

  slope<-lm(eswAfroc~-1+esRoc) # fit values to straight line thru origin

  return(list(
    slope = slope,
    R2 = summary(slope)$r.squared
  ))
}