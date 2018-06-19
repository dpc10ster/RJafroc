#' #' Statistical power in ROC and FROC paradigms from an ROC/FROC/LROC NH binned dataset
#' #' 
#' #' @description  Compares statistical power using ROC and FROC paradigms, 
#' #'    over a range of ROC effect sizes, from variability information obtained 
#' #'    from a null hypothesis \strong{binned} dataset, which can be in 
#' #'    ROC/FROC/LROC paradigms, 
#' #'    for J readers and K cases in the pivotal study.
#' #' 
#' #' @usage SsFROCPowerGivenJK(dataset, trts, rdrs, effectSizeROC, J, K)
#' #' 
#' #' @param dataset The pilot dataset to be analyzed, see \link{RJafroc-package},
#' #'    for variability information. The dataType can be "ROC", "FROC", or "LROC".
#' #' @param trts The indices of the modalities in the pilot dataset that
#' #'    will be regarded as representative of null hypothesis modalities. 
#' #'    Two or more modalities, specified by indices, e.g., c(1,2,3).
#' #' @param rdrs The indices of the readers  in the pilot dataset
#' #'    that will be regarded as representative of the NH readers;
#' #'    this can be used for example to exclude an atypical reader.
#' #' @param effectSizeROC Array, the range of expected ROC effect sizes
#' #'    to scan; see book Chapter 11 for guidelines, e.g., seq(0.01, 0.09, 0.005).
#' #' @param J The number of readers in the pivotal study.
#' #' @param K The number of cases in the pivotal study.
#' #' 
#' #' @return The returned list contains following items.
#' #' @return \item{powerROC}{Array, length(effectSizeROC), the statistical 
#' #'    power using ROC methodology.}  
#' #' @return \item{powerwAFROC}{Array, length(effectSizeROC), the statistical 
#' #'    power using wAFROC methodology.}
#' #' 
#' #' ## potential project for summer student
#' #' @note The pilot dataset must have at least 2 modalities; this is a
#' #'    temporary limitation to be removed in a future update
#' #'    
#' #'    
#' #' @examples 
#' #' \dontrun{
#' #' SsFROCPowerGivenJK(dataset04, trts = c(1,2), rdrs = c(1,2,3,4), 
#' #'    effectSizeROC = seq(0.01, 0.09, 0.005), J = 5, K = 200)
#' #'
#' #' ##SsFROCPowerGivenJK(datasetCadLroc, trts = 1, rdrs = seq(2,9), 
#' #' ##    effectSizeROC = seq(0.01, 0.09, 0.005), J = 5, K = 200)
#' #'    
#' #' }  
#' #' 
#' #' @references
#' #' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' #' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' #' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' #'
#' #' 
#' #' @export
#' SsFROCPowerGivenJK <- function(dataset, trts, rdrs, effectSizeROC, J, K){
#'   if (length(trts) < 2) stop("Must have at least 2 treatments")
#'   if (dataset$dataType == "LROC") {
#'     dataset <- DfLroc2Roc(dataset)
#'     dataset <- DfBinDataset(dataset, desiredNumBins = 5, opChType = "ROC")
#'   }
#'   mu <- array(dim = c(length(trts), length(rdrs)))
#'   lambda <- mu
#'   nu <- mu
#'   lesDistr <- UtilLesionDistribution(dataset)
#'   datasetRoc <- DfFroc2Roc(dataset)
#'   datasetRoc <- DfExtractDataset(datasetRoc, rdrs = rdrs)
#'   for (i in trts) {
#'     for (j in 1:length(rdrs)) {
#'       rsmRet <- FitRsmRoc(datasetRoc, trt = i, rdr = j, lesDistr = lesDistr)
#'       mu[i,j] <- rsmRet$mu
#'       nu[i,j] <- rsmRet$nu
#'       lambda[i,j] <- rsmRet$lambda
#'     }  
#'   }
#'   muMed <- median(mu) # instead of average, use median to get representative value over dataset
#'   nuMed <- median(nu) # do:
#'   lambdaMed <- median(lambda) # do:
#'   
#'   lesionWeights <- UtilLesionWeights(lesDistr)
#'   # calculate NH values for ROC-AUC and wAFROC-AUC
#'   aucRocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
#'                                               lesDistr = lesDistr, lesionWeights = lesionWeights, type = "ROC")$aucROC
#'   aucAfrocNH <- PlotRsmOperatingCharacteristics(muMed, lambdaMed, nuMed, 
#'                                                 lesDistr = lesDistr, lesionWeights = lesionWeights, type = "wAFROC")$aucwAFROC
#'   
#'   # following code calculates ROC-ES and wAFROC-ES
#'   deltaMu <- seq(0.01, 0.2, 0.01) # values of deltaMu to scan below
#'   esRoc <- array(dim = length(deltaMu));eswAfroc <- array(dim = length(deltaMu))
#'   for (i in 1:length(deltaMu)) {
#'     esRoc[i] <- PlotRsmOperatingCharacteristics(muMed + deltaMu[i], lambdaMed, nuMed, lesDistr = 
#'                                                   lesDistr, lesionWeights = lesionWeights, type = "ROC")$aucROC - aucRocNH
#'     eswAfroc[i] <- PlotRsmOperatingCharacteristics(muMed+ deltaMu[i], lambdaMed, nuMed, lesDistr = 
#'                                                      lesDistr, lesionWeights = lesionWeights, type = "wAFROC")$aucwAFROC - aucAfrocNH
#'   }
#'   
#'   a<-lm(eswAfroc~-1+esRoc) # fit values to straight line thru origin
#'   effectSizewAFROC <- effectSizeROC*a$coefficients[1]
#'   
#'   varCompROC <- StSignificanceTesting(dataset, FOM = "HrAuc", method = "DBMH", option = "RRRC")$varComp
#'   varYTRRoc <- varCompROC$varComp[3]
#'   varYTCRoc <- varCompROC$varComp[4]
#'   varYEpsRoc <- varCompROC$varComp[6]
#'   
#'   varCompwAFROC <- StSignificanceTesting(dataset, FOM = "wAFROC", method = "DBMH", option = "RRRC")$varComp
#'   varYTRwAfroc <- varCompwAFROC$varComp[3]
#'   varYTCwAfroc <- varCompwAFROC$varComp[4]
#'   varYEpswAfroc <- varCompwAFROC$varComp[6]
#'   powerROC <- rep(NA, length(effectSizeROC))
#'   powerwAFROC <- powerROC
#'   for (i in 1:length(effectSizeROC)) {
#'     powerROC[i] <- SsPowerGivenJK(J, K, alpha = 0.05, effectSize = effectSizeROC[i], option = "RRRC",
#'                                   method = "DBMH", varYTR = varYTRRoc, 
#'                                   varYTC = varYTCRoc, varYEps = varYEpsRoc)$powerRRRC
#'     powerwAFROC[i] <- SsPowerGivenJK(J, K, alpha = 0.05, effectSize = effectSizewAFROC[i], option = "RRRC",
#'                                      method = "DBMH", varYTR = varYTRwAfroc, varYTC = varYTCwAfroc, 
#'                                      varYEps = varYEpswAfroc)$powerRRRC
#'   }
#'   return(list(powerROC = powerROC,
#'               powerwAFROC = powerwAFROC))
#' }
#' 
