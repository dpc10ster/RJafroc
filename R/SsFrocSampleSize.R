#' RSM fitted model for FROC sample size
#'
#' @param dataset The \strong{pilot} dataset.
#' 
#' @param effectSizeROC The effect size in ROC-AUC units
#'
#' @param JPivot The number of readers in the pivotal study
#'
#' @param KPivot The number of cases in the pivotal study 
#'
#' @param lesDistr A 1D array containing the probability mass function of
#'     number of lesions per diseased case in the \strong{pivotal FROC}
#'     study.
#'
#' @return A list containing:
#'    \itemize{
#'    \item \code{effectSizeROC}, the specified ROC effect size.
#'    \item \code{scaleFactor}, the factor by which the ROC effect size
#'      must by multiplied to get the wAFROC effect size.
#'    \item \code{powerRoc}, the ROC power.
#'    \item \code{powerFroc}, the wAFROC power.
#'    
#' }
#'
#' @details See \url{https://dpc10ster.github.io/RJafrocQuickStart/froc-sample-size.html}
#'     for explanation of the FROC sample size estimation procedure.
#'
#' @examples
#'
#' \donttest{
#' ## Examples with CPU or elapsed time > 5s
#' ## user system elapsed
#' ## SsFrocSampleSize 8.102  0.023   8.135
#'
#' ## SsFrocSampleSize(DfExtractDataset(dataset04, trts = c(1,2)), 
#' ## effectSizeROC = 0.03, JPivot = 5, KPivot = 100, lesDistr = c(0.69, 0.2, 0.11))
#' }
#'
#' @export
#'
SsFrocSampleSize <- function (dataset, effectSizeROC, JPivot, KPivot, lesDistr) {
  
  JStar <- length(dataset$ratings$NL[1,,1,1])
  KStar <- length(dataset$ratings$NL[1,1,,1])
  
  if (missing(lesDistr)) {
    lesDistr <- UtilLesDistr(dataset)
  } else {
    lesDistr1 <- UtilLesDistr(dataset)
    if (is.vector(lesDistr)) {
      lesDistr <- UtilLesDistr(lesDistr)
    }
    if(length(lesDistr1$Freq) != length(lesDistr$Freq))
      stop("lesDistr length must match that of dataset")
  }
  
  ret <- SsFrocNhRsmModel(DfFroc2Roc(dataset), lesDistr = lesDistr$Freq)
  muNH <- ret$mu
  lambdaNH <- ret$lambda
  nuNH <- ret$nu
  scaleFactor <- ret$scaleFactor
  effectSizewAFROC <- effectSizeROC*scaleFactor
  R2 <- ret$R2
  
  RocDatasetBin <- DfBinDataset(DfFroc2Roc(dataset), opChType = "ROC")
  varComp_roc <- UtilVarComponentsOR(
    DfFroc2Roc(dataset), 
    FOM = "Wilcoxon")$VarCom[-2]
  
  varComp_wafroc <- UtilVarComponentsOR(
    dataset, 
    FOM = "wAFROC")$VarCom[-2]
  
  # these are OR variance components assuming FOM = "Wilcoxon"
  varR_roc <- varComp_roc["VarR","Estimates"]
  varTR_roc <- varComp_roc["VarTR","Estimates"]
  Cov1_roc <- varComp_roc["Cov1","Estimates"]
  Cov2_roc <- varComp_roc["Cov2","Estimates"]
  Cov3_roc <- varComp_roc["Cov3","Estimates"]
  Var_roc <- varComp_roc["Var","Estimates"]
  
  # these are OR variance components assuming FOM = "wAFROC"
  varR_wafroc <- varComp_wafroc["VarR","Estimates"]
  varTR_wafroc <- varComp_wafroc["VarTR","Estimates"]
  Cov1_wafroc <- varComp_wafroc["Cov1","Estimates"]
  Cov2_wafroc <- varComp_wafroc["Cov2","Estimates"]
  Cov3_wafroc <- varComp_wafroc["Cov3","Estimates"]
  Var_wafroc <- varComp_wafroc["Var","Estimates"]
  
  # compute ROC power
  ret <- SsPowerGivenJK(
    dataset = NULL, 
    FOM = "Wilcoxon", 
    J = JPivot, 
    K = KPivot, 
    effectSize = effectSizeROC, 
    list(JStar = JStar, KStar = KStar, 
         VarTR = varTR_roc,
         Cov1 = Cov1_roc,
         Cov2 = Cov2_roc,
         Cov3 = Cov3_roc,
         Var = Var_roc))
  power_roc <- ret$powerRRRC
  
  # compute wAFROC power
  ret <- SsPowerGivenJK(
    dataset = NULL, 
    FOM = "wAFROC", 
    J = JPivot, 
    K = KPivot, 
    effectSize = effectSizewAFROC, 
    list(JStar = JStar, KStar = KStar, 
         VarTR = varTR_wafroc,
         Cov1 = Cov1_wafroc,
         Cov2 = Cov2_wafroc,
         Cov3 = Cov3_wafroc,
         Var = Var_wafroc))
  power_wafroc <- ret$powerRRRC
  
  return(list(
    effectSizeROC = effectSizeROC,
    scaleFactor = scaleFactor,
    powerRoc = power_roc,
    powerFroc = power_wafroc
  ))
  
}

#'
#' Build the RSM NH model for FROC sample size estimation
#'
#' @param dataset The \strong{pilot} dataset.
#' 
#' @param lesDistr A 1D array containing the probability mass function of
#'     number of lesions per diseased case in the \strong{pivotal FROC}
#'     study.
#'     
#' @return A list containing:
#'    \itemize{
#'    \item \code{mu} The RSM mu parameter of the NH model.
#'    \item \code{lambda} The RSM lambda parameter of the NH model.
#'    \item \code{nu} The RSM nu parameter of the NH model.
#'    \item \code{scaleFactor}, the factor by which the ROC effect size
#'      must by multiplied to get the wAFROC effect size.
#'    \item \code{R2} The squared correlation of the wAFROC-AUC to ROC-AUC fit.
#'    
#' }
#' 
#' @importFrom stats median
#' 
#' @export

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
  
  # use median instead of average
  muNH <- median(RsmParms[,,1])
  lambdaNH <- median(RsmParms[,,2]) # these are physical parameters
  nuNH <- median(RsmParms[,,3]) # do:
  
  # calculate NH values for ROC-AUC and wAFROC-AUC
  aucRocNH <- PlotRsmOperatingCharacteristics(muNH, lambdaNH, nuNH,
                                              lesDistr = lesDistr, OpChType = "ROC")$aucROC
  aucwAfrocNH <- PlotRsmOperatingCharacteristics(muNH, lambdaNH, nuNH,
                                                 lesDistr = lesDistr, OpChType = "wAFROC")$aucwAFROC
  
  # Calculate effect sizes: ROC and wAFROC
  deltaMu <- seq(0.01, 0.2, 0.01) # values of deltaMu to scan below
  esROC <- array(dim = length(deltaMu))
  eswAFROC <- array(dim = length(deltaMu))
  for (i in 1:length(deltaMu)) {
    # get intrinsic parameters
    par_i <- Util2Intrinsic(muNH, lambdaNH, nuNH) # intrinsic
    # find physical parameters for increased muNH
    par_p <- Util2Physical(muNH + deltaMu[i], par_i$lambda_i, par_i$nu_i)  # physical
    
    esROC[i] <- UtilAnalyticalAucsRSM(
      muNH + deltaMu[i], par_p$lambda, par_p$nu, lesDistr = lesDistr)$aucROC - aucRocNH
    
    eswAFROC[i] <- UtilAnalyticalAucsRSM(
      muNH + deltaMu[i], par_p$lambda, par_p$nu, lesDistr = lesDistr)$aucwAFROC - aucwAfrocNH
    
  }
  
  scaleFactor<-lm(eswAFROC~-1+esROC) # fit values to straight line through origin
  
  
  return(list(
    mu = muNH,
    lambda = lambdaNH,
    nu = nuNH,
    scaleFactor = as.numeric(scaleFactor$coefficients),
    R2 = summary(scaleFactor)$r.squared
  ))
}
