#' Significance testing: standalone CAD vs. radiologists
#' 
#' @description  Comparing standalone CAD vs. a group of radiologists interpreting 
#'    the same cases; (ideally) \strong{standalone CAD} means that all the 
#'    \bold{designer-level} mark-rating pairs provided by the CAD algorithm 
#'    are available, not just the one or two marks usually displayed to the 
#'    radiologist. At the very minimum, location-level information, such as in
#'    the LROC paradigm, should be used. Ideally the FROC paradigm should be used.
#'    A severe statistical power penalty is paid if one uses the ROC paradigm. 
#'    Details of the method are in a pdf file that will be uploaded to GitHub and 
#'    in my 2017 book.
#'  
#' @param dataset \strong{The dataset to be analzed; must be single-treatment  
#'    multiple-readers, where the first reader is CAD.}
#' @param FOM The desired FOM; for ROC data it must be \code{"Wilcoxon"}, for FROC data 
#'    it can be any valid FOM, e.g., \code{"HrAuc"}, \code{"wAFROC"}, etc; 
#'    for LROC data it must be \code{"Wilcoxon"}, or \code{"PCL"} or \code{"ALROC"}. 
#' @param method The desired analysis: "1T-RRFC","1T-RRRC" (the default) or "2T-RRRC",
#'    see manuscript for details. \strong{Note: method "2T-RRRC" is not implemented 
#'    for non-LROC data. It is an obsolete method, included here only to show that
#'    it gives identical summary statistics, e.g., p-value, as "1T-RRRC".}
#' @param alpha Significance level of the test, defaults to 0.05.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' @param plots Flag, default is FALSE, i.e., a plot is not displayed. 
#'    If TRUE, it displays the appropriate operating characteristic for all 
#'    readers and CAD.
#'
#' 
#' @details
#' \itemize{
#'    \item{\strong{PCL} is the probability of a correct localization.} 
#'    \item{The LROC is the plot of PCL (ordinate) vs. FPF.} 
#'    \item{For LROC data, FOM = "PCL" means the interpolated PCL value 
#'    at the specified \code{FPFValue}.}
#'    \item{For FOM = "ALROC" the trapezoidal area under the LROC
#'    from FPF = 0 to FPF = \code{FPFValue} is used.} 
#'    \item{If \code{method = "1T-RRRC"} the first \strong{reader} is assumed to be CAD.} 
#'    \item{If \code{method = "2T-RRRC"} the first \strong{treatment} is assumed to be CAD.} 
#'    \item{The NH is that the FOM of CAD equals the average of the readers.} 
#'    \item{The \code{method = "1T-RRRC"} analysis uses an adaptation of the 
#'    single-treatment multiple-reader Obuchowski Rockette (OR) model described in a 
#'    paper by Hillis (2007), section 5.3. It is characterized by 3 parameters
#'    \code{VarR}, \code{Var} and \code{Cov2}, where the latter two are estimated 
#'    using the jackknife.} 
#'    \item{For \code{method = "2T-RRRC"} the analysis replicates the CAD data as many times as
#'    necessary so as to form one "treatment" of an MRMC pairing, the other 
#'    "treatment" being the radiologists. Then standard ORH analysis is applied. The 
#'    method is described in Kooi et al. It gives exactly the same final results 
#'    (F-statistic, ddf and p-value) as \code{"1T-RRRC"} but the intermediate quantities 
#'    are meaningless.}
#'    }
#' 
#' @return If \code{method = "1T-RRRC"} the return value is a 
#'    list with the following elements: 
#' @return \item{fomCAD}{The observed FOM for CAD.} 
#' @return \item{fomRAD}{The observed FOM array for the readers.} 
#' @return \item{avgRadFom}{The average FOM of the readers.} 
#' @return \item{avgDiffFom}{The mean of the difference FOM, RAD - CAD.} 
#' @return \item{ciAvgDiffFom}{The 95-percent CI of the average difference, RAD - CAD.}
#' @return \item{varR}{The variance of the radiologists.}
#' @return \item{varError}{The variance of the error term in the single-treatment 
#'    multiple-reader OR model.} 
#' @return \item{cov2}{The covariance of the error term.}
#' @return \item{tstat}{The observed value of the t-statistic; it's square is 
#'    equivalent to an F-statistic.} 
#' @return \item{df}{The degrees of freedom of the t-statistic.} 
#' @return \item{pval}{The p-value for rejecting the NH.} 
#' @return \item{Plots}{If argument plots = TRUE, a \pkg{ggplot} object 
#'    containing empirical operating characteristics  
#'    corresponding to specified FOM. For example, if \code{FOM} = 
#'    \code{"Wilcoxon"} an ROC plot object
#'    is produced where reader 1 is CAD. If an LROC FOM is selected, an LROC
#'    plot is displayed.} 
#'
#'
#' @return If \code{method = "2T-RRRC"} the return value is a list 
#'    with the following elements: 
#' @return \item{fomCAD}{The observed FOM for CAD.} 
#' @return \item{fomRAD}{The observed FOM array for the readers.} 
#' @return \item{avgRadFom}{The average FOM of the readers.} 
#' @return \item{avgDiffFom}{The mean of the difference FOM, RAD - CAD.} 
#' @return \item{ciDiffFom}{A data frame containing the statistics associated 
#'    with the average difference, RAD - CAD.}
#' @return \item{ciAvgRdrEachTrt}{A data frame containing the statistics 
#'    associated with the average FOM in each "treatment".}
#' @return \item{varR}{The variance of the pure reader term in the OR model.}
#' @return \item{varTR}{The variance of the treatment-reader term error 
#'    term in the OR model.} 
#' @return \item{cov1}{The covariance1 of the error term - same reader, 
#'    different treatments.}
#' @return \item{cov2}{The covariance2 of the error term  - 
#'    different readers, same treatment.}
#' @return \item{cov3}{The covariance3 of the error term  - different readers, 
#'    different treatments.}
#' @return \item{varError}{The variance of the pure error term in the OR model.}
#' @return \item{FStat}{The observed value of the F-statistic.} 
#' @return \item{ndf}{The numerator degrees of freedom of the F-statistic.} 
#' @return \item{ddf}{The denominator degrees of freedom of the F-statistic.} 
#' @return \item{pval}{The p-value for rejecting the NH.} 
#' @return \item{Plots}{see above.} 
#' 
#' 
#' 
#' @examples 
#' ret1M <- StSignificanceTestingCadVsRadiologists (dataset09, 
#' FOM = "Wilcoxon", method = "1T-RRRC")
#' 
#' \dontrun{
#' ## takes longer than 5 sec on OSX
#' retLroc1M <- StSignificanceTestingCadVsRadiologists (datasetCadLroc, 
#' FOM = "PCL", method = "1T-RRRC", FPFValue = 0.05)
#' 
#' retLroc2M <- StSignificanceTestingCadVsRadiologists (datasetCadLroc, 
#' FOM = "PCL", method = "2T-RRRC", FPFValue = 0.05)
#' 
#' ## test with fewer readers
#' dataset09a <- DfExtractDataset(dataset09, rdrs = seq(1:7))
#' ret1M7 <- StSignificanceTestingCadVsRadiologists (dataset09a, 
#' FOM = "Wilcoxon", method = "1T-RRRC")
#' 
#' datasetCadLroc7 <- DfExtractDataset(datasetCadLroc, rdrs = seq(1:7))
#' ret1MLroc7 <- StSignificanceTestingCadVsRadiologists (datasetCadLroc7, 
#' FOM = "PCL", method = "1T-RRRC", FPFValue = 0.05)
#' 
#' ret2MLroc7 <- StSignificanceTestingCadVsRadiologists (datasetCadLroc7, 
#' FOM = "PCL", method = "2T-RRRC", FPFValue = 0.05)
#' }
#' 
#' @references
#' Hillis SL (2007) A comparison of denominator degrees of freedom methods 
#' for multiple observer ROC studies, Statistics in Medicine. 26:596-619.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' Hupse R, Samulski M, Lobbes M, et al (2013) Standalone computer-aided detection compared to radiologists 
#' performance for the detection of mammographic masses, Eur Radiol. 23(1):93-100.
#' 
#' Kooi T, Gubern-Merida A, et al. (2016) A comparison between a deep convolutional 
#' neural network and radiologists for classifying regions of interest in mammography. 
#' Paper presented at: International Workshop on Digital Mammography, Malmo, Sweden.
#' 
#' 
#' @import ggplot2
#' @importFrom stats var
#' @export
StSignificanceTestingCadVsRadiologists <- function(dataset, FOM, FPFValue = 0.2, method = "1T-RRRC", 
                                                   alpha = 0.05, plots = FALSE) 
{
  if (length(dataset$NL[,1,1,1]) != 1) stop("dataset has to be single-treatment multiple-readers with CAD as the first reader")
  if ((dataset$dataType == "ROC") && (FOM %in% c("PCL", "ALROC"))) stop("Cannot use LROC FOM with ROC data")
  
  if (method == "1T-RRFC") {
    ret <- SingleModalityRRFC(dataset, FOM, FPFValue, alpha)
  } else if (method == "1T-RRRC") {  
    ret <- SingleModalityRRRC(dataset, FOM, FPFValue, alpha)
  } else if (method == "2T-RRRC") {
    ret <- DualModalityRRRC (dataset, FOM, FPFValue, alpha)
  } else stop("incorrect method specified")
  
  if ((dataset$dataType != "LROC") && (method == "2T-RRRC")) {
    stop("2T-RRRC for non LROC data (not implemented) is unnecessary as 1T-method should be used instead.")
  }
  
  if (plots) {
    genericPlot <- CadVsRadPlots (dataset, FOM)
    retNames <- names(ret)
    retNames <- c(retNames, "Plots")
    len <- length(ret)
    ret1 <- vector("list", len+1)
    for (i in 1:len){
      ret1[[i]] <- ret[[i]]
    }
    ret1[[len+1]] <- genericPlot
    names(ret1) <- retNames
  } else ret1 <- ret
  
  return(ret1)
  
}



# Handles all dataTypes
SingleModalityRRFC <- function(dataset, FOM, FPFValue, alpha){
  thetajc <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  Psijc <- thetajc[-1] - thetajc[1]
  ret <- t.test(Psijc, conf.level = 1-alpha)
  Tstat <-  as.numeric(ret$statistic)
  df <-  as.numeric(ret$parameter)
  pval <- ret$p.value
  CIAvgRadFom <- as.numeric(ret$conf.int)+thetajc[1]
  avgDiffFom <- mean(Psijc)
  CIAvgDiffFom <- as.numeric(ret$conf.int)
  return (list (
    fomCAD = thetajc[1],
    fomRAD = thetajc[-1],
    avgRadFom = mean(thetajc[-1]),
    CIAvgRadFom = CIAvgRadFom,
    avgDiffFom = avgDiffFom,
    CIAvgDiffFom = CIAvgDiffFom,
    varR = var(thetajc[-1]),
    Tstat = Tstat,
    df = df,
    pval = pval
  ))
}



# Formerly
# Anal2007Hillis53
# Anal2007Hillis53
# Anal2007Hillis53
# Handles all datasets
SingleModalityRRRC <- function (dataset, FOM, FPFValue, alpha)
{  

  ret <- DiffFomVarCov2(dataset, FOM, FPFValue) # VarCov2 (subtract first reader FOMs before getting covariance)
  varError <- ret$var;  Cov2 <- ret$cov2
  
  J <- length(dataset$NL[1,,1,1]) - 1 # number of radiologists minus CAD reader
  thetajc <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  Psijc <- thetajc[2:(J+1)] - thetajc[1] # subract CAD from RAD, my Eqn. 13
  
  MSR <- 0 # 1st un-numbered equation on page 607
  avgDiffFom <- mean(Psijc)
  for (j in 1:J){
    MSR <- MSR + (Psijc[j] - avgDiffFom)^2
  }
  MSR <- MSR / (J - 1)
  
  # Compared to equations in 2013 Hillis paper, in paragraph following Table I
  # OK; 10/14/19
  MSdenOR_single <- MSR + max(J * Cov2, 0) #  # 2nd un-numbered equation on page 607
  DdfhSingle <- MSdenOR_single^2 / (MSR^2 / (J - 1))  # 3rd un-numbered equation on page 607
  TstatStar <- avgDiffFom / sqrt(MSdenOR_single/J) # in-text equation on line 1 on page 607
  # BUT with theta0 = 0
  pval <- 1 - pt(abs(TstatStar),DdfhSingle) + pt(-abs(TstatStar),DdfhSingle) # two tailed probability
  
  CIAvgDiffFom <- array(dim=2)
  CIAvgDiffFom[1] <- qt(alpha/2,df = DdfhSingle)  # Equation 25 on page 607
  CIAvgDiffFom[2] <- qt(1-alpha/2,df = DdfhSingle)
  CIAvgDiffFom <- CIAvgDiffFom * sqrt(MSdenOR_single/J)
  CIAvgDiffFom <- CIAvgDiffFom + avgDiffFom
  CIAvgRad <- mean(thetajc[2:(J+1)]) + CIAvgDiffFom - mean(Psijc)
  return (list (
    fomCAD = thetajc[1],
    fomRAD = thetajc[-1],
    avgRadFom = mean(thetajc[-1]),
    CIAvgRad = CIAvgRad,
    avgDiffFom = avgDiffFom,
    CIAvgDiffFom = CIAvgDiffFom,
    varR = var(as.numeric(thetajc[-1])),
    varError = varError, 
    cov2 = Cov2, 
    Tstat = TstatStar,
    df = DdfhSingle,
    pval = pval
  ))
}



DualModalityRRRC <- function(dataset, FOM, FPFValue, alpha)
{
  K <- length(dataset$NL[1,1,,1])
  dataType <- dataset$dataType
  if ((dataType == "LROC") && (FOM %in% c("PCL", "ALROC"))) 
  {
    ret1 <- dataset2ratings(dataset, FOM)
    TP <- ret1$zjk2
    zjk2Il <- ret1$zjk2Il
    K2 <- length(TP[1,])
    K1 <- K - K2
    FP <- ret1$zjk1[,1:K1]
    J <- length(FP[,1]) - 1
    combinedNL <- array(-Inf, dim=c(2,J,K,1))
    for (j in 1:J){
      combinedNL[1,j,1:K1,1] <- FP[1,]
    }
    combinedNL[2,,1:K1,1] <- FP[2:(J+1),]
    
    combinedLLCl <- array(-Inf, dim=c(2,J,K2,1))
    
    for (j in 1:J){
      combinedLLCl[1,j,,1] <- TP[1,]
    }
    combinedLLCl[2,,,1] <- TP[2:(J+1),]
    
    combinedLLIl <- array(-Inf, dim=c(2,J,K2,1))
    for (j in 1:J){
      combinedLLIl[1,j,,1] <- zjk2Il[1,]
    }
    combinedLLIl[2,,,1] <- zjk2Il[2:(J+1),]
  } else if ((dataType == "LROC") && (FOM == "Wilcoxon")) {
    datasetRoc <- DfLroc2Roc(dataset)
    dataType <- datasetRoc$dataType
    NL <- datasetRoc$NL
    LL <- datasetRoc$LL
    K <- length(NL[1,1,,1])
    K2 <- length(LL[1,1,,1])
    K1 <- K - K2
    FP <- NL[1,,1:K1,1]
    TP <- LL[1,,,1]
    J <- length(FP[,1]) - 1
    combinedNL <- array(-Inf, dim=c(2,J,K,1))
    for (j in 1:J){
      combinedNL[1,j,1:K1,1] <- FP[1,]
    }
    combinedNL[2,,1:K1,1] <- FP[2:(J+1),]
    
    combinedLLCl <- array(-Inf, dim=c(2,J,K2,1))
    for (j in 1:J){
      combinedLLCl[1,j,,1] <- TP[1,]
    }
    combinedLLCl[2,,,1] <- TP[2:(J+1),]
  } else if ((dataType == "ROC") && (FOM == "Wilcoxon")) {
    NL <- dataset$NL
    LL <- dataset$LL
    K <- length(NL[1,1,,1])
    K2 <- length(LL[1,1,,1])
    K1 <- K - K2
    FP <- NL[1,,1:K1,1]
    TP <- LL[1,,,1]
    J <- length(FP[,1]) - 1
    combinedNL <- array(-Inf, dim=c(2,J,K,1))
    for (j in 1:J){
      combinedNL[1,j,1:K1,1] <- FP[1,]
    }
    combinedNL[2,,1:K1,1] <- FP[2:(J+1),]
    
    combinedLLCl <- array(-Inf, dim=c(2,J,K2,1))
    for (j in 1:J){
      combinedLLCl[1,j,,1] <- TP[1,]
    }
    combinedLLCl[2,,,1] <- TP[2:(J+1),]
  } else stop("Incorrect FOM with LROC data")
  
  lesionID <- rep(1, K2)
  dim(lesionID) <- c(K2,1)
  lesionWeight <- rep(0, K2)
  dim(lesionWeight) <- c(K2,1)
  if (dataType == "LROC") {
    datasetCombined <- list(
      NL = combinedNL,
      LLCl = combinedLLCl,
      LLIl = combinedLLIl,
      lesionVector = rep(1,K2),
      lesionID = lesionID,
      lesionWeight = lesionWeight,
      dataType = dataType,
      modalityID = as.character(c(1,2)),
      readerID = as.character(1:J)
    )
  } else {
    datasetCombined <- list(
      NL = combinedNL,
      LL = combinedLLCl,
      lesionVector = rep(1,K2),
      lesionID = lesionID,
      lesionWeight = lesionWeight,
      dataType = dataType,
      modalityID = as.character(c(1,2)),
      readerID = as.character(1:J)
    )
  }
  
  stats1 <- StSignificanceTesting(datasetCombined, FOM = FOM, method = "ORH", alpha = alpha, option = "RRRC", FPFValue = FPFValue)
  thetajc <- stats1$fomArray
  fomCAD  <-  thetajc[1,1]
  fomRAD  <-  thetajc[2,]
  avgRadFom <-  mean(fomRAD)
  varDiffFom <- var(fomRAD)
  avgDiffFom <-  avgRadFom - fomCAD
  FStat <-  stats1$FTestStatsRRRC$fRRRC
  ddf <-  stats1$FTestStatsRRRC$ddfRRRC
  ndf <- stats1$FTestStatsRRRC$ndfRRRC
  pval <-  stats1$FTestStatsRRRC$pRRRC
  varR <- stats1$varComp$varR
  varTR <- stats1$varComp$varTR
  varError <- stats1$varComp$var
  cov1 <- stats1$varComp$cov1
  cov2 <- stats1$varComp$cov2
  cov3 <- stats1$varComp$cov3
  ciDiffFom <- stats1$ciDiffTrtRRRC
  ciAvgRdrEachTrt <- stats1$ciAvgRdrEachTrtRRRC
  
  
  return (list (
    fomCAD = fomCAD,
    fomRAD = fomRAD,
    avgRadFom = avgRadFom,
    avgDiffFom = avgDiffFom,
    varDiffFom = varDiffFom,
    ciDiffFom = ciDiffFom,
    ciAvgRdrEachTrt = ciAvgRdrEachTrt,
    varR = varR,
    varTR = varTR,
    cov1 = cov1,
    cov2 = cov2,
    cov3 = cov3,
    varError = varError,
    FStat = FStat,
    ndf = ndf,
    ddf = ddf,
    pval = pval
  ))
  
}



CadVsRadPlots <- function(dataset, FOM) {
  ret1 <- dataset2ratings(dataset, FOM)
  zjk1 <- ret1$zjk1
  zjk2 <- ret1$zjk2
  
  dataType <- dataset$dataType
  if (dataType == "ROC") {
    # fixed one of hard coding errors noticed by Alejandro
    genericPlot <- PlotEmpiricalOperatingCharacteristics(dataset, rdrs = 1:length(zjk1[,1]), opChType = "ROC")$Plot
  } else if ((dataType == "LROC") && ((FOM == "PCL") || (FOM == "ALROC")))  {
    if (dataType == "LROC") genericPlot <- LrocPlots (zjk1, zjk2, seq(1,length(zjk1[,1])-1))$lrocPlot
  } else if ((dataType == "LROC") && (FOM == "Wilcoxon"))  {
    if (dataType == "LROC") {
      datasetRoc <- DfLroc2Roc(dataset)
      genericPlot <- PlotEmpiricalOperatingCharacteristics(datasetRoc, rdrs = 1:length(zjk1[,1]), opChType = "ROC")$Plot
    }
  } else if ((dataType == "FROC") && (FOM %in% c("HrAuc", "AFROC", "wAFROC")))  {
    if (FOM == "HrAuc") genericPlot <- PlotEmpiricalOperatingCharacteristics(dataset, rdrs = 1:length(zjk1[,1]), opChType = "ROC")$Plot
    if (FOM == "AFROC") genericPlot <- PlotEmpiricalOperatingCharacteristics(dataset, rdrs = 1:length(zjk1[,1]), opChType = "AFROC")$Plot
    if (FOM == "wAFROC") genericPlot <- PlotEmpiricalOperatingCharacteristics(dataset, rdrs = 1:length(zjk1[,1]), opChType = "wAFROC")$Plot
  }else stop("data type has to be ROC, FROC or LROC")
  
  return(genericPlot)
}



# extracts three ratings arrays from the 
# dataset; the first is always dataset$NL
# The second depends on the FOM, and the 
# third is the incorrect localizations array
dataset2ratings <- function (dataset, FOM){
  dataType <- dataset$dataType
  if (dataType != "LROC") {
    K2 <- length(dataset$LL[1,1,,1])
  } else if (dataType == "LROC") {
    K2 <- length(dataset$LLCl[1,1,,1])
  } else stop("Incorrect data type") # should never get here
  
  if (dataType == "ROC") {
    zjk1 <- drop(dataset$NL) # must retain the full length K of the  array
    # otherwise the number of cases is thrown off
    # so array returned is J x K
    # This messes up FPF calculation in LrocOperatingPointsFromRatings as K1 is (120-80 = 40) 
    # and FPF-CAD exceeds unity in the middle 
    # and other readers plots do not go to FPF = 1.
    # Did not notice this before as plots flag = TRUE was not tested
    zjk2 <- dataset$LL[,,1:K2,1]
    zjk2Il <- NA
  } else if (dataType == "LROC") {
    if (FOM %in% c("ALROC", "PCL")) {
      zjk1 <- drop(dataset$NL) # do: must retain the full length K of the  array
      zjk2 <- dataset$LLCl[,,1:K2,1]
      zjk2Il <- dataset$LLIl[,,1:K2,1]
    } else if (FOM == "Wilcoxon")  {
      datasetRoc <- DfLroc2Roc(dataset)
      zjk1 <- drop(datasetRoc$NL) # do: must retain the full length K of the  array
      zjk2 <- datasetRoc$LL[,,1:K2,1]
      zjk2Il <- NA
    } 
  } else if ((dataType == "FROC") && FOM %in% c("HrAuc", "AFROC", "wAFROC")) {
    zjk1 <- dataset$NL
    zjk2 <- dataset$LL
    zjk2Il <- NA
  } else if ((dataType == "ROI") && FOM == "ROI") {
    zjk1 <- dataset$NL 
    zjk2 <- dataset$LL
    zjk2Il <- NA
  } else stop("incorrect FOM or dataType")
  
  
  return(list(
    zjk1 = zjk1,
    zjk2 = zjk2,
    zjk2Il = zjk2Il # this is needed in 2T analysis
  ))
}



##
## this uses the jackknife to estimate var & cov2
## Handles all datasets
DiffFomVarCov2 <- function (dataset, FOM, FPFValue) # for difference FOM, radiologist minus CAD
{ 
  #if (dataset$dataType == "LROC") stop("Dataset must NOT be LROC")
  
  J <- length(dataset$readerID)
  K <- length(dataset$NL[1,1,,1])
  
  dsCad <- DfExtractDataset(dataset, trts = 1, rdrs = 1)
  dsRad <- DfExtractDataset(dataset, trts = 1, rdrs = c(2:J))
  jkFomValuesCad <- UtilPseudoValues(dsCad, FOM, FPFValue)$jkFomValues
  jkFomValuesRad <- UtilPseudoValues(dsRad, FOM, FPFValue)$jkFomValues
  jkDiffFomValues <- array(dim = c(J-1,K))
  # this does not work !!!DPC!!! wasted much time
  # this does not work !!!DPC!!! wasted much time
  # jkDiffFomValues <- jkFomValuesRad[1,,] - jkFomValuesCad[1,1,] # this does not work
  # this does not work !!!DPC!!! wasted much time
  # this does not work !!!DPC!!! wasted much time
  
  for (j in 1:(J-1)) jkDiffFomValues[j,] <- jkFomValuesRad[1,j,] - jkFomValuesCad[1,1,]
  
  J <- J - 1 # number of human readers 
  
  Covariance <- array(dim = c(J, J))  
  for (j in 1:J){
    for (jp in 1:J){
      Covariance[j, jp] <- cov(jkDiffFomValues[j, ], jkDiffFomValues[jp, ])          
    }
  }  
  
  varError <- 0;count <- 0
  for (j in 1:J){    
    varError <- varError + Covariance[j, j] 
    count <- count + 1
  }
  varError <- varError / count 
  varError <- varError *(K-1)^2/K
  
  Cov2 <- 0;count <- 0
  for (j in 1:J){    
    for (jp in 1:J){
      if (jp != j){
        Cov2 <- Cov2 + Covariance[j, jp] 
        count <- count + 1
      }
    }
  }  
  Cov2 <- Cov2 / count 
  Cov2 <- Cov2 *(K-1)^2/K
  
  return (list (    
    var = varError,
    cov2 = Cov2
  ))  
}





LrocOperatingPointsFromRatings <- function( zk1, zk2Cl ) 
{
  if (FALSE) { # this is to check the method, see ChkLrocFoms.xlsx
    zk1 <- c(rep(0, 4), 1.424443, rep(-50,5))
    zk2Cl <- c(0.5542791, 1.2046176, -50, 3.4596787, 2.732657)
    K2 <- length(zk2Cl)
    K1 <- length(zk1) - K2
    zk1 <- zk1[zk1 != -50]
    zk2Cl <- zk2Cl[zk2Cl != -50]
  } else {
    K2 <- length(zk2Cl)
    K1 <- length(zk1) - K2
    zk1 <- zk1[is.finite(zk1)]
    zk2Cl <- zk2Cl[is.finite(zk2Cl)]
  }
  
  FPF <- 1
  PCL <- NULL
  zk1Temp <- zk1;zk2ClTemp <- zk2Cl
  while(1) {
    cutoff <- min( c( zk1Temp, zk2ClTemp ) )
    zk1Temp <- zk1[ zk1 > cutoff ]
    zk2ClTemp <- zk2Cl[ zk2Cl > cutoff ]
    FPF1 <- length( zk1Temp ) / K1
    PCL1 <- length( zk2ClTemp ) / K2
    FPF <- c( FPF, FPF1 )
    if (length(PCL) == 0) PCL <- c(PCL1, PCL1) else PCL <- c( PCL, PCL1 )
    if( FPF1 == 0 && PCL1 == 0 ) {
      break
    }
  }
  
  FPF <- rev(FPF)
  PCL <- rev(PCL)
  
  return( list(
    FPF = FPF,
    PCL = PCL
  ) )
}




LrocPlots <- function (zjk1, zjk2, doJ) 
{
  J <- length(zjk1[,1])
  j <- 1;zjk1Temp <- zjk1[j,];zk2Temp <- zjk2[j,]
  lroc <- LrocOperatingPointsFromRatings( zjk1Temp, zk2Temp )
  FPF <- lroc$FPF;PCL <- lroc$PCL
  
  lrocPlotData <- data.frame(FPF = FPF, PCL = PCL, reader = "R-CAD")
  for (j in 2:J) {
    if ((j - 1) %in% doJ) {
      zjk1Temp <- zjk1[j,]
      zk2Temp <- zjk2[j,]    
      lroc <- LrocOperatingPointsFromRatings( zjk1Temp, zk2Temp )
      FPF <- lroc$FPF
      PCL <- lroc$PCL
      reader = paste0("R-", as.character(j - 1))
      lrocPlotData <- rbind(lrocPlotData, data.frame(FPF = FPF, PCL = PCL, reader = reader))
    }
  }
  
  lrocPlot <- ggplot(data = lrocPlotData, aes(x = FPF, y = PCL, color = reader)) + geom_line()
  g <- ggplot_build(lrocPlot)
  colors <- as.character(unique(g$data[[1]]$colour))
  colors[1] <- "#000000"
  sizes <- c(2, rep(1, length(doJ)))
  lrocPlot <- ggplot(data = lrocPlotData, aes(x = FPF, y = PCL, color = reader)) + geom_line(aes(size = reader)) + 
    scale_color_manual(values = colors) + scale_size_manual(values = sizes) + 
    theme(legend.title = element_blank(), legend.position = legendPosition <- c(1, 0), legend.justification = c(1, 0))
  return(list(
    lrocPlot = lrocPlot,
    afrocPlot = NULL)
  )
}
