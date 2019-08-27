#' Significance testing, CAD vs. radiologists
#' 
#' @description  Significance testing, comparing CAD vs. a group of radiologists 
#'    interpreting the same cases, an example of single treatment analysis
#' 
#' @param dataset \strong{The dataset must be ROC or LROC.}
#' @param FOM The desired FOM, default is \code{"Wilcoxon"} for ROC data, or ROC data 
#'    inferred from LROC data; 
#'    for LROC data the choices are \code{"PCL"} and \code{"ALROC"}. 
#' @param option The desired generalization, the default is \code{"RRRC"}; 
#'    another possibility is \code{"RRFC"}.
#' @param method \code{"singleModality"}, the default, or \code{"dualModality"},
#'    see details.
#' @param FPFValue Only needed for LROC data; where to evaluate a partial curve based
#'    figure of merit, see details. The default is 0.2.
#'
#' ## potential project for summer student
#' @note The extension of the code to FROC will be addressed in a future update.
#' 
#' @details 
#' PCL is the probability of a correct localization. The LROC is the plot of PCL 
#'    (ordinate) vs. FPF. For LROC data \code{"PCL"} means interpolated PCL value 
#'    at specified \code{"FPFValue"}. \code{"ALROC"} is the trapezoidal area 
#'    under the LROC
#'    from FPF = 0 to FPF = \code{FPFValue}. If \code{method = "singleModality"} 
#'    the first \strong{reader} is assumed to be CAD. If 
#'    \code{method = "dualModality"} the first \strong{treatment} is assumed to be CAD. 
#'    The NH is that the FOM of CAD equals the average of the readers. The 
#'    \code{method = "singleModality"} analysis uses an adaptation of the 
#'    single-treatment multiple-reader Obuchowski Rockette (OR) model described in a 
#'    paper by Hillis (2007), section 5.3. The adaptation is characterized by 3 
#'    parameters
#'    \code{VarR}, \code{Var} and \code{Cov2}, which are returned by the function. 
#'    The \code{method = "dualModality"} analysis replicates CAD data as many times as
#'    necessary so as to form one "treatment" of an MRMC pairing, the other 
#'    "treatment" being the 
#'    radiologists. Standard RRRC DBMH/ORH analysis is applied. The 
#'    method, described 
#'    in Kooi et al gives exactly the same final results (F-statistic, ddf and p-value) 
#'    as \code{"singleModality"} but the intermediate quantities are questionable. 
#'    The method is characterized by 6 OR parameters \code{VarR}, \code{VarTR}, 
#'    \code{Var}, \code{Cov1}, \code{Cov2} and \code{Cov3}, which are returned 
#'    by the function. 

#' 
#' @return If \code{method = "singleModality"} the return value is a 
#' list with the following elements: 
#' @return \item{fomCAD}{The observed FOM for CAD} 
#' @return \item{fomRAD}{The observed FOM array for the readers} 
#' @return \item{avgRadFom}{The average FOM of the readers} 
#' @return \item{avgDiffFom}{The mean of the difference FOM, RAD - CAD} 
#' @return \item{ciAvgDiffFom}{The 95-percent CI of the average difference, RAD - CAD}
#' @return \item{varR}{The variance of the radiologists}
#' @return \item{varError}{The variance of the error term in the single-treatment 
#'    multiple-reader OR model} 
#' @return \item{cov2}{The covariance of the error term}
#' @return \item{tstat}{The observed value of the t-statistic; it's square is 
#'    equivalent to an F-statistic} 
#' @return \item{df}{The degrees of freedom of the t-statistic} 
#' @return \item{pval}{The p-value for rejecting the NH} 
#' @return \item{Plots}{Empirical operating characteristic plots 
#'    corresponding to specified FOM} 
#'
#'
#' @return If \code{method = "dualModality"} the return value is a list 
#'    with the following elements: 
#' @return \item{fomCAD}{The observed FOM for CAD} 
#' @return \item{fomRAD}{The observed FOM array for the readers} 
#' @return \item{avgRadFom}{The average FOM of the readers} 
#' @return \item{avgDiffFom}{The mean of the difference FOM, RAD - CAD} 
#' @return \item{ciDiffFom}{A data frame containing the statistics associated 
#'    with the average difference, RAD - CAD}
#' @return \item{ciAvgRdrEachTrt}{A data frame containing the statistics 
#'    associated with the average FOM in each treatment}
#' @return \item{varR}{The variance of the pure reader term in the OR model}
#' @return \item{varTR}{The variance of the treatment-reader term error 
#'    term in the OR model} 
#' @return \item{cov1}{The covariance1 of the error term - same reader, 
#'    different treatments}
#' @return \item{cov2}{The covariance2 of the error term  - 
#'    different readers, same treatment}
#' @return \item{cov3}{The covariance3 of the error term  - different readers, 
#'    different treatments}
#' @return \item{varError}{The variance of the pure error term in the OR model}
#' @return \item{Fstat}{The observed value of the F-statistic} 
#' @return \item{ndf}{The numerator degrees of freedom of the F-statistic} 
#' @return \item{ddf}{The denominator degrees of freedom of the F-statistic} 
#' @return \item{pval}{The p-value for rejecting the NH} 
#' @return \item{Plots}{Empirical operating characteristic plots corresponding 
#'    to specified FOM, i.e., if \code{FOM} = \code{"Wilcoxon"} an ROC plot
#'    is produced where reader 1 is CAD. If an LROC FOM is selected, an LROC
#'    plot is displayed.} 
#' 
#' 
#' 
#' @examples 
#' ret1M <- StSignificanceTestingCadVsRadiologists (dataset09, 
#' FOM = "Wilcoxon", method = "singleModality")
#' 
#' \dontrun{
#' ## takes longer than 5 sec on OSX
#' ret2M <- StSignificanceTestingCadVsRadiologists (dataset09, 
#' FOM = "Wilcoxon", method = "dualModality")
#' 
#' retLroc1M <- StSignificanceTestingCadVsRadiologists (datasetCadLroc, 
#' FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05)
#' 
#' retLroc2M <- StSignificanceTestingCadVsRadiologists (datasetCadLroc, 
#' FOM = "PCL", option = "RRRC", method = "dualModality", FPFValue = 0.05)
#' 
#' ## test with fewer readers
#' dataset09a <- DfExtractDataset(dataset09, rdrs = seq(1:7))
#' ret1M7 <- StSignificanceTestingCadVsRadiologists (dataset09a, 
#' FOM = "Wilcoxon", method = "singleModality")
#' ret2M7 <- StSignificanceTestingCadVsRadiologists (dataset09a, 
#' FOM = "Wilcoxon", method = "dualModality")
#' 
#' datasetCadLroc7 <- DfExtractDataset(datasetCadLroc, rdrs = seq(1:7))
#' ret1MLroc7 <- StSignificanceTestingCadVsRadiologists (datasetCadLroc7, 
#' FOM = "PCL", option = "RRRC", method = "singleModality", FPFValue = 0.05)
#' 
#' ret2MLroc7 <- StSignificanceTestingCadVsRadiologists (datasetCadLroc7, 
#' FOM = "PCL", option = "RRRC", method = "dualModality", FPFValue = 0.05)
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

#' @export
StSignificanceTestingCadVsRadiologists <- function(dataset, FOM = "Wilcoxon", 
  option = "RRRC", method = "singleModality", FPFValue = 0.2) {
  if (length(dataset$NL[,1,1,1]) != 1) stop("dataset has to be single-treatment multiple-readers with CAD as the first reader")
  if ((dataset$dataType == "ROC") && (FOM %in% c("PCL", "ALROC"))) stop("Cannot use LROC FOM with ROC data")
  if (option == "RRFC") {
    ret <- RRFC(dataset, FOM = FOM, FPFValue = FPFValue)
  } else if (option == "RRRC") {  
    if (method == "singleModality"){
      ret <- DiffFomAnal2007Hillis53(dataset, FOM = FOM, FPFValue = FPFValue)
    } else if (method == "dualModality") {
      ret <- dualModalityMethod (dataset, FOM = FOM, option = option, FPFValue = FPFValue)
    } else stop("incorrect method specified")
  } else stop("incorrect option specified")
  ret <- addPlot (dataset, ret, FOM)
  
  return(ret)
  
}



RRFC <- function(dataset, FOM = "Wilcoxon", FPFValue = 0.2, alpha = 0.05){
  thetajc <- lroc2fomCad (dataset, FOM, FPFValue)
  Psijc <- thetajc[-1] - thetajc[1]
  ret <- t.test(Psijc)
  Tstat <-  as.numeric(ret$statistic)
  df <-  as.numeric(ret$parameter)
  pval <- ret$p.value
  CIAvgRadFom <- as.numeric(ret$conf.int)+thetajc[1]
  avgDiffFom <- mean(Psijc)
  varDiffFom <-  var(thetajc[-1])
  CIAvgDiffFom <- as.numeric(ret$conf.int)
  return (list (
    fomCAD = thetajc[1],
    fomRAD = thetajc[-1],
    avgRadFom = mean(thetajc[-1]),
    CIAvgRadFom = CIAvgRadFom,
    avgDiffFom = avgDiffFom,
    CIAvgDiffFom = CIAvgDiffFom,
    varR = var(thetajc[-1]),
    # varError = NA, 
    # cov2 = NA, 
    Tstat = Tstat,
    df = df,
    pval = pval
  ))
}



addPlot <- function(dataset, ret, FOM) {
  ret1 <- dataset2ratings(dataset, FOM)
  zjk1 <- ret1$zjk1
  zjk2 <- ret1$zjk2
  
  dataType <- dataset$dataType
  if (dataType == "ROC") {
    # fixed one of hard coding errors noticed by Alejandro
    rocPlots <- PlotEmpiricalOperatingCharacteristics(dataset, rdrs = 1:length(zjk1[,1]))$Plot
    retNames <- names(ret)
    retNames <- c(retNames, "Plots")
    len <- length(ret)
    ret1 <- vector("list", len+1)
    for (i in 1:len){
      ret1[[i]] <- ret[[i]]
    }
    ret1[[len+1]] <- rocPlots
    names(ret1) <- retNames
  } else if ((dataType == "LROC") && ((FOM == "PCL") || (FOM == "ALROC")))  {
    if (dataType == "LROC") lrocPlots <- LrocPlots (zjk1, zjk2, seq(1,length(zjk1[,1])-1))$lrocPlot
    retNames <- names(ret)
    retNames <- c(retNames, "Plots")
    len <- length(ret)
    ret1 <- vector("list", len+1)
    for (i in 1:len){
      ret1[[i]] <- ret[[i]]
    }
    ret1[[len+1]] <- lrocPlots
    names(ret1) <- retNames
  } else if ((dataType == "LROC") && (FOM == "Wilcoxon"))  {
    if (dataType == "LROC") {
      dataset <- DfLroc2Roc(dataset)
      rocPlots <- PlotEmpiricalOperatingCharacteristics(dataset, rdrs = 1:length(zjk1[,1]))$Plot
    }
    retNames <- names(ret)
    retNames <- c(retNames, "Plots")
    len <- length(ret)
    ret1 <- vector("list", len+1)
    for (i in 1:len){
      ret1[[i]] <- ret[[i]]
    }
    ret1[[len+1]] <- rocPlots
    names(ret1) <- retNames
  } else if ((dataType == "FROC") && (FOM %in% c("Wilcoxon", "AFROC", "wAFROC")))  {
    afrocPlots <- PlotEmpiricalOperatingCharacteristics(dataset, rdrs = 1:length(zjk1[,1]), opChType = "AFROC")$Plot
    retNames <- names(ret)
    retNames <- c(retNames, "Plots")
    len <- length(ret)
    ret1 <- vector("list", len+1)
    for (i in 1:len){
      ret1[[i]] <- ret[[i]]
    }
    ret1[[len+1]] <- afrocPlots
    names(ret1) <- retNames
  }else stop("data type has to be ROC, FROC or LROC")
  
  return(ret1)
}



dualModalityMethod <- function(dataset, FOM = "Wilcoxon", option = "RRRC", FPFValue = 0.2){
  dataType <- dataset$dataType
  if ((dataType == "LROC") && (FOM %in% c("PCL", "ALROC"))) {
    ret1 <- dataset2ratings(dataset, FOM)
    FP <- ret1$zjk1
    TP <- ret1$zjk2
    zjk2Il <- ret1$zjk2Il
    K1 <- length(FP[1,])
    K2 <- length(TP[1,])
    K <- K1 + K2
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
    dataset <- DfLroc2Roc(dataset)
    dataType <- dataset$dataType
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
  
  stats1 <- StSignificanceTesting(datasetCombined,FOM = FOM, method = "ORH", option = option, FPFValue = FPFValue)
  thetajc <- stats1$fomArray
  fomCAD  <-  thetajc[1,1]
  fomRAD  <-  thetajc[2,]
  avgRadFom <-  mean(fomRAD)
  varDiffFom <- var(fomRAD)
  avgDiffFom <-  avgRadFom - fomCAD
  Fstat <-  stats1$fRRRC
  ddf <-  stats1$ddfRRRC
  ndf <- 1
  pval <-  stats1$pRRRC
  stop("this needs fixing")
  varR <- stats1$varComp$varCov[1]
  varTR <- stats1$varComp$varCov[2]
  varError <- stats1$varComp$varCov[6]
  cov1 <- stats1$varComp$varCov[3]
  cov2 <- stats1$varComp$varCov[4]
  cov3 <- stats1$varComp$varCov[5]
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
    Fstat = Fstat,
    ndf = ndf,
    ddf = ddf,
    pval = pval
  ))
  
}


dataset2ratings <- function (dataset, FOM){
  dataType <- dataset$dataType
  K <- length(dataset$NL[1,1,,1])
  if (dataType %in% c("ROC", "FROC")) {
    K2 <- length(dataset$LL[1,1,,1])
    K1 <- K - K2 
  }else if (dataType == "LROC") {
    K2 <- length(dataset$LLCl[1,1,,1])
    K1 <- K - K2 
  }
  
  if (dataType == "ROC") {
    zjk1 <- dataset$NL[,,1:K1,1]
    zjk2 <- dataset$LL[,,1:K2,1]
    zjk2Il <- NA
  } else if ((dataType == "LROC") && (FOM %in% c("ALROC", "PCL"))) {
    zjk1 <- dataset$NL[,,1:K1,1]
    zjk2 <- dataset$LLCl[,,1:K2,1]
    zjk2Il <- dataset$LLIl[,,1:K2,1]
  } else if ((dataType == "LROC") && (FOM == "Wilcoxon"))  {
    dataset <- DfLroc2Roc(dataset)
    zjk1 <- dataset$NL[,,1:K1,1]
    zjk2 <- dataset$LL[,,1:K2,1]
    zjk2Il <- NA
  } else if ((dataType == "FROC") && FOM %in% c("Wilcoxon", "AFROC", "wAFROC")) {
    zjk1 <- dataset$NL # not used; original dataset is used
    zjk2 <- dataset$LL
    zjk2Il <- NA
  } else stop("incorrect FOM")
  
  
  return(list(
    zjk1 = zjk1,
    zjk2 = zjk2,
    zjk2Il = zjk2Il
  ))
}




lroc2fomCad <- function (dataset, FOM, FPFValue) {
  dataType <- dataset$dataType
  ret <- dataset2ratings(dataset, FOM)
  zjk1 <- ret$zjk1
  zjk2 <- ret$zjk2
  I <- length(dataset$NL[,1,1,1])
  # dim(zjk1) <- c(I,dim(zjk1))
  # dim(zjk2) <- c(I,dim(zjk2))
  if (I == 1) {
    J <- length(zjk1[,1]) - 1 #singleModality anlaysis, don't count CAD
  } else stop("I cannot be > 1 here")
  thetajc <- array (dim = (J+1))
  if (FOM == "Wilcoxon") {
    dataset <- DfLroc2Roc(dataset)
    thetajc <- UtilFigureOfMerit(dataset, FOM = FOM)
  } else if (FOM == "PCL") {
    for (j in 1:(J+1)) {
      thetajc[j] <- (LrocFoms(zjk1[j,], zjk2[j,], FPFValue))$PCL
    } # the array zjk2Cl is actually passed 
  } else if (FOM == "ALROC") {
    for (j in 1:(J+1)) {
      thetajc[j] <- (LrocFoms(zjk1[j,], zjk2[j,], FPFValue))$ALroc
    } # the array zjk2Cl is actually passed 
  } else if (dataType == "FROC") {
    thetajc <- UtilFigureOfMerit(dataset, FOM = FOM)
  } else stop("incorrect FOM")  
  return(thetajc)  
}



lroc2fomMrmc <- function (dataset, FOM, FPFValue) {
  dataType <- dataset$dataType
  ret <- dataset2ratings(dataset, FOM)
  zjk1 <- ret$zjk1
  zjk2 <- ret$zjk2
  I <- length(dataset$NL[,1,1,1])
  # dim(zjk1) <- c(I,dim(zjk1))
  # dim(zjk2) <- c(I,dim(zjk2))
  if (I == 1) {
    stop("I cannot be one here")
  } else {
    J <- length(zjk1[1,,1])  
  }
  thetajc <- array (dim = c(I,J))
  if (FOM == "Wilcoxon") {
    dataset <- DfLroc2Roc(dataset)
    thetajc <- UtilFigureOfMerit(dataset, FOM = FOM)
  } else if (FOM == "PCL") {
    for (i in 1:I) {
      for (j in 1:J) {
        thetajc[i,j] <- (LrocFoms(zjk1[i,j,], zjk2[i,j,], FPFValue))$PCL
      } # the array zjk2Cl is actually passed 
    }
  } else if (FOM == "ALROC") {
    for (i in 1:I) {
      for (j in 1:J) {
        thetajc[i,j] <- (LrocFoms(zjk1[i,j,], zjk2[i,j,], FPFValue))$ALroc
      } # the array zjk2Cl is actually passed 
    }
  } else if (dataType == "FROC") {
    thetajc <- UtilFigureOfMerit(dataset, FOM = FOM)
  } else stop("incorrect FOM")  
  return(thetajc)  
}


## this uses the jackknife to estimate var & cov2
## 
## 
VarCov2 <- function (dataset, FOM, FPFValue) # for difference FOM, radiologist minus CAD
{ 
  ret <- dataset2ratings(dataset, FOM)
  zjk1 <- ret$zjk1
  zjk2 <- ret$zjk2
  
  J <- length(zjk1[,1]) - 1 # number of human readers
  K1 <- length(zjk1[1,]);K2 <- length(zjk2[1,]);K <- K1 +  K2
  
  PsiJk = array(dim = c(J,K))  
  for (k in 1:K) {
    if (k <= K1) {
      zjk1_jk <- zjk1[,-k] 
      zjk2_jk <- zjk2
    } else {
      zjk1_jk <- zjk1 
      zjk2_jk <- zjk2[,-(k-K1)]
    }
    
    if (FOM == "Wilcoxon") {
      aucCad_jk <- Wilcoxon(zjk1_jk[1, ], zjk2_jk[1, ])
      for (j in 1:J) PsiJk[j, k] <- Wilcoxon(zjk1_jk[j + 1, ], zjk2_jk[j + 1, ]) - aucCad_jk
    } else{
      if (FOM == "PCL") {
        PCLCad_jk <- (LrocFoms(zjk1_jk[1,], zjk2_jk[1,], FPFValue))$PCL  # the array zjk2Cl is actually passed 
        for (j in 1:J)  PsiJk[j,k] <- (LrocFoms(zjk1_jk[j + 1,], zjk2_jk[j + 1,], FPFValue))$PCL - PCLCad_jk # do
      } else if (FOM == "ALROC") {
        ALrocCad_jk <- (LrocFoms(zjk1_jk[1,], zjk2_jk[1,], FPFValue))$ALroc  # the array zjk2Cl is actually passed 
        for (j in 1:J)  PsiJk[j,k] <- (LrocFoms(zjk1_jk[j + 1,], zjk2_jk[j + 1,], FPFValue))$ALroc - ALrocCad_jk #do
      } else stop("incorrect FOM")  
    }
  } 
  
  Covariance <- array(dim = c(J, J))  
  for (j in 1:J){
    for (jp in 1:J){
      Covariance[j, jp] <- cov(PsiJk[j, ], PsiJk[jp, ])          
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
    varError = varError,
    Cov2 = Cov2
  ))  
}




DiffFomAnal2007Hillis53 <- function (dataset, FOM = "Wilcoxon", FPFValue = 0.2, alpha = 0.05)
{  
  ret <- dataset2ratings(dataset, FOM)#depending on FOM, zjk2 is either PCL or TPF 
  zjk1 <- ret$zjk1
  zjk2 <- ret$zjk2
  
  ret <- VarCov2(dataset, FOM, FPFValue)
  varError <- ret$varError;  Cov2 <- ret$Cov2  
  J <- length(zjk1[,1]) - 1 # subtract CAD FOM from reader FOM
  
  thetajc <- array (dim = (J+1))
  if (FOM == "Wilcoxon") {
    for (j in 1:(J+1)) thetajc[j] <- Wilcoxon(zjk1[j,],zjk2[j,])# the array max(zjk2Il, zjk2Cl) is actually passed 
  } else if (FOM == "PCL") {
    for (j in 1:(J+1)) thetajc[j] <- (LrocFoms(zjk1[j,], zjk2[j,], FPFValue))$PCL # the array zjk2Cl is actually passed 
  } else if (FOM == "ALROC") {
    for (j in 1:(J+1)) thetajc[j] <- (LrocFoms(zjk1[j,], zjk2[j,], FPFValue))$ALroc # do:
  } else if (FOM == "wAFROC") {
    lesionVector <- dataset$lesionVector
    lesionWeight <- dataset$lesionWeight
    K <- length(dataset$NL[1,1,,1])
    K2 <- length(dataset$LL[1,1,,1])
    K1 <- K - K2
    maxNL <-  length(dataset$NL[1,1,1,])
    maxLL <-  length(dataset$LL[1,1,1,])
    thetajc <- wJAFROC(zjk1, zjk2, lesionVector, c(K1, K2), maxNL, maxLL, lesionWeight)
  } else stop("incorrect FOM")  
  
  Psijc <- thetajc[2:(J+1)] - thetajc[1]
  
  MSR <- 0
  avgDiffFom <- mean(Psijc)
  for (j in 1:J){
    MSR <- MSR + (Psijc[j] - avgDiffFom)^2
  }
  MSR <- MSR / (J - 1)
  
  MSdenOR_single <- MSR + max(J * Cov2, 0)
  DdfhSingle <- MSdenOR_single^2 / (MSR^2 / (J - 1))
  TstatStar <- avgDiffFom / sqrt(MSdenOR_single/J)
  pval <- 1 - pt(abs(TstatStar),DdfhSingle) + pt(-abs(TstatStar),DdfhSingle)
  
  CIAvgDiffFom <- array(dim=2)
  CIAvgDiffFom[1] <- qt(alpha/2,df = DdfhSingle)
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
    varR = var(thetajc[-1]),
    varError = varError, 
    cov2 = Cov2, 
    Tstat = TstatStar,
    df = DdfhSingle,
    pval = pval
  ))
}



