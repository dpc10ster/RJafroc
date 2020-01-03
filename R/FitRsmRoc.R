#' Fit the radiological search model (RSM) to an ROC dataset
#' 
#' @description Fit an RSM-predicted ROC curve to a \strong{binned single-modality single-treatment ROC dataset}
#' 
#' @param binnedRocData The \strong{binned ROC} dataset containing the data
#' @param lesDistr Array [1:maxLL,1:2]. The probability mass function of the 
#'    lesion distribution for diseased cases. The first column contains the 
#'    actual numbers of lesions per case. The second column contains the fraction 
#'    of diseased cases with the number of lesions specified in the first column. 
#'    The second column must sum to unity. 
#' @param trt The selected treatment, default is 1
#' @param rdr The selected reader, default is 1
#' 
#' 
#' @return The return value is a list with the following elements:
#' @return \item{mu}{The mean of the diseased distribution relative 
#'    to the non-diseased one} 
#' @return \item{lambdP}{The Poisson parameter describing the distribution 
#'    of latent NLs per case}
#' @return \item{nuP}{The binomial success probability describing the distribution
#'    of latent LLs per diseased case}
#' @return \item{zetas}{The RSM cutoffs, zetas or thresholds} 
#' @return \item{AUC}{The RSM fitted ROC-AUC} 
#' @return \item{StdAUC}{The standard deviation of AUC} 
#' @return \item{NLLIni}{The initial value of negative LL} 
#' @return \item{NLLFin}{The final value of negative LL} 
#' @return \item{ChisqrFitStats}{The chisquare goodness of fit results} 
#' @return \item{covMat}{The covariance matrix of the parameters} 
#' @return \item{fittedPlot}{A \pkg{ggplot2} object containing the fitted 
#'    operating characteristic along with the empirical operating points. 
#'    Use \code{print} to display the object} 
#' 
#' 
#' @details 
#' If dataset is FROC, first convert it to ROC, using \code{\link{DfFroc2Roc}}. MLE ROC algorithms 
#'    require binned datasets. Use \code{\link{DfBinDataset}} to perform the binning prior to calling 
#'    this function. 
#'    In the RSM: (1) The (random) number of latent NLs per case is Poisson distributed 
#'    with mean parameter lambdaP, and the corresponding ratings are sampled from 
#'    \eqn{N(0,1)}. The (2) The (random) number of latent LLs per diseased case is 
#'    binomial distributed with success probability nuP and trial size equal to 
#'    the number of lesions in the case, and the corresponding ratings are sampled from 
#'    N(\eqn{mu},1). (3) A latent NL or LL is actually marked if its rating exceeds 
#'    the lowest threshold zeta1. To avoid clutter error bars are only shown for the 
#'    lowest and uppermost operating points. Because of the extra parameter, and the 
#'    requirement to have five counts, the chi-square statistic often cannot be calculated. 
#' 
#' 
#' @examples
#' \donttest{
#' ## Test with included ROC data (some bins have zero counts)
#' lesDistr <- UtilLesionDistr(dataset02)
#' retFit <- FitRsmRoc(dataset02, lesDistr)
#' print(retFit$fittedPlot)
#' 
#' ## Test with included degenerate ROC data
#' lesDistr <- UtilLesionDistr(datasetDegenerate)
#' retFit <- FitRsmRoc(datasetDegenerate, lesDistr);print(retFit$fittedPlot)
#' 
#' ## Test with single interior point data
#' fp <- c(rep(1,7), rep(2, 3))
#' tp <- c(rep(1,5), rep(2, 5))
#' binnedRocData <- Df2RJafrocDataset(fp, tp)
#' lesDistr <- UtilLesionDistr(binnedRocData)
#' retFit <- FitRsmRoc(binnedRocData, lesDistr);print(retFit$fittedPlot)
#' 
#' ## Test with two interior data points
#' fp <- c(rep(1,7), rep(2, 5), rep(3, 3))
#' tp <- c(rep(1,3), rep(2, 5), rep(3, 7))
#' binnedRocData <- Df2RJafrocDataset(fp, tp)
#' lesDistr <- UtilLesionDistr(binnedRocData)
#' retFit <- FitRsmRoc(binnedRocData, lesDistr);print(retFit$fittedPlot)
#' 
#' 
#' ## Test with three interior data points
#' fp <- c(rep(1,12), rep(2, 5), rep(3, 3), rep(4, 5)) #25
#' tp <- c(rep(1,3), rep(2, 5), rep(3, 7), rep(4, 10)) #25
#' binnedRocData <- Df2RJafrocDataset(fp, tp)
#' lesDistr <- UtilLesionDistr(binnedRocData)
#' retFit <- FitRsmRoc(binnedRocData, lesDistr);print(retFit$fittedPlot)
#' 
#' ## test for TONY data, i = 2 and j = 3; only case permitting chisqure calculation
#' lesDistr <- UtilLesionDistr(dataset01)
#' rocData <- DfFroc2Roc(dataset01)
#' retFit <- FitRsmRoc(rocData, lesDistr, trt = 2, rdr = 3)
#' print(retFit$fittedPlot)
#' retFit$ChisqrFitStats
#' }
#' 
#' @references 
#' Chakraborty DP (2006) A search model and figure of merit for observer data acquired according to the free-response 
#' paradigm. Phys Med Biol 51, 3449-3462.
#' 
#' Chakraborty DP (2006) ROC Curves predicted by a model of visual search. Phys Med Biol 51, 3463--3482.
#'
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#'  
#' @importFrom bbmle mle2
#' @importFrom stats lm
#' @importFrom binom binom.confint
#' 
#' @export
#'
###Why are parameter values always the same? 10/25/2018; added following comment from documentation mle2
###Method "BFGS" is a quasi-Newton method (also known as a variable metric algorithm), 
###specifically that published simultaneously in 1970 by Broyden, Fletcher, Goldfarb and 
###Shanno. This uses function values and gradients to build up a picture of the surface to be optimized. 
###DPC note: this implies it does not involve random search in parameter space, so should get same 
###results every time, regardless of seed, as observed.
FitRsmRoc <- function(binnedRocData, lesDistr, trt = 1, rdr = 1){ 
  # since the ROC dataset is sometimes derived from a FROC dataset with multiple lesions, 
  # lesDist has to be supplied externally
  
  if (missing(lesDistr)) stop("FitRsmRoc needs the lesDistr argument")
  maxLambdaP <- RJafrocEnv$maxLambdaP
  minLambdaP <- RJafrocEnv$minLambdaP
  maxNuP <- RJafrocEnv$maxNuP
  minNuP <- RJafrocEnv$minNuP
  maxMu <- RJafrocEnv$maxRsmMu
  minMu <- RJafrocEnv$minMu
  #minZeta <- RJafrocEnv$minZeta
  #maxZeta <- RJafrocEnv$maxZeta
  
  # modalityID <- binnedRocData$modalityID[trt]
  # readerID <- binnedRocData$readerID[rdr]
  class(lesDistr) <- "numeric"
  
  fp <- binnedRocData$NL[trt,rdr,,1];fp <- fp[fp != -Inf]
  tp <- binnedRocData$LL[trt,rdr,,1]
  plotStep <- 0.01
  plotZeta <- seq(from = -3, to = 10, by = plotStep)
  
  ret1 <- UtilBinCountsOpPts(binnedRocData, trt, rdr) 
  fpf <- ret1$fpf;tpf <- ret1$tpf;fpCounts <- ret1$fpCounts;tpCounts <- ret1$tpCounts
  # K1 <- sum(fpCounts);K2 <- sum(tpCounts)
  if (isDataDegenerate (fpf, tpf)) {
    if (max(tpf) > max(fpf)) { 
      mu <- maxMu # technically infinity; but then vertical line does not plot
      lambdaP <- -log(1 - fpf[length(fpf)]) # dpc
      nuP <- max(tpf)
      fpfPred <- sapply(plotZeta, xROC, lambdaP = lambdaP)
      tpfPred <- sapply(plotZeta, yROC, mu = mu, lambdaP = lambdaP,
                        nuP = nuP, lesDistr = lesDistr)
      fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred, method = "RSM")
      return(list(
        mu = maxMu,
        lambdaP = minLambdaP,
        nuP = max(tpf),
        zetas = NA,
        AUC = (1+max(tpf))/2, 
        StdAUC = 0,
        NLLIni = NA,
        NLLFin = NA,
        ChisqrFitStats = list(NA,NA,NA),
        covMat = NA,
        fittedPlot = fittedPlot 
      ))
    } else { # right or bottom boundary of ROC
      mu <- minMu # 
      lambdaP <- maxLambdaP # dpc
      nuP <- minNuP
      fpfPred <- sapply(plotZeta, xROC, lambdaP = lambdaP)
      tpfPred <- sapply(plotZeta, yROC, mu = mu, lambdaP = lambdaP, 
                        nuP = nuP, lesDistr = lesDistr)
      fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred, method = "RSM")
       return(list(
        mu = minMu,
        lambdaP = maxLambdaP,
        nuP = minNuP,
        zetas = NA,
        AUC = (1+max(tpf))/2, 
        StdAUC = 0,
        NLLIni = NA,
        NLLFin = NA,
        ChisqrFitStats = list(NA,NA,NA),
        covMat = NA,
        fittedPlot = fittedPlot 
      ))
    }
  }
  
  retCbm <- FitCbmRoc(binnedRocData, trt = trt, rdr = rdr)
  #aucCbm <- retCbm$AUC
  muIni <- retCbm$mu
  lambdaPIni <- -log(1 - fpf[length(fpf)]) # dpc
  
  nuPIni <- retCbm$alpha # dpc
  zetasIni <- retCbm$zetas
  
  mu <- muIni; lambdaP <- lambdaPIni;nuP <- nuPIni;zetas <- zetasIni
  
  # while (1){  
  muFwd <- ForwardValue(mu, minMu, maxMu)
  lambdaPFwd <- ForwardValue(lambdaP, minLambdaP, maxLambdaP)
  nuPFwd <- ForwardValue(nuP, minNuP, maxNuP)
  zetaFwd <- ForwardZetas(zetas)
  
  parameters <- c(muFwd, lambdaPFwd, nuPFwd, zetaFwd)
  namesVector <- paste0("zetaFwd", 1:length(zetaFwd))
  names(parameters) <- c("muFwd" ,"lambdaPFwd", "nuPFwd", namesVector)
  RSMNLLAdd <- AddArguments(RSMNLL, length(zetaFwd))
  
  ret <- mle2(RSMNLLAdd, start = as.list(parameters), eval.only = TRUE,
              data = list(fb = fpCounts, tb = tpCounts, lesDistr = lesDistr),
              method = "BFGS", control = list(maxit = 2000))
  
  NLLIni <- ret@min
  
  ret <- mle2(RSMNLLAdd, start = as.list(parameters),
              data = list(fb = fpCounts, tb = tpCounts, lesDistr = lesDistr),
              method = "BFGS", control = list(maxit = 2000))
  
  if (ret@details$convergence != 0) stop("mle2 error in rsm fit")
  
  vcov <- ret@vcov
  
  mu <- InverseValue(ret@coef[1], minMu, maxMu)
  lambdaP <- InverseValue(ret@coef[2], minLambdaP, maxLambdaP)
  nuP <- InverseValue(ret@coef[3], minNuP, maxNuP)
  zetas <- InverseZetas(ret@coef[4:length(ret@coef)])
  
  NLLFin <- ret@min
  
  AUC <- UtilAucsRSM(mu, lambdaP, nuP, lesDistr)$aucROC
  ## following checks out
  ##temp <- tempAucRSM (c(ret@coef[1], ret@coef[2], ret@coef[3]), lesDistr)  
  
  covMat <- vcov[1:3,1:3]
  StdAUC <- StdDevRsmAuc(ret@coef[1], ret@coef[2], ret@coef[3], covMat, lesDistr = lesDistr) ## !!!dpc!!! looks right; can it be proved?  
  
  ChisqrFitStats <- ChisqrGoodnessOfFit(fpCounts, tpCounts,
                                        parameters = c(mu,lambdaP,nuP,zetas), model = "RSM", lesDistr)
  
  fpfPred <- sapply(plotZeta, xROC, lambdaP = lambdaP)
  tpfPred <- sapply(plotZeta, yROC, mu = mu, lambdaP = lambdaP, 
                    nuP = nuP, lesDistr = lesDistr)
  fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred, method = "RSM")

  # calculate covariance matrix using un-transformed variables
  namesVector <- c(c("mu", "lambdaP", "nuP"), paste0("zeta", 1:length(zetas)))
  parameters <- c(list(mu, lambdaP, nuP), as.list(zetas))
  names(parameters) <- namesVector
  
  RSMNLLAddNoTransf <- AddArguments2(RSMNLLNoTransf, length(zetas))
  
  # ret <- mle2(RSMNLLAddNoTransf, start = parameters, 
  #             upper = c(mu = 10, lambdaP = 20, nuP = 0.9999, zeta1 = 3, zeta2 = 3, zeta3 = 3, zeta4 = 3), 
  #             method = "L-BFGS-B", 
  #             data = list(fb = fpCounts, tb = tpCounts, lesDistr = lesDistr))
  # 
  # ret <- mle2(RSMNLLAddNoTransf, start = parameters, fixed = as.vector("nuP"),
  #             method = "BFGS", 
  #             data = list(fb = fpCounts, tb = tpCounts, lesDistr = lesDistr))
  # 
  
  if ((nuP < 0.98) && (nuP > 0.02) && (mu > 0.1)){
    
    ret <- suppressWarnings(mle2(RSMNLLAddNoTransf, start = parameters, 
                                 method = "BFGS", 
                                 data = list(fb = fpCounts, tb = tpCounts, lesDistr = lesDistr)))
    
    covMat <- ret@vcov
    NLLChk <- ret@min
    
  } else {
    
    covMat <- NA
    
  }
  
  #   if (NLLChk < NLLFin) {
  #     if (abs(NLLChk - NLLFin)/NLLFin > 1e-6) next else break
  #   } else break
  #   
  # }
  
  return(list(
    mu = mu,
    lambdaP = lambdaP,
    nuP = nuP,
    zetas = zetas,
    AUC = AUC, 
    StdAUC = StdAUC,
    NLLIni = NLLIni,
    NLLFin = NLLFin,
    ChisqrFitStats = ChisqrFitStats,
    covMat = covMat,
    fittedPlot = fittedPlot 
  ))
}




###############################################################################
RSMNLL <- function(muFwd, lambdaPFwd, nuPFwd, fb, tb, lesDistr){
  maxLambdaP <- RJafrocEnv$maxLambdaP
  minLambdaP <- RJafrocEnv$minLambdaP
  maxNuP <- RJafrocEnv$maxNuP
  minNuP <- RJafrocEnv$minNuP
  maxMu <- RJafrocEnv$maxRsmMu
  minMu <- RJafrocEnv$minMu
  
  mu <- InverseValue(muFwd, minMu, maxMu)
  lambdaP <- InverseValue(lambdaPFwd, minLambdaP, maxLambdaP)
  nuP <- InverseValue(nuPFwd, minNuP, maxNuP)
  allParameters <- names(formals())
  zetaPos <- grep("zetaFwd", allParameters)
  zetas <- unlist(mget(allParameters[zetaPos]))
  zetas <- InverseZetas(zetas)
  
  return(RsmInner(mu, lambdaP, nuP, lesDistr, zetas, fb, tb))
}



###############################################################################
# RSM paradigm negative of log likelihood function, without transformations
RSMNLLNoTransf <- function (mu, lambdaP, nuP, fb, tb, lesDistr){
  
  allParameters <- names(formals())
  zetaPos <- regexpr("zeta", allParameters)
  zetas <- unlist(mget(allParameters[which(zetaPos == 1)]))
  L <- RsmInner(mu, lambdaP, nuP, lesDistr, zetas, fb, tb)
  return(L)
  
}



###############################################################################
# inputs are transformed parameters; covMat is also wrt trans. parameters
StdDevRsmAuc<-function(muFwd,lambdaPFwd,nuPFwd,covMatFwd, lesDistr)
{
  derivsFwd <- jacobian(func = tempAucRSM, c(muFwd,lambdaPFwd,nuPFwd), lesDistr = lesDistr) # this is for transformed variables
  VarAz <- derivsFwd %*% covMatFwd %*% t(derivsFwd)
  if (VarAz < 0 || is.na(VarAz)) return(NA) else return(sqrt(VarAz))
}



###############################################################################
tempAucRSM <- function (forwardParms, lesDistr = lesDistr){
  maxLambdaP <- RJafrocEnv$maxLambdaP
  minLambdaP <- RJafrocEnv$minLambdaP
  maxNuP <- RJafrocEnv$maxNuP
  minNuP <- RJafrocEnv$minNuP
  maxMu <- RJafrocEnv$maxRsmMu
  minMu <- RJafrocEnv$minMu
  
  mu <- InverseValue(forwardParms[1], minMu, maxMu)
  lambdaP <- InverseValue(forwardParms[2], minLambdaP, maxLambdaP)
  nuP <- InverseValue(forwardParms[3], minNuP, maxNuP)
  
  maxFPF <- xROC(-20, lambdaP)
  maxTPF <- yROC(-20, mu, lambdaP, nuP, lesDistr)
  AUC <- integrate(intROC, 0, maxFPF, mu = mu, lambdaP = lambdaP, nuP = nuP, 
                   lesDistr = lesDistr)$value
  
  AUC <- AUC + (1 + maxTPF) * (1 - maxFPF) / 2
  
  return (AUC)
}



