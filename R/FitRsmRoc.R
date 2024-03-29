#' Fit the radiological search model (RSM) to an ROC dataset
#' 
#' @description Fit an RSM-predicted ROC curve to a \strong{binned single-modality single-reader ROC dataset}
#' 
#' @param binnedRocData A \strong{binned ROC} dataset
#' @param lesDistr The lesion distribution 1D array. 
#' @param trt The selected modality, default is 1
#' @param rdr The selected reader, default is 1
#' 
#' 
#' @return A list with the following elements
#' 
#' @return \item{mu}{The mean of the diseased distribution relative 
#'    to the non-diseased one} 
#'    
#' @return \item{lambda}{The Poisson parameter describing the distribution 
#'    of latent NLs per case}
#'    
#' @return \item{nu}{The binomial success probability describing the distribution
#'    of latent LLs per diseased case}
#'    
#' @return \item{zetas}{The RSM cutoffs, zetas or thresholds} 
#' 
#' @return \item{AUC}{The RSM fitted ROC-AUC} 
#' 
#' @return \item{StdAUC}{The standard deviation of AUC} 
#' 
#' @return \item{NLLIni}{The initial value of negative LL}
#'  
#' @return \item{NLLFin}{The final value of negative LL}
#'  
#' @return \item{ChisqrFitStats}{The chisquare goodness of fit results}
#'  
#' @return \item{covMat}{The covariance matrix of the parameters}
#'  
#' @return \item{fittedPlot}{A \pkg{ggplot2} object containing the fitted 
#'    operating characteristic along with the empirical operating points. 
#'    Use \code{print} to display the object} 
#' 
#' 
#' @details 
#' If dataset is FROC, first convert it to ROC, using \code{\link{DfFroc2Roc}}. 
#'    MLE ROC algorithms 
#'    require binned datasets. Use \code{\link{DfBinDataset}} to perform the 
#'    binning prior to calling 
#'    this function. 
#'    In the RSM: (1) The (random) number of latent NLs per case is Poisson distributed 
#'    with mean parameter lambda, and the corresponding ratings are sampled from 
#'    \eqn{N(0,1)}. The (2) The (random) number of latent LLs per diseased case is 
#'    binomial distributed with success probability nu and trial size equal to 
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
#' lesDistr <- UtilLesDistr(dataset02)$Freq
#' retFit <- FitRsmRoc(dataset02, lesDistr)
#' ## print(retFit$fittedPlot)
#' 
#' ## Test with included degenerate ROC data
#' lesDistr <- UtilLesDistr(datasetDegenerate)$Freq
#' retFit <- FitRsmRoc(datasetDegenerate, lesDistr)
#' 
#' ## Test with single interior point data
#' fp <- c(rep(1,7), rep(2, 3))
#' tp <- c(rep(1,5), rep(2, 5))
#' binnedRocData <- Df2RJafrocDataset(fp, tp)
#' lesDistr <- UtilLesDistr(binnedRocData)$Freq
#' retFit <- FitRsmRoc(binnedRocData, lesDistr)
#' 
#' ## Test with two interior data points
#' fp <- c(rep(1,7), rep(2, 5), rep(3, 3))
#' tp <- c(rep(1,3), rep(2, 5), rep(3, 7))
#' binnedRocData <- Df2RJafrocDataset(fp, tp)
#' lesDistr <- UtilLesDistr(binnedRocData)$Freq
#' retFit <- FitRsmRoc(binnedRocData, lesDistr)
#' 
#' 
#' ## Test with three interior data points
#' fp <- c(rep(1,12), rep(2, 5), rep(3, 3), rep(4, 5))
#' tp <- c(rep(1,3), rep(2, 5), rep(3, 7), rep(4, 10))
#' binnedRocData <- Df2RJafrocDataset(fp, tp)
#' lesDistr <- UtilLesDistr(binnedRocData)$Freq
#' retFit <- FitRsmRoc(binnedRocData, lesDistr)
#' 
#' ## test for TONY data, i = 2 and j = 3 
#' ## only case permitting chisqure calculation
#' lesDistr <- UtilLesDistr(dataset01)$Freq
#' rocData <- DfFroc2Roc(dataset01)
#' retFit <- FitRsmRoc(rocData, lesDistr, trt = 2, rdr = 3)
#' ## print(retFit$fittedPlot)
#' ## retFit$ChisqrFitStats
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
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#'  
#' @importFrom bbmle mle2
#' @importFrom stats lm
#' @importFrom binom binom.confint
#' 
#' @export
#'
### Why are parameter values always the same? 10/25/2018; added following comment from documentation mle2
### Method "BFGS" is a quasi-Newton method (also known as a variable metric algorithm), 
### specifically that published simultaneously in 1970 by Broyden, Fletcher, Goldfarb and 
### Shanno. This uses function values and gradients to build up a picture of the surface to be optimized. 
### Note: this implies it does not involve random search in parameter space, so should get same 
### results every time, regardless of seed, as observed.

FitRsmRoc <- function(binnedRocData, lesDistr, trt = 1, rdr = 1){ 
  # since the ROC dataset is sometimes derived from a FROC dataset with multiple lesions, 
  # lesDistr has to be supplied externally

  errorMsg <- ""  # keep track of any generated warnings
  
  if (missing(lesDistr)) stop("FitRsmRoc needs the lesDistr argument")
  
  maxLambda <- RJafrocEnv$maxLambda
  minLambda <- RJafrocEnv$minLambda
  maxNu <- RJafrocEnv$maxNu
  minNu <- RJafrocEnv$minNu
  maxMu <- RJafrocEnv$maxRsmMu
  minMu <- RJafrocEnv$minMu
  class(lesDistr) <- "numeric"
  
  # following line no longer needed as lesDist is now a 1D vector
  #lesDistr <- UtilLesDistr(lesDistr) # convert to internal 2D form
  
  fp <- binnedRocData$ratings$NL[trt,rdr,,1];fp <- fp[fp != -Inf]
  tp <- binnedRocData$ratings$LL[trt,rdr,,1]
  plotStep <- 0.01
  plotZeta <- seq(from = -3, to = 10, by = plotStep)
  
  ret1 <- UtilBinCountsOpPts(binnedRocData, trt, rdr) 
  fpf <- ret1$fpf
  tpf <- ret1$tpf
  fpCounts <- ret1$fpCounts
  tpCounts <- ret1$tpCounts
  # K1 <- sum(fpCounts);K2 <- sum(tpCounts)
  temp <- isDataDegenerate (fpf, tpf)
  msg <- temp$msg
  if (msg != "") errorMsg <- paste0(errorMsg, msg)
  if (temp$ret) {
    if (max(tpf) > max(fpf)) { # left or top boundary of ROC square
      mu <- maxMu # technically infinity; but then vertical line does not plot
      lambda <- -log(1 - fpf[length(fpf)]) # dpc
      nu <- max(tpf)
      fpfPred <- sapply(plotZeta, xROC_cpp, lambda = lambda)
      tpfPred <- sapply(plotZeta, yROC_cpp, mu = mu, lambda = lambda,
                        nu = nu, lesDistr = lesDistr)
      fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred, method = "RSM")
      return(list(
        mu = maxMu,
        lambda = minLambda,
        nu = max(tpf),
        zetas = NA,
        AUC = (1+max(tpf))/2, 
        StdAUC = 0,
        NLLIni = NA,
        NLLFin = NA,
        ChisqrFitStats = list(NA,NA,NA),
        covMat = NA,
        fittedPlot = fittedPlot,
        errorMsg = errorMsg
      ))
    } else { # right or bottom boundary of ROC square
      mu <- minMu # 
      lambda <- maxLambda # dpc
      nu <- minNu
      fpfPred <- sapply(plotZeta, xROC_cpp, lambda = lambda)
      tpfPred <- sapply(plotZeta, yROC_cpp, mu = mu, lambda = lambda, 
                        nu = nu, lesDistr = lesDistr)
      fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred, method = "RSM")
      return(list(
        mu = minMu,
        lambda = maxLambda,
        nu = minNu,
        zetas = NA,
        AUC = (1+max(tpf))/2, 
        StdAUC = 0,
        NLLIni = NA,
        NLLFin = NA,
        ChisqrFitStats = list(NA,NA,NA),
        covMat = NA,
        fittedPlot = fittedPlot, 
        errorMsg = errorMsg
      ))
    }
  }

  retCbm <- FitCbmRoc(binnedRocData, trt = trt, rdr = rdr)
  aucCbm <- retCbm$AUC
  muIni <- retCbm$mu
  lambdaIni <- -log(1 - fpf[length(fpf)]) 
  nuIni <- retCbm$alpha 
  zetasIni <- retCbm$zetas
  mu <- muIni; lambda <- lambdaIni;nu <- nuIni;zetas <- zetasIni
  
  muFwd <- ForwardValue(mu, minMu, maxMu)
  lambdaFwd <- ForwardValue(lambda, minLambda, maxLambda)
  nuFwd <- ForwardValue(nu, minNu, maxNu)
  zetaFwd <- ForwardZetas(zetas)
  
  parameters <- c(muFwd, lambdaFwd, nuFwd, zetaFwd)
  namesVector <- paste0("zetaFwd", 1:length(zetaFwd))
  names(parameters) <- c("muFwd" ,"lambdaFwd", "nuFwd", namesVector)
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
  lambda <- InverseValue(ret@coef[2], minLambda, maxLambda)
  nu <- InverseValue(ret@coef[3], minNu, maxNu)
  zetas <- InverseZetas(ret@coef[4:length(ret@coef)])
  
  # 11/30/20
  retx <- Util2Intrinsic(mu, lambda, nu)
  lambda_i <- retx$lambda_i
  nu_i <- retx$nu_i
  
  NLLFin <- ret@min
  
  AUC <- UtilAnalyticalAucsRSM(mu, lambda, nu, zeta1 <- -Inf, lesDistr)$aucROC # 11/30/20
  ## following checks out
  ##temp <- tempAucRSM (c(ret@coef[1], ret@coef[2], ret@coef[3]), lesDistr)  
  
  covMat <- vcov[1:3,1:3]
  StdAUC <- StdDevRsmAuc(ret@coef[1], ret@coef[2], ret@coef[3], covMat, lesDistr = lesDistr) ## !!!dpc!!! looks right; can it be proved?  
  
  ChisqrFitStats <- ChisqrGoodnessOfFit(fpCounts, tpCounts,
                                        parameters = c(mu,lambda,nu,zetas), model = "RSM", lesDistr)
  
  fpfPred <- sapply(plotZeta, xROC_cpp, lambda = lambda)
  tpfPred <- sapply(plotZeta, yROC_cpp, mu = mu, lambda = lambda, 
                    nu = nu, lesDistr = lesDistr)
  fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred, method = "RSM")
  
  # calculate covariance matrix using un-transformed variables
  namesVector <- c(c("mu", "lambda", "nu"), paste0("zeta", 1:length(zetas)))
  parameters <- c(list(mu, lambda, nu), as.list(zetas))
  names(parameters) <- namesVector
  
  RSMNLLAddNoTransf <- AddArguments2(RSMNLLNoTransf, length(zetas))
  
  # ret <- mle2(RSMNLLAddNoTransf, start = parameters, 
  #             upper = c(mu = 10, lambda = 20, nu = 0.9999, zeta1 = 3, zeta2 = 3, zeta3 = 3, zeta4 = 3), 
  #             method = "L-BFGS-B", 
  #             data = list(fb = fpCounts, tb = tpCounts, lesDistr = lesDistr))
  # 
  # ret <- mle2(RSMNLLAddNoTransf, start = parameters, fixed = as.vector("nu"),
  #             method = "BFGS", 
  #             data = list(fb = fpCounts, tb = tpCounts, lesDistr = lesDistr))
  # 
  
  if ((nu < 0.98) && (nu > 0.02) && (mu > 0.1)){
    
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
    lambda = lambda,
    nu = nu,
    zetas = zetas,
    AUC = AUC, 
    StdAUC = StdAUC,
    NLLIni = NLLIni,
    NLLFin = NLLFin,
    ChisqrFitStats = ChisqrFitStats,
    covMat = covMat,
    fittedPlot = fittedPlot, 
    errorMsg = errorMsg
  ))
}




###############################################################################
RSMNLL <- function(muFwd, lambdaFwd, nuFwd, fb, tb, lesDistr){
  maxLambda <- RJafrocEnv$maxLambda
  minLambda <- RJafrocEnv$minLambda
  maxNu <- RJafrocEnv$maxNu
  minNu <- RJafrocEnv$minNu
  maxMu <- RJafrocEnv$maxRsmMu
  minMu <- RJafrocEnv$minMu
  
  mu <- InverseValue(muFwd, minMu, maxMu)
  lambda <- InverseValue(lambdaFwd, minLambda, maxLambda)
  nu <- InverseValue(nuFwd, minNu, maxNu)
  allParameters <- names(formals())
  zetaPos <- grep("zetaFwd", allParameters)
  zetas <- unlist(mget(allParameters[zetaPos]))
  zetas <- InverseZetas(zetas)
  
  return(RsmInner(mu, lambda, nu, lesDistr, zetas, fb, tb))
}



###############################################################################
# RSM paradigm negative of log likelihood function, without transformations
RSMNLLNoTransf <- function (mu, lambda, nu, fb, tb, lesDistr){
  
  allParameters <- names(formals())
  zetaPos <- regexpr("zeta", allParameters)
  zetas <- unlist(mget(allParameters[which(zetaPos == 1)]))
  L <- RsmInner(mu, lambda, nu, lesDistr, zetas, fb, tb)
  return(L)
  
}



###############################################################################
# inputs are transformed parameters; covMat is also wrt trans. parameters
StdDevRsmAuc<-function(muFwd,lambdaFwd,nuFwd,covMatFwd, lesDistr)
{
  derivsFwd <- jacobian(func = tempAucRSM, c(muFwd,lambdaFwd,nuFwd), lesDistr = lesDistr) # this is for transformed variables
  VarAz <- derivsFwd %*% covMatFwd %*% t(derivsFwd)
  if (VarAz < 0 || is.na(VarAz)) return(NA) else return(sqrt(VarAz))
}



###############################################################################
tempAucRSM <- function (forwardParms, lesDistr = lesDistr){
  maxLambda <- RJafrocEnv$maxLambda
  minLambda <- RJafrocEnv$minLambda
  maxNu <- RJafrocEnv$maxNu
  minNu <- RJafrocEnv$minNu
  maxMu <- RJafrocEnv$maxRsmMu
  minMu <- RJafrocEnv$minMu
  
  mu <- InverseValue(forwardParms[1], minMu, maxMu)
  lambda <- InverseValue(forwardParms[2], minLambda, maxLambda)
  nu <- InverseValue(forwardParms[3], minNu, maxNu)
  
  maxFPF <- xROC_cpp(-20, lambda)
  maxTPF <- yROC_cpp(-20, mu, lambda, nu, lesDistr)
  AUC <- integrate(y_ROC_FPF_cpp, 0, maxFPF, mu = mu, lambda = lambda, nu = nu, 
                   lesDistr = lesDistr)$value
  
  AUC <- AUC + (1 + maxTPF) * (1 - maxFPF) / 2
  
  return (AUC)
}



