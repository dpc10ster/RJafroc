#' Fit the contaminated binormal model (CBM) to selected treatment and 
#'    reader in an ROC dataset
#'
#' @description Fit the CBM-predicted ROC curve for specified treatment and reader
#'
#' @param dataset The dataset containing the data
#' @param trt The desired treatment, default is 1
#' @param rdr The desired reader, default is 1
#'
#'
#' @return The return value is a list with the following elements:
#' @return \item{mu}{The mean of the visible diseased distribution (the non-diseased)
#'    has zero mean}
#' @return \item{alpha}{The proportion of diseased cases where the disease is visible}
#' @return \item{zetas}{The cutoffs, zetas or thresholds}
#' @return \item{AUC}{The AUC of the fitted ROC curve}
#' @return \item{StdAUC}{The standard deviation of AUC}
#' @return \item{NLLIni}{The initial value of negative LL}
#' @return \item{NLLFin}{The final value of negative LL}
#' @return \item{ChisqrFitStats}{The chisquare goodness of fit results}
#' @return \item{covMat}{The covariance matrix of the parameters}
#' @return \item{fittedPlot}{A \pkg{ggplot2} object containing the fitted 
#'    operating characteristic along with the empirical operating points. 
#'    Use \code{print()} to display the object}
#'
#' @details
#' In  CBM ratings from diseased cases
#'    are sampled from a mixture distribution: (1) with integrated area \eqn{alpha} 
#'    distributed \eqn{N(\eqn{mu},1)} and (2) from a distribution with 
#'    integrated area \eqn{1-alpha} distributed\eqn{N(0,1)}. Ratings 
#'    for non-diseased cases are sampled from \eqn{N(0,1)}. The \code{ChisqrFitStats} 
#'    consists of a list containing the chi-square value, the p-value and the degrees 
#'    of freedom. 
#'
#'@note This algorithm is more robust than the binormal model.
#'
#' @examples
#' 
#' \dontrun{
#' ## CPU time 8.7 sec on Ubuntu (#13)
#' ## Test with included ROC data
#' retFit <- FitCbmRoc(dataset02);print(retFit$fittedPlot)
#'
#' ## Test with included degenerate ROC data (yes! CBM can fit such data)
#' retFit <- FitCbmRoc(datasetDegenerate);print(retFit$fittedPlot)
#'
#' ## Test with single interior point data
#' fp <- c(rep(1,7), rep(2, 3))
#' tp <- c(rep(1,5), rep(2, 5))
#' dataset <- Df2RJafrocDataset(fp, tp)
#' retFit <- FitCbmRoc(dataset);print(retFit$fittedPlot)
#'
#' ## Test with two interior data points
#' fp <- c(rep(1,7), rep(2, 5), rep(3, 3))
#' tp <- c(rep(1,3), rep(2, 5), rep(3, 7))
#' dataset <- Df2RJafrocDataset(fp, tp)
#' retFit <- FitCbmRo
#' c(dataset);print(retFit$fittedPlot)
#'
#' ## Test with included ROC data (some bins have zero counts)
#' retFit <- FitCbmRoc(dataset02, 2, 1);print(retFit$fittedPlot)
#' 
#' ## Test with TONY data for which chisqr can be calculated
#' ds <- DfFroc2Roc(dataset01)
#' retFit <- FitCbmRoc(ds, 2, 3);print(retFit$fittedPlot)
#' retFit$ChisqrFitStats
#' }
#' 
#' @references
#' Dorfman DD, Berbaum KS (2000) A contaminated binormal model for ROC data: Part II. A formal model,
#' Acad Radiol, 7:6, 427--437.
#'
#' @importFrom bbmle mle2
#' @importFrom numDeriv hessian jacobian
#' @importFrom Rcpp cppFunction
#' @export
#
# dpc changes; July 2017
# removed I, J, i and j, and reduced to single dataset analysis; renamed old code with "IJ" appended
# moved plotting part to a function call, genericPlotROC(), that is shared by all ROC fitting method
# printed out full covariance matrix
# added variance of AUC
# removed (1,1) plotted point
# added initial and final values of NLL
# added capability of handling degenerate datasets
# checked capability to handle single interior operating point
# there was an error in forward zetas and inverse zetas function
# which had no effect on previous analysis with more than one operating points
#
#' @importFrom stats uniroot

FitCbmRoc <- function(dataset, trt = 1, rdr = 1){
  # minZeta <- RJafrocEnv$minZeta
  # maxZeta <- RJafrocEnv$maxZeta
  minMu <- RJafrocEnv$minMu
  maxMu <- RJafrocEnv$maxMu
  minAlpha <- RJafrocEnv$minAlpha
  maxAlpha <- RJafrocEnv$maxAlpha
  
  dataset <- DfExtractDataset(dataset, trts = trt, rdrs = rdr)
  aucArray <- UtilFigureOfMerit(dataset, FOM = "Wilcoxon")
  maxAUC <- max(aucArray)
  while (pnorm(maxMu / sqrt(2)) <= aucArray){
    maxMu <- qnorm(maxAUC) * sqrt(2) + 0.5
  }
  
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  
  fp <- dataset$NL[1,1,,1]
  tp <- dataset$LL[1,1,,1]
  plotStep <- 0.01
  plotZeta <- seq(from = -3, to = 10, by = plotStep)
  K2 <- length(tp)
  K1 <- length(fp) - K2
  fp <- fp[1:K1]
  
  ret1 <- UtilBinCountsOpPts(dataset)
  fpf <- ret1$fpf;tpf <- ret1$tpf;fpCounts <- ret1$fpCounts;tpCounts <- ret1$tpCounts
  if (any(fpf == 0)) InfFlag <- TRUE else InfFlag <- FALSE
  
  if (isDataDegenerate(fpf, tpf)) {
    if (max(fpf) >= max(tpf)) {
      mu <- 0; alpha <- 0; AUC <- 0.5
      #fpfPred <- c(0,1);tpfPred <- c(0,1)
      fpfPred <- 1 - pnorm(plotZeta)
      tpfPred <- (1 - alpha) * (1 - pnorm(plotZeta)) + alpha * (1 - pnorm(plotZeta, mean = mu))
      fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred)
      #fittedPlot <- genericPlotCbmROC (mu, alpha, modalityID, readerID, fpf, tpf, K1, K2)
    } else {
      alpha <- max(tpf)
      mu <- maxMu
      AUC <- 1
      ret <- Df2RJafrocDataset(fp, tp)
      fpfPred <- 1 - pnorm(plotZeta)
      tpfPred <- (1 - alpha) * (1 - pnorm(plotZeta)) + alpha * (1 - pnorm(plotZeta, mean = mu))
      fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred)
      #fittedPlot <- genericPlotCbmROC (mu, alpha, modalityID, readerID, fpf, tpf, K1, K2)
    }
    return(list(
      mu = mu,
      alpha = alpha,
      zetas = NA,
      AUC = AUC,
      StdAUC = NA,
      NLLIni = NA,
      NLLFin = NA,
      ChisqrFitStats = list(NA,NA,NA),
      covMat = NA,
      fittedPlot = fittedPlot
    ))
  }
  
  iniPntIndx <- which(tpf > fpf)
  iniPntIndx <- iniPntIndx[length(iniPntIndx)]
  alphaIni <- max(min(1 - (1 - tpf[iniPntIndx]) / (1 - fpf[iniPntIndx]), maxAlpha - 0.01), minAlpha + 0.01) ## !dpc! OK??
  muIni <- qnorm(max(min((aucArray - (1 - alphaIni)/2) / alphaIni, 0.99), 0.51)) * sqrt(2) ## !dpc! OK? ??
  fpCountsCum <- cumsum(fpCounts)[1:(length(fpCounts) - 1)]
  tpCountsCum <- cumsum(tpCounts)[1:(length(tpCounts) - 1)]

  zetaNorIni <- qnorm(fpCountsCum[which(fpCountsCum != 0)]/K1)
  zetaAbnIni <- sapply(tpCountsCum[which(tpCountsCum != 0)]/K2, IniZetaAbn, alpha = alphaIni, mu = muIni)
  l1 <- length(zetaNorIni)
  l2 <- length(zetaAbnIni)
  if (l1 > l2) {
    zetaAbnIni <- c(rep(-Inf, l1-l2), zetaAbnIni)
    zetaIni <- array(dim = l1) 
    for (el in 1:l1) {
      X <- zetaNorIni[el]
      Y <- zetaAbnIni[el]
      XY <- c(X,Y)
      XYk <- XY[is.finite(XY)]
      zetaIni[el] <- mean(XYk)
    }
  } else if (l2 > l1) {
    stop("untested code section in FitCbmRoc")
    zetaNorIni <- c(rep(-Inf, l2-l1), zetaNorIni)
    zetaIni <- array(dim = l2) 
    for (el in 1:l2) {
      X <- zetaNorIni[el]
      Y <- zetaAbnIni[el]
      XY <- c(X,Y)
      XYk <- XY[is.finite(XY)]
      zetaIni[el] <- mean(XYk)
    }
  } else {
    zetaIni <- array(dim = l2) 
    for (el in 1:l2) {
      X <- zetaNorIni[el]
      Y <- zetaAbnIni[el]
      XY <- c(X,Y)
      XYk <- XY[is.finite(XY)]
      zetaIni[el] <- mean(XYk)
    }
  }

  zetasIni <- sort(zetaIni)
  
  mu <- muIni; alpha <- alphaIni;zetas <- zetasIni
  
  muFwd <- ForwardValue(mu, minMu, maxMu)
  alphaFwd <- ForwardValue(alpha, minAlpha, maxAlpha)
  zetaFwd <- ForwardZetas(zetas)
  parameters <- c(list(muFwd, alphaFwd), as.list(zetaFwd))
  namesVector <- c("muFwd", "alphaFwd", paste0("zetaFwd", 1:length(zetas)))
  names(parameters) <- namesVector
  
  CBMNLLNew <- AddArguments(CBMNLL, length(zetas))
  ret <- mle2(CBMNLLNew, start = parameters, method = "BFGS", eval.only = TRUE,
              data = list(fi = fpCounts, ti = tpCounts, maxMu = maxMu))
  NLLIni <- ret@min
  
  ret <- mle2(CBMNLLNew, start = parameters, method = "BFGS",
              data = list(fi = fpCounts, ti = tpCounts, maxMu = maxMu))
  if (ret@details$convergence != 0) stop("mle2 error in Cbm fit")
  NLLFin <- ret@min
  
  vcov <- ret@vcov
  
  mu <- InverseValue(ret@coef[1], minMu, maxMu)
  alpha <- InverseValue(ret@coef[2], minAlpha, maxAlpha)
  zetas <- InverseZetas(ret@coef[3:length(ret@coef)])
  
  param <- c(mu, alpha, zetas)
  fixParam <- c(which(param[1] > (maxMu - 0.01)),
                1+which(param[2] > (maxAlpha - 0.01)))
  fixList <- list();afterIndex <- c()
  if (length(fixParam) > 0){
    for (p in fixParam){
      if (p == 1){
        mu <- maxMu - 0.01
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(muFwd = ForwardValue(mu, minMu, maxMu)))
      } else if (p == 2){
        alpha <- maxAlpha - 0.01
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(alphaFwd = ForwardValue(alpha, minAlpha, maxAlpha)))
      }
    }
    muFwd <- ForwardValue(mu, minMu, maxMu)
    alphaFwd <- ForwardValue(alpha, minAlpha, maxAlpha)
    zetaFwd <- ForwardZetas(zetas)
    
    parameters <- c(list(muFwd, alphaFwd), as.list(zetaFwd))
    namesVector <- c("muFwd", "alphaFwd", paste0("zetaFwd", 1:length(zetas)))
    names(parameters) <- namesVector
    ret2 <- mle2(CBMNLLNew, start = parameters, method = "BFGS", fixed = fixList,
                 data = list(fi = fpCounts, ti = tpCounts, maxMu = maxMu))
    allCoef <- ret2@coef
    for (i in 1:length(afterIndex)) {
      allCoef <- append(allCoef, fixList[[i]], after = afterIndex[i])
    }
    names(allCoef) <- namesVector
    mu <- InverseValue(allCoef[1], minMu, maxMu)
    alpha <- InverseValue(allCoef[2], minAlpha, maxAlpha)
    zetas <- InverseZetas(allCoef[3:(2 + length(zetas))])
  }
  
  if (abs(mu-maxMu) < 0.1 || abs(alpha-1) < 0.1) nearDeg <- TRUE else nearDeg <- FALSE
  
  AUC <- 0.5 * (1 - alpha) + alpha * pnorm(mu/sqrt(2)) #dpc code 12/14/16
  ## x <- tempAucCBM(c(ret@coef[1],ret@coef[2])) ## same result
  if (!nearDeg) {
    covMat <- vcov[1:2,1:2]
    StdAUC <- StdDevCbmAuc(ret@coef[1], ret@coef[2], covMat) ## !!!dpc!!! looks right; can it be proved?
    ChisqrFitStats <- ChisqrGoodnessOfFit(fpCounts, tpCounts,
                                             parameters = c(mu,alpha,zetas), model = "CBM")
  } else {
    covMat <- NA
    StdAUC <- NA
    ChisqrFitStats <- NA
  }
  fpfPred <- 1 - pnorm(plotZeta)
  tpfPred <- (1 - alpha) * (1 - pnorm(plotZeta)) + alpha * (1 - pnorm(plotZeta, mean = mu))
  fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred)
  
  if (!nearDeg){
    # calculate covariance matrix using un-transformed variables
    namesVector <- c(c("mu", "alpha"), paste0("zeta", 1:length(zetas)))
    parameters <- c(list(mu, alpha), as.list(zetas))
    names(parameters) <- namesVector
    
    CBMNLLNewNoTransf <- AddArguments2(nLLCBMNoTransf, length(zetas))
    
    ret <- suppressWarnings(mle2(CBMNLLNewNoTransf, start = parameters, method = "BFGS",
                                 data = list(fi = fpCounts, ti = tpCounts)))
    covMat <- ret@vcov
  }else{
    covMat <- NA
    StdAUC <- NA
    ChisqrFitStats <- NA
  }
  
  return(list(
    mu = mu,
    alpha = alpha,
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


IniZetaAbn <- function(alpha, mu, tpCum){
  minZeta <- RJafrocEnv$minZeta
  maxZeta <- RJafrocEnv$maxZeta
  if (TPCumDiff(minZeta, alpha, mu, tpCum) >= 0){
    return(minZeta)
  }else if (TPCumDiff(maxZeta, alpha, mu, tpCum) <= 0){
    return(maxZeta)
  }else{
    return(uniroot(TPCumDiff, c(minZeta, maxZeta), alpha = alpha, mu = mu, tpCum = tpCum)$root)
  }
}



TPCumDiff <- function(zeta, alpha, mu, tpCum){
  return((1 - alpha) * pnorm(zeta) + alpha * pnorm(zeta - mu) - tpCum)
}


CBMNLL <- function(muFwd, alphaFwd, fi, ti, maxMu){
  minMu <- RJafrocEnv$minMu
  minAlpha <- RJafrocEnv$minAlpha
  maxAlpha <- RJafrocEnv$maxAlpha
  
  mu <- InverseValue(muFwd, minMu, maxMu)
  alpha <- InverseValue(alphaFwd, minAlpha, maxAlpha)
  allParameters <- names(formals())
  zetaPos <- regexpr("zeta", allParameters)
  zetaFwd <- unlist(mget(allParameters[which(zetaPos == 1)]))
  zeta <- InverseZetas(zetaFwd)
  if ((abs(zeta[1]) > 2) || (zeta[length(zeta)] > 10))  {
    return(1e10)
  }
  zetas <- c(-Inf, zeta, Inf)
  L <- CBMNLLInner(mu, alpha, zetas, fi, ti)
  return(-L)
}



# CBM paradigm negative of log likelihood function, without transformations
nLLCBMNoTransf <- function (mu, alpha, fi, ti){
  
  allParameters <- names(formals())
  zetaPos <- regexpr("zeta", allParameters)
  zetas <- unlist(mget(allParameters[which(zetaPos == 1)]))
  zetas <- c(-Inf, zetas, Inf)
  
  L <- CBMNLLInner(mu, alpha, zetas, fi, ti)
  return(-L)
  
}



# inputs are transformed parameters; covMat is also wrt trans. parameters
StdDevCbmAuc <- function (muFwd, alphaFwd,covMatFwd)
{
  derivsFwd <- jacobian(func = tempAucCBM, c(muFwd,alphaFwd))
  VarAz <- derivsFwd %*% covMatFwd %*% t(derivsFwd)
  
  # followng code gives much larger, unreasonable, variance
  # because forward transformed matrix cannot be combined with untransformed derivatives, DUMMY
  # mu <- InverseValue(muFwd, minMu, maxMu)
  # alpha <- InverseValue(alphaFwd, minAlpha, maxAlpha)
  #
  # derivWrtmu <- alpha*dnorm(mu/sqrt(2))/sqrt(2)
  # derivWrtalpha <- -0.5 + pnorm(mu/sqrt(2))
  #
  # VarAz <- (derivWrtmu)^2*covMatFwd[1,1]+(derivWrtalpha)^2*covMatFwd[2,2] +
  #   2*derivWrtmu*derivWrtalpha*covMatFwd[1,2]
  # end unreasonable
  
  # VarAz <- derivsFwd[1]^2*covMatFwd[1,1]+derivsFwd[2]^2*covMatFwd[2,2] +
  #   2*derivsFwd[1]*derivsFwd[2]*covMatFwd[1,2]
  # following line gives same result as longer expression above
  
  if (VarAz < 0 || is.na(VarAz)) return(NA) else return(sqrt(VarAz))
}


tempAucCBM <- function (fwdParms){
  minMu <- RJafrocEnv$minMu
  maxMu <- RJafrocEnv$maxMu
  minAlpha <- RJafrocEnv$minAlpha
  maxAlpha <- RJafrocEnv$maxAlpha
  
  mu <- InverseValue(fwdParms[1], minMu, maxMu)
  alpha <- InverseValue(fwdParms[2], minAlpha, maxAlpha)
  
  AUC <- 0.5 * (1 - alpha) + alpha * pnorm(mu/sqrt(2)) #dpc code 12/14/16
  
  return (AUC)
}




