#' Fit the binormal model to selected treatment and reader in an ROC dataset
#' 
#' @description Fit the binormal model-predicted ROC curve for a dataset. 
#'    This is the R equivalent of ROCFIT or RSCORE
#' 
#' @param dataset The ROC dataset
#' @param trt The desired treatment, default is 1
#' @param rdr The desired reader, default is 1
#' 
#' @return The returned value is a list with the following elements:
#' @return \item{a}{The mean of the diseased distribution; 
#'    the non-diseased distribution is assumed to have zero mean} 
#' @return \item{b}{The standard deviation of the non-diseased 
#'    distribution. The diseased distribution is assumed to have 
#'    unit standard deviation} 
#' @return \item{zetas}{The binormal model cutoffs, zetas or thresholds} 
#' @return \item{AUC}{The binormal model fitted ROC-AUC} 
#' @return \item{StdAUC}{The standard deviation of AUC} 
#' @return \item{NLLIni}{The initial value of negative LL} 
#' @return \item{NLLFin}{The final value of negative LL} 
#' @return \item{ChisqrFitStats}{The chisquare goodness of fit results} 
#' @return \item{covMat}{The covariance matrix of the parameters} 
#' @return \item{fittedPlot}{A \pkg{ggplot2} object containing the 
#'    fitted operating characteristic along with the empirical operating 
#'    points. Use \code{print()} to display the object} 
#' 
#' @details 
#' In the binormal model ratings (more accurately the latent decision variables) 
#'    from diseased cases are sampled from \eqn{N(a,1)} while ratings for 
#'    non-diseased cases are sampled from \eqn{N(0,b^2)}. To avoid clutter error 
#'    bars are only shown for the lowest and uppermost operating points. An FROC
#'    dataset is internally converted to a highest rating inferred ROC dataset. To
#'    many bins containing zero counts will cause the algorithm to fail; so be sure
#'    to bin the data appropriately to fewer bins, where each bin has at least one
#'    count.  
#' 
#' 
#' @examples
#' \donttest{
#' ## Test with an included ROC dataset
#' retFit <- FitBinormalRoc(dataset02);print(retFit$fittedPlot)
#' 
#' 
#' ## Test with an included FROC dataset; it needs to be binned
#' ## as there are more than 5 discrete ratings levels
#' binned <- DfBinDataset(dataset05, desiredNumBins = 5, opChType = "ROC")
#' retFit <- FitBinormalRoc(binned);print(retFit$fittedPlot)
#' 
#' 
#' ## Test with single interior point data
#' fp <- c(rep(1,7), rep(2, 3))
#' tp <- c(rep(1,5), rep(2, 5))
#' dataset <- Df2RJafrocDataset(fp, tp)
#' retFit <- FitBinormalRoc(dataset);print(retFit$fittedPlot)
#' 
#' ## Test with two interior data points
#' fp <- c(rep(1,7), rep(2, 5), rep(3, 3))
#' tp <- c(rep(1,3), rep(2, 5), rep(3, 7))
#' dataset <- Df2RJafrocDataset(fp, tp)
#' retFit <- FitBinormalRoc(dataset);print(retFit$fittedPlot)
#'
#' ## Test with TONY data for which chisqr can be calculated
#' ds <- DfFroc2Roc(dataset01)
#' retFit <- FitBinormalRoc(ds, 2, 3);print(retFit$fittedPlot)
#' retFit$ChisqrFitStats
#'  
#' ## Test with included degenerate ROC data
#' retFit <- FitBinormalRoc(datasetDegenerate);print(retFit$fittedPlot)
#' }
#' 
#'  
#' 
#' @references 
#' Dorfman DD, Alf E (1969) Maximum-Likelihood Estimation of Parameters of Signal-Detection Theory and Determination of Confidence Intervals - 
#' Rating-Method Data, Journal of Mathematical Psychology 6, 487-496.
#'
#' Grey D, Morgan B (1972) Some aspects of ROC curve-fitting: normal and logistic models. Journal of Mathematical Psychology 9, 128-139.
#'
#' @importFrom bbmle mle2
#' @importFrom stats lm
#' @importFrom binom binom.confint
#' @export
#' 
FitBinormalRoc <- function(dataset, trt = 1, rdr = 1){
  if (dataset$dataType == "FROC") dataset <- DfFroc2Roc(dataset)
  minA <- RJafrocEnv$minA
  maxA <- RJafrocEnv$maxA
  minB <- RJafrocEnv$minB
  maxB <- RJafrocEnv$maxB
  minZeta <- RJafrocEnv$minZeta 
  maxZeta <- RJafrocEnv$maxZeta 
  
  fp <- dataset$NL[trt,rdr,,1];fp <- fp[fp != -Inf]
  tp <- dataset$LL[trt,rdr,,1]
  lenZetas <- length(unique(c(fp, tp))) - 1
  zetas <- array(dim = lenZetas)
  plotZeta <- seq(-3, 8, by = 0.1)
  
  ret1 <- RawOpPtsROC2ROC(fp, tp) 
  fpCounts <- ret1$fpCounts;tpCounts <- ret1$tpCounts;fpf <- ret1$fpf;tpf <- ret1$tpf
  
  if (isDataDegenerate(fpf, tpf)) {
    warning("Data is degenerate; binormal model cannot fit it unambiguously: use CBM or RSM method")
    return(list(
      mu = NA,
      alpha = NA,
      zetas = NA,
      AUC = NA,
      StdAUC = NA,
      NLLIni = NA,
      NLLFin = NA,
      covMat = NA,
      fittedPlot = NA
    ))
  }
  
  if (length(fpf) != 1) {
    phiInvFpf <- qnorm(fpf)
    phiInvTpf <- qnorm(tpf)
    phiInvFpf <- qnorm(fpf); phiInvFpf[phiInvFpf == -Inf] <- minZeta; phiInvFpf[phiInvFpf == Inf] <- maxZeta
    phiInvTpf <- qnorm(tpf); phiInvTpf[phiInvTpf == -Inf] <- minZeta; phiInvTpf[phiInvTpf == Inf] <- maxZeta
    fit <- lm(phiInvTpf ~ phiInvFpf) # straight line fit method of estimating a and b
    aIni <- fit$coefficients[[1]] # these is the initial estimate of a 
    bIni <- fit$coefficients[[2]] # these is the initial estimate of b
    # thresholds can be estimated by by applying inverse function to Eqn. xx and solving for zetas
    zetaNorIni <- rev(-phiInvFpf) # see Eqn. xx
    zetaAbnIni <- rev((aIni - phiInvTpf)/bIni) # see Eqn. xx
    zetasIni <- (zetaNorIni + zetaAbnIni) / 2 # average the two estimates
  } else {
    aIni <- 1
    bIni <- 1
    zetasIni <- 0
  }
  
  a <- aIni; b <- bIni;zetas <- zetasIni
  
  # while (1){  
    #param <- c(a, b, zetas)
    namesVector <- c(c("aFwd", "bFwd"), paste0("zetaFwd", 1:length(zetas)))
    aFwd <- ForwardValue(a, minA, maxA)
    bFwd <- ForwardValue(b, minB, maxB)
    zetaFwd <- ForwardZetas(zetas)
    parameters <- c(list(aFwd, bFwd), as.list(zetaFwd))
    names(parameters) <- namesVector
    
    BMNLLNew <- AddArguments(BMNLL, length(zetas))
    
    ret <- mle2(BMNLLNew, start = parameters, method = "BFGS", eval.only = TRUE,
                data = list(fi = fpCounts, ti = tpCounts))
    
    NLLIni <- ret@min
    
    ret <- mle2(BMNLLNew, start = parameters, method = "BFGS", 
                data = list(fi = fpCounts, ti = tpCounts))
    
    if (ret@details$convergence != 0) stop("mle2 error in binormal fit")
    
    NLLFin <- ret@min
    
    vcov <- ret@vcov
    
    a <- InverseValue(ret@coef[1], minA, maxA)
    b <- InverseValue(ret@coef[2], minB, maxB)
    zetas <- InverseZetas(ret@coef[3:length(ret@coef)])
    
    AUC <- pnorm(a / sqrt(1 + b^2))
    
    covMat <- vcov[1:2,1:2]
    StdAUC <- StdDevBinormalAuc1(ret@coef[1], ret@coef[2], covMat) ## !!!dpc!!! looks right; can it be proved?  
    ChisqrFitStats <- ChisqrGoodnessOfFit(fpCounts, tpCounts, 
                                            parameters = c(a,b,zetas), model = "BINORMAL")
    
    fpfPred <- 1 - pnorm(plotZeta)
    tpfPred <- pnorm(a - b * plotZeta)
    fittedPlot <- genericPlotROC (fp, tp, fpfPred, tpfPred)
    
    # calculate covariance matrix using un-transformed variables
    namesVector <- c(c("a", "b"), paste0("zeta", 1:length(zetas)))
    parameters <- c(list(a, b), as.list(zetas))
    names(parameters) <- namesVector
    
    BMNLLNewNoTransf <- AddArguments2(BMNLLNoTransf, length(zetas))
    
    ret <- mle2(BMNLLNewNoTransf, start = parameters, method = "BFGS", 
                data = list(fi = fpCounts, ti = tpCounts))
    
    # NLLChk <- ret@min
    
  #   if (NLLChk < NLLFin) {
  #     if (abs(NLLChk - NLLFin)/NLLFin > 1e-6) next else break
  #   } else break
  #   
  # }
  
  covMat <- ret@vcov 
  
  return(list(
    a = a,
    b = b,
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


BMNLL <- function(aFwd, bFwd, fi, ti){
  minA <- RJafrocEnv$minA
  maxA <- RJafrocEnv$maxA
  minB <- RJafrocEnv$minB
  maxB <- RJafrocEnv$maxB
  
  a <- InverseValue(aFwd, minA, maxA)
  b <- InverseValue(bFwd, minB, maxB)
  allParameters <- names(formals())
  zetaPos <- regexpr("zeta", allParameters)
  zetaFwd <- unlist(mget(allParameters[which(zetaPos == 1)]))
  zeta <- InverseZetas(zetaFwd)
  
  zetas <- c(-Inf, zeta, Inf)
  L <- BMNLLInner(a, b, zetas, fi, ti)
  return(-L)
}

# ROC paradigm negative of log likelihood function, without transformations
BMNLLNoTransf <- function (a, b, fi, ti){
  
  allParameters <- names(formals())
  zetaPos <- regexpr("zeta", allParameters)
  zetas <- unlist(mget(allParameters[which(zetaPos == 1)]))
  zetas <- c(-Inf, zetas, Inf)
  
  L <- BMNLLInner(a, b, zetas, fi, ti)
  return(-L)
  
}


# analytical derivatives using Cov derived using no transformations
StdDevBinormalAuc <- function (a, b,Cov)
{
  derivWrtA <- dnorm (a/sqrt(1+b^2))/sqrt(1+b^2)
  derivWrtB <- dnorm (a/sqrt(1+b^2))*(-a*b*(1+b^2)^(-1.5)) 
  VarAUC <- (derivWrtA)^2*Cov[1,1]+(derivWrtB)^2*Cov[2,2] +
    2 * derivWrtA*derivWrtB*Cov[1,2]
  return (sqrt(VarAUC))  
}


# inputs are transformed parameters; covMat is also wrt trans. parameters
StdDevBinormalAuc1 <- function (aFwd, bFwd,covMatFwd)
{
  minA <- RJafrocEnv$minA
  maxA <- RJafrocEnv$maxA
  minB <- RJafrocEnv$minB
  maxB <- RJafrocEnv$maxB
  
  derivsFwd <- jacobian(func = tempAucBinormal, c(aFwd,bFwd))
  VarAz <- derivsFwd %*% covMatFwd %*% t(derivsFwd)
  
  if (VarAz < 0 || is.na(VarAz)) return(NA) else return(sqrt(VarAz))
}


tempAucBinormal <- function (fwdParms){
  minA <- RJafrocEnv$minA
  maxA <- RJafrocEnv$maxA
  minB <- RJafrocEnv$minB
  maxB <- RJafrocEnv$maxB
  
  a <- InverseValue(fwdParms[1], minA, maxA)
  b <- InverseValue(fwdParms[2], minB, maxB)
  
  AUC <- pnorm(a / sqrt(1 + b^2))
  
  return (AUC)
}




