#' Fit CORCBM to a paired ROC dataset
#'
#' @description Fit the Correlated Contaminated Binormal Model (CORCBM) 
#'    to a paired ROC dataset.
#'    The \strong{ROC} dataset has to be formatted as a 
#'    \strong{single treatment}, \strong{two-reader} dataset, even though the actual 
#'    pairing may be different, see details.
#'
#'
#' @param dataset A \strong{paired ROC} dataset
#'
#'
#' @return The return value is a list containing three objects:
#' @return \item{fitCorCbmRet}{list(\code{FPCounts},\code{TPCounts},
#' \code{muX},\code{muY},\code{alphaX},\code{alphaY},\code{rhoNor},
#' \code{rhoAbn2},\code{zetaX},\code{zetaY},\code{covMat},\code{fixParam})}
#' @return \item{stats}{list(\code{aucX},\code{aucX},\code{stdAucX},
#' \code{stdAucY},\code{stdErr},\code{areaStat},\code{areaPval})}
#' @return \item{fittedPlot}{The fitted plot with operating points, error bars, 
#'    for both conditions}
#'
#' @details The conditions (X, Y) can be two readers interpreting images in the same 
#'    treatment, the same reader interpreting images in different treatments, or 
#'    different readers interpreting images in 2 different treatments. Function 
#'    \code{\link{DfExtractCorCbmDataset}} can be used to construct a dataset suitable for 
#'    \code{FitCorCbmRoc}. With reference to the returned values, and assuming R bins 
#'    in condition X and L bins in conditon Y, 
#'    \code{FPCounts} is the R x L matrix containing the counts for non-diseased cases, 
#'    \code{TPCounts} is the R x L matrix containing the counts for diseased cases; 
#'    \code{muX},\code{muY},\code{alphaX},\code{alphaY},\code{rhoNor},\code{rhoAbn2} are 
#'    the CORCBM parameters; \code{aucX},\code{aucX} are the AUCs in the two conditions; 
#'    \code{stdAucX},\code{stdAucY} are the corresponding standard errors;\code{stdErr} 
#'    contains the standard errors of the parameters of the model; \code{areaStat}, 
#'    \code{areaPval},\code{covMat} are the area-statistic, the p-value and the covariance 
#'    matrix of the parameters. If a parameter approaches a limit, e.g., \code{rhoNor} 
#'    = 0.9999, it is held constant at near the limiting value and the covariance matrix 
#'    has one less dimension (along each edge) for each parameter that is held constant.
#'    The indices of the parameters held fixed are in \code{fitCorCbmRet$fixParam}.
#'
## following examples generate excessive CPU time NOTES on devtools::check_win_xx() 
## but it is instructive to execute them independently
## see also the publication referenced below
##
## dataset <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(4,7))
## ret <- FitCorCbmRoc(dataset)
## print(ret$fitCorCbmRet)
## print(ret$stats)
## print(ret$fittedPlot)
## 
## 
## ret <- FitCorCbmRoc(datasetBinned123)
## print(ret$fitCorCbmRet)
## print(ret$stats)
## print(ret$fittedPlot)
## Also try two other datasets ending with 124 and 125
#'
#'
#' @references
#' Zhai X, Chakraborty DP (2017) A bivariate contaminated binormal model for robust fitting of proper ROC
#' curves to a pair of correlated, possibly degenerate, ROC datasets. Medical Physics. 44(6):2207--2222.
#'
#'
#' @importFrom bbmle mle2
#' @importFrom Rcpp cppFunction
#' @importFrom mvtnorm pmvnorm
#' @importFrom stats cor
#'
#' @export
#'
#'
FitCorCbmRoc <- function(dataset){
  options(stringsAsFactors = FALSE) # check compatibility with new default for R 4.0.0
  
  if (dataset$dataType != "ROC") {
    stop("This program requires an ROC dataset")
  }

  minMu <- RJafrocEnv$minMu
  maxMu <- RJafrocEnv$maxMu
  minAlpha <- RJafrocEnv$minAlpha
  maxAlpha <- RJafrocEnv$maxAlpha
  minRho <- RJafrocEnv$minRho
  maxRho <- RJafrocEnv$maxRho

  aucArray <- UtilFigureOfMerit(dataset, FOM = "Wilcoxon")
  maxAUC <- max(aucArray)
  while (pnorm(maxMu / sqrt(2)) <= maxAUC){
    maxMu <- qnorm(maxAUC) * sqrt(2) + 0.5
  }

  I <- length(dataset$NL[,1,1,1])
  J <- length(dataset$NL[1,,1,1])
  K <- length(dataset$NL[1,1,,1])
  K2 <- length(dataset$LL[1,1,,1])
  K1 <- K - K2
  FP <- dataset$NL[1,,1:K1,1]
  TP <- dataset$LL[1,,,1]

  z1X <- FP[1, ];z1Y <- FP[2, ]
  z2X <- TP[1, ];z2Y <- TP[2, ]

  rhoNor <- cor(z1X, z1Y)
  rhoAbn <- cor(z2X, z2Y)

  iniParamX <- FitCbmRoc(dataset, 1, 1) # arm X of pairing
  muX <- iniParamX$mu
  zetaX <- iniParamX$zetas
  alphaX <- iniParamX$alpha

  iniParamY <- FitCbmRoc(dataset, 1, 2) # arm Y of pairing
  muY <- iniParamY$mu
  zetaY <- iniParamY$zetas
  alphaY <- iniParamY$alpha

  muXFwd <- ForwardValue(muX, minMu, maxMu)
  muYFwd <- ForwardValue(muY, minMu, maxMu)
  alphaXFwd <- ForwardValue(alphaX, minAlpha, maxAlpha)
  alphaYFwd <- ForwardValue(alphaY, minAlpha, maxAlpha)
  rhoNorFwd <- ForwardValue(rhoNor, minRho, maxRho)
  rhoAbn2Fwd <- ForwardValue(rhoAbn, minRho, maxRho)
  zetaXFwd <- ForwardZetas(zetaX)
  zetaYFwd <- ForwardZetas(zetaY)

  parameters <- c(list(muXFwd, muYFwd, alphaXFwd, alphaYFwd, rhoNorFwd, rhoAbn2Fwd),
                  as.list(zetaXFwd), as.list(zetaYFwd))
  namesVector <- c("muXFwd", "muYFwd", "alphaXFwd", "alphaYFwd","rhoNorFwd", "rhoAbn2Fwd")
  namesVector <- c(namesVector, paste0("zetaXFwd", 1:length(zetaXFwd)))
  namesVector <- c(namesVector, paste0("zetaYFwd", 1:length(zetaYFwd)))
  names(parameters) <- namesVector

  nBinsX <- length(unique(c(FP[1,], TP[1,])))
  nBinsY <- length(unique(c(FP[2,], TP[2,])))

  FPCounts <- array(dim = c(nBinsX, nBinsY))
  TPCounts <- array(dim = c(nBinsX, nBinsY))

  for (bX in 1:nBinsX){
    for (bY in 1:nBinsY){
      FPCounts[bX, bY] <- sum((z1X == bX & z1Y == bY))
      TPCounts[bX, bY] <- sum((z2X == bX & z2Y == bY))
    }
  }

  nLLCorCBMAdd <- AddZetaXFwd(nLLCorCBM, length(zetaX))
  nLLCorCBMAdd <- AddZetaYFwd(nLLCorCBMAdd, length(zetaY))

  ret <- mle2(nLLCorCBMAdd, start = parameters, eval.only = TRUE,
              method = "BFGS", data = list(FPCounts = FPCounts, TPCounts = TPCounts, maxMu = maxMu))

  NLLIni <- as.numeric(ret@min)
  
  ret <- mle2(nLLCorCBMAdd, start = parameters,
              method = "BFGS", data = list(FPCounts = FPCounts, TPCounts = TPCounts, maxMu = maxMu))
  NLLFin <- as.numeric(ret@min)

  allCoef1 <- ret@coef
  muX <- InverseValue(allCoef1[1], minMu, maxMu)
  muY <- InverseValue(allCoef1[2], minMu, maxMu)
  alphaX <- InverseValue(allCoef1[3], minAlpha, maxAlpha)
  alphaY <- InverseValue(allCoef1[4], minAlpha, maxAlpha)
  rhoNor <- InverseValue(allCoef1[5], minRho, maxRho)
  rhoAbn2 <- InverseValue(allCoef1[6], minRho, maxRho)
  zetaX <- InverseZetas(allCoef1[7:(6 + length(zetaX))])
  zetaY <- InverseZetas(allCoef1[(7 + length(zetaX)):length(allCoef1)])

  CorCBMLLNoTrnsAdd <- AddZetaX(nLLCorCBMNoTrns, length(zetaX))
  CorCBMLLNoTrnsAdd <- AddZetaY(CorCBMLLNoTrnsAdd, length(zetaY))

  param <- c(muX, muY, alphaX, alphaY, rhoNor, rhoAbn2, zetaX, zetaY)
  fixParam <- c(which(param[1:2] > (maxMu - 0.01)),
                2+which(param[3:4] > (maxAlpha - 0.01)),
                4+which(param[5:6] > (maxRho - 0.01)))
  if (length(fixParam) > 0){
    # some parameters are fixed
    fixList <- list()
    afterIndex <- c()
    for (p in fixParam){
      if (p == 1){
        muX <- maxMu - 0.01
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(muXFwd = ForwardValue(muX, minMu, maxMu)))
      } else if (p == 2){
        muY <- maxMu - 0.01
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(muYFwd = ForwardValue(muY, minMu, maxMu)))
      }else if (p == 3){
        alphaX <- maxAlpha - 0.01
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(alphaXFwd = ForwardValue(alphaX, minAlpha, maxAlpha)))
      }else if (p == 4){
        alphaY <- maxAlpha - 0.01
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(alphaYFwd = ForwardValue(alphaY, minAlpha, maxAlpha)))
      }else if (p == 5){
        rhoNor <- ifelse(test = rhoNor > 0, yes = 0.99, no = -0.99)
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(rhoNorFwd = ForwardValue(rhoNor, minRho, maxRho)))
      }else if (p == 6){
        rhoAbn2 <- ifelse(test = rhoAbn2 > 0, yes = 0.99, no = -0.99)
        afterIndex <- c(afterIndex, p - 1)
        fixList <- c(fixList, list(rhoAbn2Fwd = ForwardValue(rhoAbn2, minRho, maxRho)))
      }
    }
    muXFwd <- ForwardValue(muX, minMu, maxMu)
    muYFwd <- ForwardValue(muY, minMu, maxMu)
    alphaXFwd <- ForwardValue(alphaX, minAlpha, maxAlpha)
    alphaYFwd <- ForwardValue(alphaY, minAlpha, maxAlpha)
    rhoNorFwd <- ForwardValue(rhoNor, minRho, maxRho)
    rhoAbn2Fwd <- ForwardValue(rhoAbn2, minRho, maxRho)
    zetaXFwd <- ForwardZetas(zetaX)
    zetaYFwd <- ForwardZetas(zetaY)

    parameters <- c(list(muXFwd, muYFwd, alphaXFwd, alphaYFwd, rhoNorFwd, rhoAbn2Fwd),
                    as.list(zetaXFwd), as.list(zetaYFwd))
    namesVector <- c("muXFwd", "muYFwd", "alphaXFwd", "alphaYFwd","rhoNorFwd", "rhoAbn2Fwd")
    namesVector <- c(namesVector, paste0("zetaXFwd", 1:length(zetaXFwd)))
    namesVector <- c(namesVector, paste0("zetaYFwd", 1:length(zetaYFwd)))
    names(parameters) <- namesVector
    ret2 <- mle2(nLLCorCBMAdd, start = parameters, method = "BFGS", fixed = fixList,
                 data = list(FPCounts = FPCounts, TPCounts = TPCounts, maxMu = maxMu), trace = TRUE)
    NLLFin <- ret2@min
    allCoef2 <- ret2@coef
    for (i in 1:length(afterIndex)) {
      allCoef2 <- append(allCoef2, fixList[[i]], after = afterIndex[i])
    }
    names(allCoef2) <- namesVector
    muX <- InverseValue(allCoef2[1], minMu, maxMu)
    muY <- InverseValue(allCoef2[2], minMu, maxMu)
    alphaX <- InverseValue(allCoef2[3], minAlpha, maxAlpha)
    alphaY <- InverseValue(allCoef2[4], minAlpha, maxAlpha)
    rhoNor <- InverseValue(allCoef2[5], minRho, maxRho)
    rhoAbn2 <- InverseValue(allCoef2[6], minRho, maxRho)
    zetaX <- InverseZetas(allCoef2[7:(6 + length(zetaX))])
    zetaY <- InverseZetas(allCoef2[(7 + length(zetaX)):length(allCoef2)])

    parameters <- c(list(muX, muY, alphaX, alphaY, rhoNor, rhoAbn2),
                    as.list(zetaX), as.list(zetaY))
    namesVector <- c("muX", "muY", "alphaX", "alphaY","rhoNor", "rhoAbn2")
    namesVector <- c(namesVector, paste0("zetaX", 1:length(zetaX)))
    namesVector <- c(namesVector, paste0("zetaY", 1:length(zetaY)))
    names(parameters) <- namesVector

    ret3 <- mle2(CorCBMLLNoTrnsAdd, start = parameters, method = "BFGS", fixed = parameters[fixParam],
                 data = list(FPCounts = FPCounts, TPCounts = TPCounts))
  } else {
    # all parameters are used
    allCoef2 <- allCoef1
    names(allCoef2) <- namesVector
    muX <- InverseValue(allCoef2[1], minMu, maxMu)
    muY <- InverseValue(allCoef2[2], minMu, maxMu)
    alphaX <- InverseValue(allCoef2[3], minAlpha, maxAlpha)
    alphaY <- InverseValue(allCoef2[4], minAlpha, maxAlpha)
    rhoNor <- InverseValue(allCoef2[5], minRho, maxRho)
    rhoAbn2 <- InverseValue(allCoef2[6], minRho, maxRho)
    zetaX <- InverseZetas(allCoef2[7:(6 + length(zetaX))])
    zetaY <- InverseZetas(allCoef2[(7 + length(zetaX)):length(allCoef2)])

    parameters <- c(list(muX, muY, alphaX, alphaY, rhoNor, rhoAbn2),
                    as.list(zetaX), as.list(zetaY))
    namesVector <- c("muX", "muY", "alphaX", "alphaY","rhoNor", "rhoAbn2")
    namesVector <- c(namesVector, paste0("zetaX", 1:length(zetaX)))
    namesVector <- c(namesVector, paste0("zetaY", 1:length(zetaY)))
    names(parameters) <- namesVector

    ret3 <- mle2(CorCBMLLNoTrnsAdd, start = parameters, method = "BFGS",
                 data = list(FPCounts = FPCounts, TPCounts = TPCounts))
  }
  covMat <- ret3@vcov

  fittedPlot <- PlotCorCbmFit(
    list(
      FPCounts = FPCounts,
      TPCounts = TPCounts,
      muX = muX,
      alphaX = alphaX,
      muY = muY,
      alphaY = alphaY,
      rhoNor = rhoNor,
      rhoAbn2 = rhoAbn2,
      zetaX = zetaX,
      zetaY = zetaY
    )
  )

  fitCorCbmRet <- list(
    FPCounts = FPCounts,
    TPCounts = TPCounts,
    muX = muX,
    alphaX = alphaX,
    muY = muY,
    alphaY = alphaY,
    rhoNor = rhoNor,
    rhoAbn2 = rhoAbn2,
    zetaX = zetaX,
    zetaY = zetaY,
    covMat = covMat,
    fixParam = fixParam
    )

  stats <- StatsCorCbm(fitCorCbmRet)

  return(list(
    fitCorCbmRet = fitCorCbmRet,
    stats = stats,
    fittedPlot = fittedPlot
  ))
}



StatsCorCbm <- function(fitCorCbmRet) {
  muX <- fitCorCbmRet$muX
  muY <- fitCorCbmRet$muY
  alphaX <- fitCorCbmRet$alphaX
  alphaY <- fitCorCbmRet$alphaY
  rhoNor <- fitCorCbmRet$rhoNor
  rhoAbn2 <- fitCorCbmRet$rhoAbn2
  zetaX <- fitCorCbmRet$zetaX
  zetaY <- fitCorCbmRet$zetaY
  FPCounts <- fitCorCbmRet$FPCounts
  TPCounts <- fitCorCbmRet$TPCounts
  covMat <- fitCorCbmRet$covMat
  fixParam <- fitCorCbmRet$fixParam

  vars <- diag(covMat)
  stdErr <- sqrt(vars)
  dMuX <- alphaX * dnorm(muX / sqrt(2)) / sqrt(2)
  dAlphaX <- -1 / 2 + pnorm(muX / sqrt(2))
  stdAucX <-
    sqrt(vars[1] * dMuX ^ 2 + vars[3] * dAlphaX ^ 2 + dMuX * dAlphaX * covMat[1, 3])

  dMuY <- alphaY * dnorm(muY / sqrt(2)) / sqrt(2)
  dAlphaY <- -1 / 2 + pnorm(muY / sqrt(2))
  stdAucY <-
    sqrt(vars[2] * dMuY ^ 2 + vars[4] * dAlphaY ^ 2 + dMuY * dAlphaY * covMat[2, 4])

  stdErr <- c(stdErr, stdAucX, stdAucY)

  if (length(fixParam) > 0) {
    for (p in fixParam) {
      stdErr <- append(stdErr, NA, after = p - 1)
      if (p == 3) {
        alphaX <- 1
      } else if (p == 4) {
        alphaY <- 1
      } else if (p == 5) {
        rhoNor <- 1
      } else if (p == 6) {
        rhoAbn2 <- 1
      }
    }
    stdErr <- as.numeric(stdErr)
  }
  stdErr[2:3] <- stdErr[3:2] # switch the position of alphaX and muY

  aucX <- (1 - alphaX) * 0.5 + alphaX * pnorm(muX / sqrt(2))
  aucY <- (1 - alphaY) * 0.5 + alphaY * pnorm(muY / sqrt(2))

  aucDiff <- aucX - aucY

  varDiff <- 0
  derivs <- c(dMuX,-dMuY, dAlphaX,-dAlphaY)
  for (k in 1:4) {
    for (l in 1:4) {
      varDiff <- derivs[l] * derivs[k] * covMat[k, l] + varDiff
    }
  }
  stdDiff <- sqrt(varDiff)
  areaStat <- abs(aucDiff) / stdDiff
  areaPval <- 1 - pnorm(areaStat)

  return(
    list(
      aucX = aucX,
      aucY = aucY,
      stdAucX = as.numeric(stdAucX),
      stdAucY = as.numeric(stdAucY),
      stdErr = stdErr,
      areaStat = areaStat,
      areaPval = areaPval
    )
  )
}





###############################################################################
nLLCorCBM <- function(muXFwd, muYFwd, alphaXFwd, alphaYFwd, rhoNorFwd, rhoAbn2Fwd, FPCounts, TPCounts, maxMu){
  minMu <- RJafrocEnv$minMu
  #maxMu <- RJafrocEnv$maxMu
  minAlpha <- RJafrocEnv$minAlpha
  maxAlpha <- RJafrocEnv$maxAlpha
  minRho <- RJafrocEnv$minRho
  maxRho <- RJafrocEnv$maxRho

  allParameters <- names(formals())
  zetaXPos <- regexpr("zetaX", allParameters)
  zetaXFwd <- unlist(mget(allParameters[which(zetaXPos == 1)]))
  zetaYPos <- regexpr("zetaY", allParameters)
  zetaYFwd <- unlist(mget(allParameters[which(zetaYPos == 1)]))

  muX <- InverseValue(muXFwd, minMu, maxMu)
  muY <- InverseValue(muYFwd, minMu, maxMu)
  alphaX <- InverseValue(alphaXFwd, minAlpha, maxAlpha)
  alphaY <- InverseValue(alphaYFwd, minAlpha, maxAlpha)

  rhoNor <- InverseValue(rhoNorFwd, minRho, maxRho)
  rhoAbn <- InverseValue(rhoAbn2Fwd, minRho, maxRho)

  zetaX <- InverseZetas(zetaXFwd)
  zetaY <- InverseZetas(zetaYFwd)

  LL <- LLCorCBM (muX, muY, alphaX, alphaY, rhoNor, rhoAbn, zetaX, zetaY, FPCounts, TPCounts)

  return(-LL)
}


###############################################################################
nLLCorCBMNoTrns <- function(muX, muY, alphaX, alphaY, rhoNor, rhoAbn2, FPCounts, TPCounts){
  allParameters <- names(formals())
  zetaXPos <- regexpr("zetaX", allParameters)
  zetaX <- unlist(mget(allParameters[which(zetaXPos == 1)]))
  zetaYPos <- regexpr("zetaY", allParameters)
  zetaY <- unlist(mget(allParameters[which(zetaYPos == 1)]))

  LL <- LLCorCBM (muX, muY, alphaX, alphaY, rhoNor, rhoAbn2, zetaX, zetaY, FPCounts, TPCounts)

  return(-LL)
}






###############################################################################
LLCorCBM <- function(muX, muY, alphaX, alphaY, rhoNor, rhoAbn, zetaX, zetaY, FPCounts, TPCounts) {
  if ((abs(zetaX[1]) > 2) ||
      (abs(zetaY[1]) > 2) ||
      (zetaX[length(zetaX)] > 10) ||
      (zetaY[length(zetaY)] > 10))  {
    return(-1e10)
  }
  zetaX <- c(-Inf, zetaX, Inf)
  zetaY <- c(-Inf, zetaY, Inf)
  if (is.unsorted(zetaX)) return(-1e10)
  if (is.unsorted(zetaY)) return(-1e10)
  rhoAbn1 <- rhoNor
  rhoAbn2 <- rhoAbn
  rhoAbn12 <- mean(c(rhoAbn1, rhoAbn2))
  rhoAbn4 <- rhoAbn12

  sigmaNor <- rbind(c(1, rhoNor), c(rhoNor, 1))
  sigmaAbn1 <- rbind(c(1, rhoAbn1), c(rhoAbn1, 1))
  sigmaAbn2 <- rbind(c(1, rhoAbn2), c(rhoAbn2, 1))
  sigmaAbn3 <- rbind(c(1, rhoAbn12), c(rhoAbn12, 1))
  sigmaAbn4 <- rbind(c(1, rhoAbn4), c(rhoAbn4, 1))

  LLNor <- 0
  LLAbn <- 0
  nBinsX <- dim(FPCounts)[1]
  nBinsY <- dim(FPCounts)[2]
  for (bX in 1:nBinsX){
    for (bY in 1:nBinsY){
      if (FPCounts[bX, bY] > 0) {
        tempNor <- pmvnorm(c(zetaX[bX], zetaY[bY]), c(zetaX[bX + 1], zetaY[bY + 1]), sigma = sigmaNor)
        if (tempNor >= 0) {
          LLNor <- LLNor +
            FPCounts[bX, bY] * log(tempNor)
        } else {
          return(-1e10)
        }
      }
      if (TPCounts[bX, bY] > 0) {
        tempAbn <- (1 - alphaX) * (1 - alphaY) *
          pmvnorm(c(zetaX[bX], zetaY[bY]), c(zetaX[bX + 1], zetaY[bY + 1]), mean = c(0, 0), sigma = sigmaAbn1) +
          (alphaX) * (1 - alphaY) * pmvnorm(c(zetaX[bX], zetaY[bY]), c(zetaX[bX + 1], zetaY[bY + 1]),
                                            mean = c(muX, 0), sigma = sigmaAbn3) +
          (1 - alphaX) * (alphaY) * pmvnorm(c(zetaX[bX], zetaY[bY]), c(zetaX[bX + 1], zetaY[bY + 1]),
                                            mean = c(0, muY), sigma = sigmaAbn4) +
          alphaX * alphaY * pmvnorm(c(zetaX[bX], zetaY[bY]), c(zetaX[bX + 1], zetaY[bY + 1]),
                                    mean = c(muX, muY), sigma = sigmaAbn2)
        if (tempAbn >= 0) {
          LLAbn <- LLAbn +
            TPCounts[bX, bY] * log(tempAbn)
        } else {
          return(-1e10)
        }
      }
    }
  }
  LL <- LLNor + LLAbn
  return(LL)
}

#' @import ggplot2
PlotCorCbmFit <- function(retFitCorCBM){
  
  muX <- retFitCorCBM$muX
  muY <- retFitCorCBM$muY
  alphaX <- retFitCorCBM$alphaX
  alphaY <- retFitCorCBM$alphaY
  FPCounts <- retFitCorCBM$FPCounts
  TPCounts <- retFitCorCBM$TPCounts
  K1 <- sum(FPCounts);K2 <- sum(TPCounts)
  
  plotZeta <- seq(-3, max(muX,muY)+2, by = 0.1)
  plotCBM <- NULL
  plotOpPnts <- NULL
  FPFX <- 1 - pnorm(plotZeta)
  TPFX <- (1 - alphaX) * (1 - pnorm(plotZeta)) + alphaX * (1 - pnorm(plotZeta, mean = muX))

  plotCBM <- rbind(plotCBM, data.frame(FPF = FPFX, TPF = TPFX, Condition = "X"))
  FPFX <- cumsum(rev(rowSums(FPCounts)))/K1
  TPFX <- cumsum(rev(rowSums(TPCounts)))/K2
  FPFX <- FPFX[-length(FPFX)]
  TPFX <- TPFX[-length(TPFX)]
  plotOpPnts <- rbind(plotOpPnts, data.frame(FPF = FPFX, TPF = TPFX, Condition = "X"))
  
  FPFY <- 1 - pnorm(plotZeta)
  TPFY <- (1 - alphaY) * (1 - pnorm(plotZeta)) + alphaY * (1 - pnorm(plotZeta, mean = muY))
  plotCBM <- rbind(plotCBM, data.frame(FPF = FPFY, TPF = TPFY, Condition = "Y"))
  FPFY <- cumsum(rev(colSums(FPCounts)))/K1
  TPFY <- cumsum(rev(colSums(TPCounts)))/K2
  FPFY <- FPFY[-length(FPFY)]
  TPFY <- TPFY[-length(TPFY)]
  plotOpPnts <- rbind(plotOpPnts, data.frame(FPF = FPFY, TPF = TPFY, Condition = "Y"))
  
  Condition <- NULL;FPF <- NULL;TPF <- NULL
  fittedPlot <- ggplot(data = plotCBM, mapping = aes(x = FPF, y = TPF, color = Condition)) +
    geom_line(data = plotCBM, size = 1) +
    geom_point(data = plotOpPnts, size = 4) +
    theme(legend.position = c(1, 0), legend.direction = "horizontal")
  
  fittedPlot <- fittedPlot +
    geom_line(data = plotCBM, mapping = aes(linetype = Condition), size = 1) +
    geom_point(data = plotOpPnts, mapping = aes(shape = Condition), size = 3) +
    theme(legend.title=element_blank(),
          legend.position = c(1, 0),
          legend.direction = "horizontal",
          legend.justification = c(1, 0),
          legend.key.size = unit(1, "cm")) #+
  # scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
  
  ciIndxX <- c(1, length(FPFX))
  FPF <- FPFX[ciIndxX]
  TPF <- TPFX[ciIndxX]
  ciX <- binom.confint(x = FPF * K1, n = K1, methods = "exact")
  ciY <- binom.confint(x = TPF * K2, n = K2, methods = "exact")
  ciXUpper <- ciX$upper
  ciXLower <- ciX$lower
  ciYUpper <- ciY$upper
  ciYLower <- ciY$lower
  for (p in 1:length(FPF)){
    ciX <- data.frame(FPF = c(ciXUpper[p], ciXLower[p]), TPF = c(TPF[p], TPF[p]))
    ciY <- data.frame(FPF = c(FPF[p], FPF[p]), TPF = c(ciYUpper[p], ciYLower[p]))
    fittedPlot <- fittedPlot + geom_line(data = ciY, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = ciX, aes(x = FPF, y = TPF), color = "black")
    barRgt <- data.frame(FPF = c(ciXUpper[p], ciXUpper[p]), TPF = c(TPF[p] - 0.01, TPF[p] + 0.01))
    barLft <- data.frame(FPF = c(ciXLower[p], ciXLower[p]), TPF = c(TPF[p] - 0.01, TPF[p] + 0.01))
    barUp <- data.frame(FPF = c(FPF[p] - 0.01, FPF[p] + 0.01), TPF = c(ciYUpper[p], ciYUpper[p]))
    barBtm <- data.frame(FPF = c(FPF[p] - 0.01, FPF[p] + 0.01), TPF = c(ciYLower[p], ciYLower[p]))
    fittedPlot <- fittedPlot + geom_line(data = barRgt, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = barLft, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = barUp, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = barBtm, aes(x = FPF, y = TPF), color = "black")
  }
  
  ciIndxY <- c(1, length(FPFY))
  FPF <- FPFY[ciIndxY]
  TPF <- TPFY[ciIndxY]
  ciX <- binom.confint(x = FPF * K1, n = K1, methods = "exact")
  ciY <- binom.confint(x = TPF * K2, n = K2, methods = "exact")
  ciXUpper <- ciX$upper
  ciXLower <- ciX$lower
  ciYUpper <- ciY$upper
  ciYLower <- ciY$lower
  for (p in 1:length(FPF)){
    ciX <- data.frame(FPF = c(ciXUpper[p], ciXLower[p]), TPF = c(TPF[p], TPF[p]))
    ciY <- data.frame(FPF = c(FPF[p], FPF[p]), TPF = c(ciYUpper[p], ciYLower[p]))
    fittedPlot <- fittedPlot + geom_line(data = ciY, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = ciX, aes(x = FPF, y = TPF), color = "black")
    barRgt <- data.frame(FPF = c(ciXUpper[p], ciXUpper[p]), TPF = c(TPF[p] - 0.01, TPF[p] + 0.01))
    barLft <- data.frame(FPF = c(ciXLower[p], ciXLower[p]), TPF = c(TPF[p] - 0.01, TPF[p] + 0.01))
    barUp <- data.frame(FPF = c(FPF[p] - 0.01, FPF[p] + 0.01), TPF = c(ciYUpper[p], ciYUpper[p]))
    barBtm <- data.frame(FPF = c(FPF[p] - 0.01, FPF[p] + 0.01), TPF = c(ciYLower[p], ciYLower[p]))
    fittedPlot <- fittedPlot + geom_line(data = barRgt, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = barLft, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = barUp, aes(x = FPF, y = TPF), color = "black") +
      geom_line(data = barBtm, aes(x = FPF, y = TPF), color = "black")
  }
  return(fittedPlot)
}


# fitted <- function (muX, alphaX, muY, alphaY, FPCounts, TPCounts){
#   
#   K1 <- sum(FPCounts);K2 <- sum(TPCounts)
#   plotZeta <- seq(-3, max(muX,muY)+2, by = 0.1)
#   plotCBM <- NULL
#   plotOpPnts <- NULL
#   FPFX <- 1 - pnorm(plotZeta)
#   TPFX <- (1 - alphaX) * (1 - pnorm(plotZeta)) + alphaX * (1 - pnorm(plotZeta, mean = muX))
#   plotCBM <- rbind(plotCBM, data.frame(FPF = FPFX, TPF = TPFX, Condition = "X"))
#   FPFX <- cumsum(rev(rowSums(FPCounts)))/K1
#   TPFX <- cumsum(rev(rowSums(TPCounts)))/K2
#   FPFX <- FPFX[-length(FPFX)]
#   TPFX <- TPFX[-length(TPFX)]
#   plotOpPnts <- rbind(plotOpPnts, data.frame(FPF = FPFX, TPF = TPFX, Condition = "X"))
#   
#   FPFY <- 1 - pnorm(plotZeta)
#   TPFY <- (1 - alphaY) * (1 - pnorm(plotZeta)) + alphaY * (1 - pnorm(plotZeta, mean = muY))
#   plotCBM <- rbind(plotCBM, data.frame(FPF = FPFY, TPF = TPFY, Condition = "Y"))
#   FPFY <- cumsum(rev(colSums(FPCounts)))/K1
#   TPFY <- cumsum(rev(colSums(TPCounts)))/K2
#   FPFY <- FPFY[-length(FPFY)]
#   TPFY <- TPFY[-length(TPFY)]
#   plotOpPnts <- rbind(plotOpPnts, data.frame(FPF = FPFY, TPF = TPFY, Condition = "Y"))
#   
#   fittedPlot <- ggplot(mapping = aes(x = FPF, y = TPF, color = Condition)) +
#     geom_line(data = plotCBM, size = 1) +
#     geom_point(data = plotOpPnts, size = 4) +
#     theme(legend.position = c(1, 0), legend.direction = "horizontal")
#   
#   return(list(
#     fittedPlot = fittedPlot,
#     plotCBM = plotCBM,
#     plotOpPnts = plotOpPnts,
#     FPFX = FPFX,
#     TPFX = TPFX,
#     FPFY = FPFY,
#     TPFY = TPFY
#   ))
#   
# }
# 
