#' Returns a binned dataset
#'
#' @description Bins continuous (i.e. floating point) or quasi-continuous (e.g. integers 0-100)
#' ratings in a dataset and returns the corresponding binned dataset in which the ratings are integers
#' 1, 2,...., with higher values representing greater confidence in presence of disease
#'
#'
#' @param dataset The dataset to be binned, with structure as in \code{\link{RJafroc-package}}.
#' @param desiredNumBins The desired number of bins. The default is 7.
#' @param opChType The operating characteristic relevant to the binning operation: 
#' \code{"ROC"}, \code{"FROC"}, \code{"AFROC"}, or \code{"wAFROC"}. 
#'
#'
#' @return The binned dataset
#'
#' @details
#' For small datasets the number of bins may be smaller than \code{desiredNumBins}.
#' \strong{The algorithm needs to know the type of operating characteristic 
#' relevant to the binning operation.} For ROC the bins are FP and TP counts, for 
#' FROC the bins are NL and LL counts, for AFROC the bins are FP and LL counts, 
#' and for wAFROC the bins are FP and wLL counts. Binning is generally
#' employed prior to fitting a statistical model, e.g., maximum likelihood, to the data.
#' This version chooses ctffs so as to maximize empirical AUC (this yields a 
#' unique choice of ctffs which gives the reader the maximum deserved credit).
#'
#'
#' @import ggplot2
#' @import utils
#'
#' @examples
#' \donttest{
#' binned <- DfBinDataset(dataset05, opChType = "ROC")
#' PlotEmpiricalOperatingCharacteristics(dataset05, 
#' trts= c(1,2), rdrs = seq(1,9), opChType = "ROC")$Plot
#' PlotEmpiricalOperatingCharacteristics(binned, trts= c(1,2), 
#' rdrs = seq(1,9), opChType = "ROC")$Plot
#'
#' binned <- DfBinDataset(dataset05, opChType = "AFROC")
#' PlotEmpiricalOperatingCharacteristics(dataset05, 
#' trts= c(1,2), rdrs = seq(1,9), opChType = "AFROC")$Plot
#' PlotEmpiricalOperatingCharacteristics(binned, trts= c(1,2), 
#' rdrs = seq(1,9), opChType = "AFROC")$Plot
#' }
#'  
#' \donttest{
#' ## takes longer than 5 sec on OSX
#' ## library(ggplot2)
#' ## dataset <- SimulateRocDataset(K1 = 5000, K2 = 7000, a = 1, b = 0.5, seed = 123)
#' ## datasetB <- DfBinDataset(dataset, desiredNumBins = 7, opChType = "ROC")
#' ## fomOrg <- as.matrix(UtilFigureOfMerit(dataset, FOM = "Wilcoxon"), nrow = 2, ncol = 9)
#' ## print(fomOrg)
#' ## fomBinned <- as.matrix(UtilFigureOfMerit(datasetB, FOM = "Wilcoxon"), nrow = 2, ncol = 9)
#' ## print(fomOrg)
#' ## cat("fomOrg = ", mean(fomOrg), "\n")
#' ## cat("fomBinned = ", mean(fomBinned), "\n")
#' ## x <- PlotEmpiricalOperatingCharacteristics(dataset, opChType = "ROC")$Plot
#' ## y <- PlotEmpiricalOperatingCharacteristics(datasetB, opChType = "ROC")$Points
#' ## fpf <- y$genAbscissa[-1];fpf <- fpf[-length(fpf)]
#' ## tpf <- y$genOrdinate[-1];tpf <- tpf[-length(tpf)]
#' ## plotOpPnts <- rbind(data.frame(fpf = fpf, tpf = tpf))
#' ## x <- x + geom_point(data = plotOpPnts, aes(x = fpf, y = tpf), size = 4)
#' ## print(x)
#' ## xx <- PlotEmpiricalOperatingCharacteristics(datasetB, opChType = "ROC")
#' ## print(xx$Points)
#' }
#' 
#' 
#' @references
#' Miller GA (1956) The Magical Number Seven, Plus or Minus Two:
#' Some limits on our capacity for processing information, The Psychological Review 63, 81-97
#'
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#'
#' @export

DfBinDataset <- function(dataset, desiredNumBins = 7, opChType) {
  
  nlDim <- dim(dataset$ratings$NL)
  llDim <- dim(dataset$ratings$LL)
  I <- nlDim[1]
  J <- nlDim[2]
  K <- nlDim[3]
  K2 <- llDim[3]
  K1 <- K - K2
  type <- dataset$descriptions$type
  
  if (type == "ROC") {
    if (opChType == "FROC") stop("Cannot convert an ROC dataset to an AFROC dataset")
  } else if (type == "FROC") {
    if (opChType == "ROC") dataset <- DfFroc2Roc(dataset)
    if (opChType == "AFROC") dataset <- DfFroc2Afroc(dataset)
    if (opChType == "wAFROC") dataset <- DfFroc2Afroc(dataset)
    type <- dataset$descriptions$type
  } else {
    stop("type must be ROC or FROC")
  }
  if (opChType == "ROC") FOM <- "Wilcoxon" else
    if (opChType == "AFROC") FOM <- "AFROC" else
      if (opChType == "wAFROC") FOM <- "wAFROC" else 
        if (opChType == "FROC") {
          FOM <- "FROC"
          desiredNumBins <- desiredNumBins + 1
        } else stop("should not be here")
  
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL

  # fomOrg <- as.matrix(UtilFigureOfMerit(dataset, FOM = FOM), nrow = I, ncol = J)
  # ## print(fomOrg)
  # cat("mean, sd = ", mean(fomOrg), sd(fomOrg), "\n")
  numZeta <- desiredNumBins - 1 # TBA may need to distinguish here btwn ROC and FROC
  maxFomij <- array(-1,dim = c(I,J))
  sSave <- array(dim = c(I,J))
  zetasArr <- array(dim = c(I,J,numZeta))
  
  # NL <- dataset$ratings$NL[i,j,,]
  # LL <- dataset$ratings$LL[i,j,1:K2,]
  # searchZetas2MaximizeFom (dataset, FOM, maximize) {
  # find zetas to maximize FOM
  datasetB <- dataset
  for (i in 1:I){
    for (j in 1:J){
      if (FOM == "FROC") {      
        NL <- dataset$ratings$NL[i,j,,]
        LL <- dataset$ratings$LL[i,j,1:K2,]
      } else {
        NL <- dataset$ratings$NL[i,j,1:K1,]
        LL <- dataset$ratings$LL[i,j,1:K2,]
      }
      nl <- NL[NL != -Inf];lx <- length(nl)
      ll <- LL[LL != -Inf];ly <- length(ll)
      nl_ll <- c(nl, ll)
      
      candidateZetas <-  sort(unique(nl_ll))
      # for ROC, need to remove lowest value, as this gives (1,1) point
      if (type == "ROC") candidateZetas <-  candidateZetas[-1]
      
      el <- length(candidateZetas)
      if (el < numZeta) {
        sample <- combn(candidateZetas, el -1)
      } else {
        # if more than 20 candidates, need to trim
        if (el > 20) {
          byDivisor <- 10
          while (1) {
            by <- as.integer(el/byDivisor)
            candidateZetasTrim <- candidateZetas[seq(from = 1, to = el, by = by)]
            sample <- combn(candidateZetasTrim, numZeta)
            if (length(sample[1,]) > 200) {
              byDivisor <- byDivisor - 1
            } else break
          }
        } else sample <- combn(candidateZetas, numZeta)
      }
      for (s in 1:length(sample[1,])) {
        z <- sort(sample[,s])
        if (type == "ROC")  zetas <- c(-Inf,z,+Inf) else zetas <- c(z,+Inf) 
        nLlLB <- cut(nl_ll, zetas, labels = FALSE, right = FALSE)
        nLlLB[is.na(nLlLB)] <- -Inf
        nlB <- array(-Inf, dim = c(K1+K2,nlDim[4]))
        llB <- array(-Inf, dim = c(K2,llDim[4]))
        if (FOM == "FROC") {      
          nlB[x] <- nLlLB[1:lx]
          llB[1:K2,][y] <- nLlLB[(lx+1):(lx+ly)]
        } else {
          nlB[1:K1,][x] <- nLlLB[1:lx]
          llB[1:K2,][y] <- nLlLB[(lx+1):(lx+ly)]
        }
        datasetB$ratings$NL[i,j,1:K1,] <- nlB[1:K1,]
        datasetB$ratings$LL[i,j,,] <- llB
        fom1 <- as.matrix(UtilFigureOfMerit(datasetB, FOM = FOM))[i,j]
        if (fom1 > maxFomij[i,j]){
          sSave[i,j] <- s
          maxFomij[i,j] <- fom1
          zetasArr[i,j,1:length(z)] <- z
        }
      }
      next
    }
  }
  
  # this time we simply bin the data, no more maximization
  datasetB <- dataset
  for (i in 1:I) {
    for (j in 1:J) { 
      if (FOM == "FROC") {      
        NL <- dataset$ratings$NL[i,j,,]
        LL <- dataset$ratings$LL[i,j,1:K2,]
      } else {
        NL <- dataset$ratings$NL[i,j,1:K1,]
        LL <- dataset$ratings$LL[i,j,1:K2,]
      }
      x <- (NL != -Inf);lx <- length(NL[x])
      y <- (LL != -Inf);ly <- length(LL[y])
      nl_ll <- c(NL[x],LL[y])
      z <- zetasArr[i,j,]
      z <- z[!is.na(z)]
      if (type == "ROC")  zetas <- c(-Inf,z,+Inf) else zetas <- c(z,+Inf) 
      nLlLB <- cut(nl_ll, zetas, labels = FALSE, right = FALSE)
      nLlLB[is.na(nLlLB)] <- -Inf
      # nlB <- array(-Inf, dim = c(K1+K2,nlDim[4]))
      # llB <- array(-Inf, dim = c(K2,llDim[4]))
      # nlB[1:K1,][x] <- nLlLB[1:lx]
      # llB[1:K2,][y] <- nLlLB[(lx+1):(lx+ly)]
      # datasetB$NL[i,j,1:K1,] <- nlB[1:K1,]
      # datasetB$LL[i,j,,] <- llB
      if (FOM == "FROC") {      
        datasetB$ratings$NL[i,j,,][x] <- nLlLB[1:lx]
        datasetB$ratings$LL[i,j,1:K2,][y] <- nLlLB[(lx+1):(lx+ly)]
      } else {
        datasetB$ratings$NL[i,j,1:K1,][x] <- nLlLB[1:lx]
        datasetB$ratings$LL[i,j,1:K2,][y] <- nLlLB[(lx+1):(lx+ly)]
      }
    }
  }
  # fom1 <- UtilFigureOfMerit(datasetB, FOM = FOM)
  # ## print(fom1)
  # cat("mean, sd = ", mean(fom1), sd(fom1), "\n")
  NL <- datasetB$ratings$NL
  LL <- datasetB$ratings$LL
  binned <- isBinned(NL, LL)
  fileName <- NA
  name <- NA
  design <- "FCTRL"
  truthTableStr <- NA
  type <- "ROC"
  perCase <- rep(1,K2)
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, dataset$lesions$IDs, dataset$lesions$weights,
                         binned, fileName, type, name, truthTableStr, design,
                         dataset$descriptions$modalityID, dataset$descriptions$readerID))
  return(datasetB)
}



searchZetas2MaximizeFom <- function(dataset, FOM, maximize) {
  
  nlDim <- dim(dataset$ratings$NL)
  llDim <- dim(dataset$ratings$LL)
  I <- nlDim[1]
  J <- nlDim[2]
  K <- nlDim[3]
  K2 <- llDim[3]
  K1 <- K - K2
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  type <- dataset$descriptions$type
  
  datasetB <- dataset # datasetB is the binned dataset
  for (i in 1:I){
    for (j in 1:J){
      if (FOM == "FROC") {      
        NL <- NL[i,j,,]
        LL <- LL[i,j,1:K2,]
      } else {
        NL <- NL[i,j,1:K1,]
        LL <- LL[i,j,1:K2,]
      }
      x <- (NL != -Inf);lx <- length(NL[x])
      y <- (LL != -Inf);ly <- length(LL[y])
      nLlL <- c(NL[x],LL[y])
      # for ROC, need to remove lowest value, as this gives (1,1) point
      if (type == "ROC") candidateZetas <-  sort(unique(nLlL))[-1] else
        candidateZetas <-  sort(unique(nLlL))
      el <- length(candidateZetas)
      if (el < numZeta) {
        sample <- combn(candidateZetas, el -1)
      } else {
        # if more than 20 candidates, need to trim
        if (el > 20) {
          byDivisor <- 10
          while (1) {
            by <- as.integer(el/byDivisor)
            candidateZetasTrim <- candidateZetas[seq(from = 1, to = el, by = by)]
            sample <- combn(candidateZetasTrim, numZeta)
            if (length(sample[1,]) > 200) {
              byDivisor <- byDivisor - 1
            } else break
          }
        } else sample <- combn(candidateZetas, numZeta)
      }
      for (s in 1:length(sample[1,])) {
        z <- sort(sample[,s])
        if (type == "ROC")  zetas <- c(-Inf,z,+Inf) else zetas <- c(z,+Inf) 
        nLlLB <- cut(nLlL, zetas, labels = FALSE, right = FALSE)
        nLlLB[is.na(nLlLB)] <- -Inf
        nlB <- array(-Inf, dim = c(K1+K2,nlDim[4]))
        llB <- array(-Inf, dim = c(K2,llDim[4]))
        if (FOM == "FROC") {      
          nlB[x] <- nLlLB[1:lx]
          llB[1:K2,][y] <- nLlLB[(lx+1):(lx+ly)]
        } else {
          nlB[1:K1,][x] <- nLlLB[1:lx]
          llB[1:K2,][y] <- nLlLB[(lx+1):(lx+ly)]
        }
        if (maximize) {
          datasetB$ratings$NL[i,j,1:K1,] <- nlB[1:K1,]
          datasetB$ratings$LL[i,j,,] <- llB
          fom1 <- as.matrix(UtilFigureOfMerit(datasetB, FOM = FOM))[i,j]
          if (fom1 > maxFomij[i,j]){
            sSave[i,j] <- s
            maxFomij[i,j] <- fom1
            zetasArr[i,j,1:length(z)] <- z
          }
        }
      }
      next
    }
  }
  
  return(list(
    zetasArr = zetasArr,
    datasetB = datasetB
  ))
}



DfFroc2Afroc <- function (dataset){
  if (dataset$descriptions$type != "FROC") stop("The dataset has to be FROC")
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2 
  
  NL <- apply(NL, c(1, 2, 3), max)
  dim(NL) <- c(dim(NL), 1)
  NL[,,(K1+1):K,1] <- -Inf
  
  binned <- isBinned(NL, LL)
  fileName <- NA
  name <- NA
  design <- dataset$descriptions$design
  truthTableStr <- dataset$descriptions$truthTableStr
  IDs <- dataset$lesions$IDs
  type <- "FROC"
  perCase <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         binned, fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
  
}



isDataDegenerate <-  function (fpf, tpf) {
  
  ret <- rep(FALSE, length(fpf))
  for (i in 1:length(fpf)){
    if ((fpf[i] == 0) || (tpf[i] == 0) || (fpf[i] == 1) || (tpf[i] == 1)) ret[i] <- TRUE
  }
  if (all(ret)) return (TRUE) else return (FALSE)
}




UtilBinCountsOpPts <- function(dataset, trt = 1, rdr = 1)
{
  I <- nlDim[1]
  J <- nlDim[2]
  K <- nlDim[3]
  K2 <- llDim[3]
  K1 <- K - K2
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  
  stop("need fix here")
  # TBA SimplifyDatasets
  
  fp <- NL[trt,rdr,1:K1,,drop = TRUE] 
  tp <- LL[trt,rdr,,,drop = TRUE]
  
  bins <- sort(unique(c(fp,tp)))
  nBins <- length(bins)
  
  fpCounts <- array(0, dim = nBins)
  tpCounts <- array(0, dim = nBins)
  
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == bins[b])
    tpCounts[b] <- sum(tp == bins[b])
  }
  
  fpf <- cumsum(rev(fpCounts)) / K1
  tpf <- cumsum(rev(tpCounts)) / K2
  fpf <- fpf[-length(fpf)]
  tpf <- tpf[-length(tpf)]
  
  return(list(
    fpCounts = fpCounts,
    tpCounts = tpCounts,
    fpf = fpf,
    tpf = tpf
  ))
}





