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
#' binned <- DfBinDataset(dataset02, desiredNumBins = 3, opChType = "ROC")
#'
#' binned <- DfBinDataset(dataset02, desiredNumBins = 3, opChType = "ROC")
#' binned <- DfBinDataset(dataset02, desiredNumBins = 3, opChType = "AFROC")
#' binned <- DfBinDataset(dataset05, opChType = "wAFROC", desiredNumBins = 1)
#' binned <- DfBinDataset(dataset05, opChType = "wAFROC", desiredNumBins = 2)
#' binned <- DfBinDataset(dataset05, opChType = "wAFROC", desiredNumBins = 3)
#' ## etc.
#' }
#'  
#' \donttest{
#' ## takes longer than 5 sec on OSX
#' dataset <- SimulateRocDataset(I = 2, J = 5, K1 = 50, K2 = 70, a = 1, b = 0.5, seed = 123)
#' datasetB <- DfBinDataset(dataset, desiredNumBins = 7, opChType = "ROC")
#' fomOrg <- as.matrix(UtilFigureOfMerit(dataset, FOM = "Wilcoxon"))
#' print(fomOrg)
#' fomBinned <- as.matrix(UtilFigureOfMerit(datasetB, FOM = "Wilcoxon"))
#' print(fomBinned)
#' cat("mean, sd = ", mean(fomOrg), sd(fomOrg), "\n")
#' cat("mean, sd = ", mean(fomBinned), sd(fomBinned), "\n")
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
  
  if (dataset$descriptions$design != "FCTRL") stop("Need factorial dataset for this function")
  
  type <- dataset$descriptions$type
  
  DEBUG <- FALSE
  
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
  
  if (opChType == "ROC") FOM <- "Wilcoxon" else if (opChType == "AFROC") 
    FOM <- "AFROC" else if (opChType == "wAFROC") FOM <- "wAFROC" 
  else if (opChType == "FROC") {
    FOM <- "FROC"
    desiredNumBins <- desiredNumBins + 1 # TBA why?
  } else stop("should not be here")
  
  if (DEBUG) {
    fomOrg <- as.matrix(UtilFigureOfMerit(dataset, FOM = FOM))
    print(fomOrg)
    cat("mean, sd = ", mean(fomOrg), sd(fomOrg), "\n")
  }
  
  if (type == "ROC") {
    desiredNumZetas <- desiredNumBins - 1
  } 
  else {
    desiredNumZetas <- desiredNumBins
  }
  
  nlDim <- dim(dataset$ratings$NL)
  llDim <- dim(dataset$ratings$LL)
  I <- nlDim[1]
  J <- nlDim[2]
  K <- nlDim[3]
  K2 <- llDim[3]
  K1 <- K - K2
  
  # find zetas to maximize FOM
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  NL_B <- NL;LL_B <- LL
  zetas_ij <- array(dim = c(I,J,desiredNumZetas))
  for (i in 1:I){
    for (j in 1:J){
      maxFom <- -1
      if (FOM == "FROC") {      
        NL_ij <- NL[i,j,,]
        LL_ij <- LL[i,j,1:K2,]
      } else {
        NL_ij <- NL[i,j,1:K1,]
        LL_ij <- LL[i,j,1:K2,]
      }
      x <- (NL_ij != -Inf)
      nl <- NL_ij[x]
      lx <- length(nl)
      y <- (LL_ij != -Inf)
      ll <- LL_ij[y]
      ly <- length(ll)
      nl_ll <- c(nl, ll)
      
      candidateZetas <-  sort(unique(nl_ll))
      # for ROC, could remove lowest value, as this gives the trivial (1,1) point
      # this step is optional, does not make a difference in the end
      if (type == "ROC") {
        candidateZetas <-  candidateZetas[-1]
      }
      
      if (length(candidateZetas) <= desiredNumZetas) { # rhs is desiredNumBins - 1 for ROC desiredNumBins for FROC
        # this dataset is already binned acccrding to desiredNumZetas criteria
        # no point searching, so save the candidateZetas and move on to next i,j
        zetas_ij[i,j,1:length(candidateZetas)] <- candidateZetas
        next
      }
      
      el <- length(candidateZetas)
      if (el < desiredNumZetas) {
        sample <- combn(candidateZetas, el -1)
      } else {
        # if more than 20 candidates, need to trim
        if (el > 20) {
          byDivisor <- 10
          while (1) {
            by <- as.integer(el/byDivisor)
            candidateZetasTrim <- candidateZetas[seq(from = 1, to = el, by = by)]
            sample <- combn(candidateZetasTrim, desiredNumZetas)
            if (length(sample[1,]) > 200) {
              byDivisor <- byDivisor - 1
            } else break
          }
        } else sample <- combn(candidateZetas, desiredNumZetas)
      }
      
      for (s in 1:length(sample[1,])) {
        z <- sort(sample[,s])
        if (type == "ROC")  zetas <- c(-Inf,z,+Inf) else zetas <- c(z,+Inf) 
        nl_ll_B <- cut(nl_ll, zetas, labels = FALSE, right = FALSE)
        nl_ll_B[is.na(nl_ll_B)] <- -Inf
        nl_B <- array(-Inf, dim = c(K1+K2,nlDim[4]))
        ll_B <- array(-Inf, dim = c(K2,llDim[4]))
        if (FOM == "FROC") {      
          nl_B[x] <- nl_ll_B[1:lx]
          ll_B[1:K2,][y] <- nl_ll_B[(lx+1):(lx+ly)]
        } else {
          nl_B[1:K1,][x] <- nl_ll_B[1:lx]
          ll_B[1:K2,][y] <- nl_ll_B[(lx+1):(lx+ly)]
        }
        dim(nl_B) <- c(K, nlDim[4])
        dim(ll_B) <- c(K2, llDim[4])
        fom_ij <- MyFom_ij(nl_B, ll_B, dataset$lesions$perCase, dataset$lesions$IDs, 
                           dataset$lesions$weights, nlDim[4], llDim[4], K1, K2, FOM, FPFValue)
        if (fom_ij > maxFom){
          if (DEBUG) cat(sprintf("higher fom found, i = %d, j = %d, s = %d, fom = %f\n", i, j, s, fom_ij))
          maxFom <- fom_ij
          zetas_ij[i,j,1:length(z)] <- z      # save the ctff values yielding max fom here
          NL_B[i,j,1:K1,] <- nl_B[1:K1,]      # save the binned NL values yielding max fom here
          LL_B[i,j,,] <- ll_B                 # do:             LL
        }
      }
      if (DEBUG) cat("\n")
    }
    if (DEBUG) cat("\n")
  } 
  
  # return the binned dataset
  binned <- all(isBinned(NL_B, LL_B)) # TBA this needs to be fixed
  fileName <- NA
  name <- NA
  design <- "FCTRL"
  truthTableStr <- NA
  type <- dataset$descriptions$type # sic; dataset not datasetB
  perCase <- dataset$lesions$perCase
  IDs <- dataset$lesions$IDs
  weights <- dataset$lesions$weights
  datasetB <- convert2dataset(NL_B, LL_B, LL_IL = NA, 
                              perCase, IDs, weights,
                              binned, fileName, type, name, truthTableStr, design,
                              dataset$descriptions$modalityID, dataset$descriptions$readerID)
  
  if (DEBUG) {
    fomFinal <- as.matrix(UtilFigureOfMerit(datasetB, FOM = FOM))
    print(fomFinal)
    cat("mean, sd = ", mean(fomFinal), sd(fomFinal), "\n")
  }
  
  return(datasetB)
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
  weights <- dataset$lesions$weights
  type <- dataset$descriptions$type
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





