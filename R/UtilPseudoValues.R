#' Calculate pseudovalues
#' 
#' Calculates centered pseudovalues using the jackknife
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}.
#' @param FOM The figure of merit to be used in the calculation. 
#'    The default is \code{"wJAFROC"}. See \code{\link{UtilFigureOfMerit}}.
#' 
#' @return An \code{c(I, J, K)} array containing the pseudovalues of the datasets.
#' 
#' @examples
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")[1,1,1:10]
#' UtilPseudoValues(dataset05, FOM = "wAFROC")[1,1,1:10] # default FOM is wAFROC for this FROC dataset
#' 
#' @export

# UtilPseudoValues.R had errors insofar as it was dropping the 1 dimension in 
# lesionID and lesionWeight; was affecting StSingleModality when used with 
# wAFROC FOM. This part of the code needs further checking; 
# no essential changes made in MyFOM.cpp and gpfMyFOM.R
UtilPseudoValues <- function(dataset, FOM = "Wilcoxon"){
  NL <- dataset$NL
  LL <- dataset$LL
  lesionVector <- dataset$lesionVector
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  fomArray <- UtilFigureOfMerit(dataset, FOM)
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    jkFOMArray <- array(dim = c(I, J, K1))
    pseudoValues <- array(dim = c(I, J, K1))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K1) {
          nl <- NL[i, j, -k, ]
          ll <- LL[i, j, , ]
          dim(nl) <- c(K - 1, maxNL)
          dim(ll) <- c(K2, max(lesionVector))
          jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM)
          pseudoValues[i, j, k] <- fomArray[i, j] * K1 - jkFOMArray[i, j, k] * (K1 - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    jkFOMArray <- array(dim = c(I, J, K2))
    pseudoValues <- array(dim = c(I, J, K2))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K2) {
          nl <- NL[i, j, -(k + K1), ]
          ll <- LL[i, j, -k, ]
          dim(nl) <- c(K - 1, maxNL)
          dim(ll) <- c(K2 - 1, max(lesionVector))
          lesID <- lesionID[-k, ]
          dim(lesID) <- c(K2 - 1, max(lesionVector))
          lesWght <- lesionWeight[-k, ]
          dim(lesWght) <- c(K2 - 1, max(lesionVector))
          jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionVector[-k], lesID, lesWght, maxNL, maxLL, K1, K2 - 1, FOM)
          pseudoValues[i, j, k] <- fomArray[i, j] * K2 - jkFOMArray[i, j, k] * (K2 - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  } else {
    jkFOMArray <- array(dim = c(I, J, K))
    pseudoValues <- array(dim = c(I, J, K))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K) {
          if (k <= K1) {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(lesionVector))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM)
          } else {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, -(k - K1), ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(lesionVector))
            lesWght <- lesionWeight[-(k - K1), ]
            dim(lesWght) <- c(K2 - 1, max(lesionVector))
            lesID <- lesionID[-(k - K1), ]
            dim(lesID) <- c(K2 - 1, max(lesionVector))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionVector[-(k - K1)], lesID, lesWght, maxNL, maxLL, K1, K2 - 1, FOM)
          }
          pseudoValues[i, j, k] <- fomArray[i, j] * K - jkFOMArray[i, j, k] * (K - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  }
  return(pseudoValues)
}