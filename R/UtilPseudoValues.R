#' Calculate pseudovalues
#' 
#' Calculates centered pseudovalues using the jackknife
#' 
#' @param dataset The dataset to be analyzed, see \link{RJafroc-package}.
#' @param FOM The figure of merit to be used in the calculation. 
#'    The default is \code{"wJAFROC"}. See \link{UtilFigureOfMerit}.
#' 
#' @return An \code{c(I, J, K)} array containing the pseudovalues of the datasets.
#' 
#' @examples
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")[1,1,1:10]
#' 
#' UtilPseudoValues(dataset05)[1,1,1:10] # default FOM is wAFROC for this FROC dataset
#' 
#' @export

UtilPseudoValues <- function(dataset, FOM = "wJAFROC"){
  NL <- dataset$NL
  LL <- dataset$LL
  lesionNum <- dataset$lesionNum
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
          dim(ll) <- c(K2, max(lesionNum))
          jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM)
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
          dim(ll) <- c(K2 - 1, max(lesionNum))
          jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum[-k], lesionID[-k, ], lesionWeight[-k, ], maxNL, maxLL, K1, K2 - 1, FOM)
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
            dim(ll) <- c(K2, max(lesionNum))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM)
          } else {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, -(k - K1), ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(lesionNum))
            jkFOMArray[i, j, k] <- gpfMyFOM(nl, ll, lesionNum[-(k - K1)], lesionID[-(k - K1), ], lesionWeight[-(k - K1), ], maxNL, maxLL, K1, K2 - 1, FOM)
          }
          pseudoValues[i, j, k] <- fomArray[i, j] * K - jkFOMArray[i, j, k] * (K - 1)
        }
        pseudoValues[i, j, ] <- pseudoValues[i, j, ] + (fomArray[i, j] - mean(pseudoValues[i, j, ]))
      }
    }
  }
  return(pseudoValues)
}