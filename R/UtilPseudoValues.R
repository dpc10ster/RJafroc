#' Calculate pseudovalues
#' 
#' Calculates \strong{centered} jackknife pseudovalues AND jackknife FOM values
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}.
#' @param FOM The figure of merit to be used in the calculation. 
#'    The default is \code{"FOM_wAFROC"}. See \code{\link{UtilFigureOfMerit}}.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list containing two \code{c(I, J, K)} arrays containing the pseudovalues
#'    and the jackknife FOM values of the datasets.
#' 
#' @examples
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")$jkPseudoValues[1,1,1:10]
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")$jkFomValues[1,1,1:10]
#' UtilPseudoValues(dataset05, FOM = "wAFROC")$jkPseudoValues[1,1,1:10]
#' UtilPseudoValues(dataset05, FOM = "wAFROC")$jkFomValues[1,1,1:10]
#' 
#' @export

# UtilPseudoValues.R had errors insofar as it was dropping the 1 dimension in 
# lesionID and lesionWeight; was affecting StSingleModality when used with 
# wAFROC FOM. This part of the code needs further checking; 
# no essential changes made in MyFOM.cpp and gpfMyFOM.R.
# 
UtilPseudoValues <- function(dataset, FOM, FPFValue = 0.2){
  dataType <- dataset$dataType
  if (dataType != "LROC") {
    NL <- dataset$NL
    LL <- dataset$LL
  } else {
    if (FOM == "Wilcoxon"){
      datasetRoc <- DfLroc2Roc(dataset)
      NL <- datasetRoc$NL
      LL <- datasetRoc$LL
    } else if (FOM %in% c("PCL", "ALROC")){
      NL <- dataset$NL
      LL <- dataset$LLCl
    } else stop("incorrect FOM for LROC data")
  }
  lesionVector <- dataset$lesionVector
  lesionID <- dataset$lesionID
  lesionWeight <- dataset$lesionWeight
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    jkFomValues <- array(dim = c(I, J, K1))
    jkPseudoValues <- array(dim = c(I, J, K1))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K1) {
          nl <- NL[i, j, -k, ]
          ll <- LL[i, j, , ]
          dim(nl) <- c(K - 1, maxNL)
          dim(ll) <- c(K2, max(lesionVector))
          jkFomValues[i, j, k] <- gpfMyFOM(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM, FPFValue)
          jkPseudoValues[i, j, k] <- fomArray[i, j] * K1 - jkFomValues[i, j, k] * (K1 - 1)
        }
        jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] + (fomArray[i, j] - mean(jkPseudoValues[i, j, ]))
      }
    }
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    jkFomValues <- array(dim = c(I, J, K2))
    jkPseudoValues <- array(dim = c(I, J, K2))
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
          jkFomValues[i, j, k] <- gpfMyFOM(nl, ll, lesionVector[-k], lesID, lesWght, maxNL, maxLL, K1, K2 - 1, FOM, FPFValue)
          jkPseudoValues[i, j, k] <- fomArray[i, j] * K2 - jkFomValues[i, j, k] * (K2 - 1)
        }
        jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] + (fomArray[i, j] - mean(jkPseudoValues[i, j, ]))
      }
    }
  } else {
    jkFomValues <- array(dim = c(I, J, K))
    jkPseudoValues <- array(dim = c(I, J, K))
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K) {
          if (k <= K1) {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(lesionVector))
            jkFomValues[i, j, k] <- gpfMyFOM(nl, ll, lesionVector, lesionID, lesionWeight, maxNL, maxLL, K1 - 1, K2, FOM, FPFValue)
          } else {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, -(k - K1), ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(lesionVector))
            lesWght <- lesionWeight[-(k - K1), ]
            dim(lesWght) <- c(K2 - 1, max(lesionVector))
            lesID <- lesionID[-(k - K1), ]
            dim(lesID) <- c(K2 - 1, max(lesionVector))
            jkFomValues[i, j, k] <- gpfMyFOM(nl, ll, lesionVector[-(k - K1)], lesID, lesWght, maxNL, maxLL, K1, K2 - 1, FOM, FPFValue)
          }
          jkPseudoValues[i, j, k] <- fomArray[i, j] * K - jkFomValues[i, j, k] * (K - 1)
        }
        jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] + (fomArray[i, j] - mean(jkPseudoValues[i, j, ]))
      }
    }
  }
  return(list(
    jkPseudoValues = jkPseudoValues, 
    jkFomValues = jkFomValues
  ))
}