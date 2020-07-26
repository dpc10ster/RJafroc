PseudoValuesFactorial <- function(dataset, FOM, FPFValue) {
  
  if (dataset$descriptions$design != "FCTRL") stop("This function requires a factorial study design")
  
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL 
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  jkFomValues <- array(dim = c(I, J, K))
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K) {
        if (k <= K1) {
          if (FOM %in% c("MaxLLF", "HrSe")) next
          nl <- NL[i, j, -k, ]
          ll <- LL[i, j, , ]
          jkFomValues[i, j, k] <- 
            MyFom_ij(nl, ll, dataset$lesions$perCase, 
                     dataset$lesions$IDs, 
                     dataset$lesions$weights, 
                     maxNL, maxLL, K1 - 1, K2, FOM, FPFValue)
        } else {
          if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) next
          nl <- NL[i, j, -k, ]
          ll <- LL[i, j, -(k - K1), ]
          lesWght <- dataset$lesions$weights[-(k - K1), ]
          dim(lesWght) <- c(K2 - 1, max(dataset$lesions$perCase))
          lesID <- dataset$lesions$IDs[-(k - K1), ]
          dim(lesID) <- c(K2 - 1, max(dataset$lesions$perCase))
          jkFomValues[i, j, k] <- 
            MyFom_ij(nl, ll, dataset$lesions$perCase[-(k - K1)], 
                     lesID, lesWght, 
                     maxNL, maxLL, K1, K2 - 1, FOM, FPFValue)
        }
      }
    }
  }
  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    jkPseudoValues <- array(dim = c(I,J,K1))
    jkFomValues <- jkFomValues[, , 1:K1];dim(jkFomValues) <- c(I,J,K1)
  } else if (FOM %in% c("MaxLLF", "HrSe")) {
    jkPseudoValues <- array(dim = c(I,J,K2))
    jkFomValues <- jkFomValues[, , (K1+1):K];dim(jkFomValues) <- c(I,J,K2)
  } else {
    jkPseudoValues <- array(dim = c(I,J,K))
    # jkFomValues already exists
  }
  
  for (i in 1:I) {
    for (j in 1:J) {
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
        jkPseudoValues[i,j,] <- fomArray[i,j] * K1 - jkFomValues[i,j,] * (K1 - 1)
      } else if (FOM %in% c("MaxLLF", "HrSe")) {
        jkPseudoValues[i,j,] <- fomArray[i,j] * K2 - jkFomValues[i,j,] * (K2 - 1)
      } else {
        jkPseudoValues[i,j,] <- fomArray[i,j] * K - jkFomValues[i,j,] * (K - 1)
      }
      # centering transformation
      # this is known not to make a difference for ROC data and Wilcoxon FOM
      # I found this also makes no difference for FROC data and wAFROC FOM
      # is it possible that it makes no difference for any empirical FOM?
      # no; they are not equal for maxLLF
      jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] + (fomArray[i, j] - mean(jkPseudoValues[i, j, ]))
    }
  }
  
  return(list(
    jkPseudoValues = jkPseudoValues, 
    jkFomValues = jkFomValues,
    caseTransitions = NULL
  ))
}