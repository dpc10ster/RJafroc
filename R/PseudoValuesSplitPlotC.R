PseudoValuesSplitPlotC <- function(dataset, FOM, FPFValue) {
  
  if (dataset$descriptions$design != "SPLIT-PLOT-C") 
    stop("This function requires a split plot C study design")
  
  # cannot use "MaxNLF", "ExpTrnsfmSp", "HrSp" etc. here 
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp", "MaxLLF", "HrSe")) 
    stop("Cannot use MaxNLF, ExpTrnsfmSp, HrSp, MaxLLF, HrSe FOMs with SPLIT-PLOT-C dataset")
  
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
  
  t <- dataset$descriptions$truthTableStr
  jkFomValues <- array(dim = c(I,J,K))
  jkPseudoValues <- array(dim =c(I,J,K))
  lastCase <- 0
  caseTransitions <- array(dim = J)
  for (i in 1:I) {
    for (j in 1:J) {
      k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2])
      k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K]
      lV_ij <- dataset$lesions$perCase[k2_ij_sub]
      k1_ij <- sum(!is.na(t[i,j,,1]))
      k2_ij <- sum(!is.na(t[i,j,,2]))
      k_ij <- k1_ij + k2_ij
      lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL, drop = FALSE]
      lW_ij <- dataset$lesions$weights[k2_ij_sub,1:maxLL, drop = FALSE]
      nl_ij <- NL[i, j, k1_ij_sub, ]
      ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL]
      dim(nl_ij) <- c(k1_ij+k2_ij, maxNL)
      dim(ll_ij) <- c(k2_ij, maxLL)
      fom_ijk <- 
        MyFom_ij(nl_ij, ll_ij, lV_ij, lID_ij, lW_ij, maxNL, maxLL, 
                 k1_ij, k2_ij, FOM, FPFValue)
      for (k in 1:k_ij) {
        if (k <= k1_ij) {
          nlij_jk <- nl_ij[-k, ]
          llij_jk <- ll_ij[, ]
          dim(nlij_jk) <- c(k_ij - 1, maxNL)
          dim(llij_jk) <- c(k2_ij, maxLL)
          jkFomValues[i, j, lastCase+k] <- 
            MyFom_ij(nlij_jk, llij_jk, lV_ij, lID_ij, lW_ij, 
                     maxNL, maxLL, k1_ij - 1, k2_ij, FOM, FPFValue)
        } else {
          nlij_jk <- nl_ij[-k, ]
          llij_jk <- ll_ij[-(k - k1_ij), ]
          dim(nlij_jk) <- c(k_ij - 1, maxNL)
          dim(llij_jk) <- c(k2_ij - 1, maxLL)
          lV_ij_jk <- lV_ij[-(k - k1_ij)]
          lW_ij_jk <- lW_ij[-(k - k1_ij), ]
          dim(lW_ij_jk) <- c(k2_ij - 1, maxLL)
          lID_ij_jk <- lID_ij[-(k - k1_ij), ]
          dim(lID_ij_jk) <- c(k2_ij - 1, maxLL)
          jkFomValues[i, j, lastCase+k] <- 
            MyFom_ij(nlij_jk, llij_jk, lV_ij_jk, lID_ij_jk, lW_ij_jk, 
                     maxNL, maxLL, k1_ij, k2_ij - 1, FOM, FPFValue)
        }
        jkPseudoValues[i, j, lastCase+k] <- 
          fom_ijk * k_ij - jkFomValues[i, j, k] * (k_ij - 1)
      }
      case_sub <- (lastCase+1):(lastCase+k_ij)
      # centering correction aka "normalization" by Hillis
      jkPseudoValues[i, j, case_sub] <- jkPseudoValues[i, j, case_sub] + 
        (fom_ijk - mean(jkPseudoValues[i, j, case_sub]))
      caseTransitions[j] <- lastCase
      lastCase <- lastCase + k_ij
    }
  }
  caseTransitions <- c(caseTransitions, lastCase)
  return(list(
    jkPseudoValues = jkPseudoValues,
    jkFomValues = jkFomValues,
    caseTransitions = caseTransitions
  ))
  
}