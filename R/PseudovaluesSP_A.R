# simplified for debugging
PseudovaluesSP_A <- function(dataset, FOM) {
  
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  t <- dataset$descriptions$truthTableStr
  
  jkFomValues <- array(dim = c(I, J/I, K))
  jkPseudoValues <- array(dim = c(I, J/I, K))
  
  fomArray <- FOM_SP(dataset, FOM) 
  
  lastCase <- 0
  caseTransitions <- array(dim = J)
  for (i in 1:I) {
    for (j in (J/I*(i-1)+1):(J/I*i)) {
      j1 <- j-J/I*(i-1) 
      perCase_ij <- dataset$lesions$perCase 
      lID_ij <- dataset$lesions$IDs
      lW_ij <- dataset$lesions$weights
      nl_ij <- NL[i, j, 1:K, 1:maxNL]; dim(nl_ij) <- c(K, maxNL)
      ll_ij <- LL[i, j, 1:K2, 1:maxLL]; dim(ll_ij) <- c(K2, maxLL)
      
      for (k in 1:K) {
        if (k <= K1) {
          nl_ij_jk <- nl_ij[-k, ];dim(nl_ij_jk) <- c(K - 1, maxNL)
          ll_ij_jk <- ll_ij;dim(ll_ij_jk) <- c(K2, maxLL)
          lID_ij_jk <- lID_ij;dim(lID_ij_jk) <- c(K2, maxLL)
          lW_ij_jk <- lW_ij;dim(lW_ij_jk) <- c(K2, maxLL)
          jkFomValues[i, j1, k] <- 
            MyFom_ij_SP(nl_ij_jk, ll_ij_jk, perCase_ij, 
                        lID_ij_jk, lW_ij_jk, maxNL, maxLL, 
                        K1 - 1, K2, FOM)
          
          jkPseudoValues[i, j1, k] <- 
            fomArray[i, j1] * K - jkFomValues[i, j1, k] * (K - 1)
        } else { # (k > K1)
          nl_ij_jk <- nl_ij[-k, ];dim(nl_ij_jk) <- c(K - 1, maxNL)
          ll_ij_jk <- ll_ij[-(k - K1), ];dim(ll_ij_jk) <- c(K2 - 1, maxLL)
          lV_ij_jk <- perCase_ij[-(k - K1)]
          lW_ij_jk <- lW_ij[-(k - K1), ];dim(lW_ij_jk) <- c(K2 - 1, maxLL)
          lID_ij_jk <- lID_ij[-(k - K1), ];dim(lID_ij_jk) <- c(K2 - 1, maxLL)
          jkFomValues[i, j1, k] <- 
            MyFom_ij_SP(nl_ij_jk, ll_ij_jk, lV_ij_jk, 
                        lID_ij_jk, lW_ij_jk, maxNL, maxLL, 
                        K1, K2 - 1, FOM)
          jkPseudoValues[i, j1, k] <- 
            fomArray[i, j1] * K - jkFomValues[i, j1, k] * (K - 1)
        }
      }
      # center the pseudovalues 
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
        # FOMs defined over NORMAL cases
        jkPseudoValues[i, j1, ] <- jkPseudoValues[i, j1, ] + 
          (fomArray[i, j1] - mean(jkPseudoValues[i, j1, ]))
      }  else if (FOM %in% c("MaxLLF", "HrSe")) {
        # FOMs defined over ABNORMAL cases
        jkPseudoValues[i, j1, ] <- 
          jkPseudoValues[i, j1, ] + 
          (fomArray[i, j1] - mean(jkPseudoValues[i, j1, ]))
      } else {
        # FOMs defined over ALL cases
        jkPseudoValues[i, j1, ] <- 
          jkPseudoValues[i, j1, ] + (fomArray[i, j1] - mean(jkPseudoValues[i, j1, ]))
      }
      caseTransitions[j] <- lastCase
      lastCase <- (lastCase + K) %% K
    }
  }
  
  # there should not be any NAs in each of the following arrays
  # any(is.na(jkPseudoValues))
  # [1] FALSE
  # any(is.na(jkFomValues))
  # [1] FALSE
  # any(is.na(fomArray))
  # [1] FALSE
  
  caseTransitions <- c(caseTransitions, K)
  return(list(
    jkPseudoValues = jkPseudoValues, 
    jkFomValues = jkFomValues,
    caseTransitions = caseTransitions
  ))
}


