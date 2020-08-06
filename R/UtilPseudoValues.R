#' Calculate pseudovalues for given dataset and FOM
#' 
#' Calculates \strong{centered} jackknife pseudovalues AND jackknife FOM values, 
#'    for factorial, split-plot-a and split-plot-c study designs
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}};
#'    must be factorial, or split-plot-a and split-plot-c.
#' @param FOM The figure of merit to be used in the calculation. 
#'    The default is \code{"FOM_wAFROC"}. See \code{\link{UtilFigureOfMerit}}.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list containing two \code{c(I, J, K)} arrays containing the pseudovalues
#'    and the jackknife FOM values of the datasets (a third returned value 
#'    is for internal use)
#' 
#' @examples
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")$jkPseudoValues[1,1,1:10]
#' UtilPseudoValues(dataset02, FOM = "Wilcoxon")$jkFomValues[1,1,1:10]
#' UtilPseudoValues(dataset05, FOM = "wAFROC")$jkPseudoValues[1,1,1:10]
#' UtilPseudoValues(dataset05, FOM = "wAFROC")$jkFomValues[1,1,1:10]
#' 
#' @export

UtilPseudoValues <- function(dataset, FOM, FPFValue = 0.2) {
  dataType <- dataset$descriptions$type
  if (dataType != "LROC") {
    NL <- dataset$ratings$NL
    LL <- dataset$ratings$LL
  } else {
    if (FOM == "Wilcoxon"){
      datasetRoc <- DfLroc2Roc(dataset)
      NL <- datasetRoc$ratings$NL
      LL <- datasetRoc$ratings$LL
    } else if (FOM %in% c("PCL", "ALROC")){
      NL <- dataset$ratings$NL
      LL <- dataset$ratings$LL
    } else stop("incorrect FOM for LROC data")
  }
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    # FOMs defined over NORMAL cases
    jkFomValues <- array(dim = c(I, J, K1))
    jkPseudoValues <- array(dim = c(I, J, K1))
  }  else if (FOM %in% c("MaxLLF", "HrSe")) {
    # FOMs defined over ABNORMAL cases
    jkFomValues <- array(dim = c(I, J, K2))
    jkPseudoValues <- array(dim = c(I, J, K2))
  } else {
    # FOMs defined over ALL cases
    jkFomValues <- array(dim = c(I, J, K))
    jkPseudoValues <- array(dim = c(I, J, K))
  }
  
  t <- dataset$descriptions$truthTableStr
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  lastCase <- 0
  caseTransitions <- array(dim = J)
  for (i in 1:I) {
    for (j in 1:J) {
      # NOTATION
      # k1_ij_logi = logical array of NORMAL cases meeting the i,j criteria, length K1 
      # k2_ij_logi = logical array of ABNORMAL cases meeting the i,j criteria, length K2 
      # k_ij_logi = logical array of ALL cases meeting the i,j criteria, length K 
      k1_ij_logi <- !is.na(t[i,j,,1])
      # i.e., indices of normal cases meeting the i,j criteria
      k2_ij_logi <- !is.na(t[i,j,,2])[(K1+1):K]
      # i.e., indices of abnormal cases meeting the i,j criteria
      k_ij_logi <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) 
      # i.e., indices of all cases meeting the i,j criteria
      if (sum(k_ij_logi) == 0) next
      perCase_ij <- dataset$lesions$perCase[k2_ij_logi] 
      # i.e., perCase indices for all abnormal cases meeting the i,j criteria
      K1_ij <- sum(!is.na(t[i,j,,1]))
      K2_ij <- sum(!is.na(t[i,j,,2]))
      K_ij <- K1_ij + K2_ij
      lID_ij <- dataset$lesions$IDs[k2_ij_logi,1:maxLL, drop = FALSE]
      lW_ij <- dataset$lesions$weights[k2_ij_logi,1:maxLL, drop = FALSE]
      nl_ij <- NL[i, j, k_ij_logi, 1:maxNL]; dim(nl_ij) <- c(K_ij, maxNL)
      # i.e., NL ratings for all cases meeting the i,j criteria
      ll_ij <- LL[i, j, k2_ij_logi, 1:maxLL]; dim(ll_ij) <- c(K2_ij, maxLL)
      # i.e., LL ratings for all cases meeting the i,j criteria
      
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) { 
        for (k in 1:K1_ij) {
          kIndxAll <- which(k_ij_logi)[k];if (is.na(kIndxAll)) 
            stop("Indexing error in UtilPseudoValues")
          # FOMs defined over NORMAL cases
          nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
          llij_jk <- ll_ij;dim(llij_jk) <- c(K2_ij, maxLL)
          lV_j_jk <- perCase_ij
          lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
          lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
          if (is.na(jkFomValues[i, j, kIndxAll])) {
            jkFomValues[i, j, kIndxAll] <- 
              MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                       lID_j_jk, lW_j_jk, maxNL, maxLL, 
                       K1_ij - 1, K2_ij, FOM, FPFValue)
          } else stop("overwriting UtilPseudoValues")
          if (is.na(jkPseudoValues[i, j, kIndxAll])) {
            jkPseudoValues[i, j, kIndxAll] <- 
              fomArray[i, j] * K1_ij - jkFomValues[i, j, kIndxAll] * (K1_ij - 1)
          } else stop("overwriting UtilPseudoValues")
        }
      } else if (FOM %in% c("MaxLLF", "HrSe")) { 
        # FOMs defined over ABNORMAL cases
        for (k in 1:K2_ij) {
          kIndxAbn <- which(k2_ij_logi)[k];if (is.na(kIndxAbn)) 
            stop("Indexing error in UtilPseudoValues")
          nlij_jk <- nl_ij[-(k+K1_ij), ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
          llij_jk <- ll_ij[-k, ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
          lV_j_jk <- perCase_ij[-k]
          lW_j_jk <- lW_ij[-k, ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
          lID_j_jk <- lID_ij[-k, ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
          if (is.na(jkFomValues[i, j, kIndxAbn])) {
            jkFomValues[i, j, kIndxAbn] <- 
              MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                       lID_j_jk, lW_j_jk, maxNL, maxLL, 
                       K1_ij, K2_ij - 1, FOM, FPFValue)
          } else stop("overwriting UtilPseudoValues 3")
          if (is.na(jkPseudoValues[i, j, kIndxAbn])) {
            jkPseudoValues[i, j, kIndxAbn] <- 
              fomArray[i, j] * K2_ij - jkFomValues[i, j, kIndxAbn] * (K2_ij - 1)
          } else stop("overwriting UtilPseudoValues")
        }
      } else { 
        # FOMs defined over ALL cases
        for (k in 1:K_ij) {
          kIndxAll <- which(k_ij_logi)[k];if (is.na(kIndxAll)) 
            stop("Indexing error in UtilPseudoValues")
          if (k <= K1_ij) {
            nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_ij;dim(llij_jk) <- c(K2_ij, maxLL)
            lV_j_jk <- perCase_ij
            lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
            lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
            if (is.na(jkFomValues[i, j, kIndxAll])) {
              jkFomValues[i, j, kIndxAll] <- 
                MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                         lID_j_jk, lW_j_jk, maxNL, maxLL, 
                         K1_ij - 1, K2_ij, FOM, FPFValue)
            } else stop("overwriting UtilPseudoValues")
            if (is.na(jkPseudoValues[i, j, kIndxAll])) {
              jkPseudoValues[i, j, kIndxAll] <- 
                fomArray[i, j] * K_ij - jkFomValues[i, j, kIndxAll] * (K_ij - 1)
            } else stop("overwriting UtilPseudoValues")
          } else {
            nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_ij[-(k - K1_ij), ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
            lV_j_jk <- perCase_ij[-(k - K1_ij)]
            lW_j_jk <- lW_ij[-(k - K1_ij), ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
            lID_j_jk <- lID_ij[-(k - K1_ij), ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
            if (is.na(jkFomValues[i, j, kIndxAll])) {
              jkFomValues[i, j, kIndxAll] <- 
                MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                         lID_j_jk, lW_j_jk, maxNL, maxLL, 
                         K1_ij, K2_ij - 1, FOM, FPFValue)
            } else stop("overwriting UtilPseudoValues")
            if (is.na(jkPseudoValues[i, j, kIndxAll])) {
              jkPseudoValues[i, j, kIndxAll] <- 
                fomArray[i, j] * K_ij - jkFomValues[i, j, kIndxAll] * (K_ij - 1)
            } else stop("overwriting UtilPseudoValues")
          }
        }
      }
      # center the pseudovalues 
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
        # FOMs defined over NORMAL cases
        jkPseudoValues[i, j, which(k1_ij_logi)] <- 
          jkPseudoValues[i, j, which(k1_ij_logi)] + 
          (fomArray[i, j] - mean(jkPseudoValues[i, j, which(k1_ij_logi)]))
      }  else if (FOM %in% c("MaxLLF", "HrSe")) {
        # FOMs defined over ABNORMAL cases
        jkPseudoValues[i, j, which(k2_ij_logi)] <- 
          jkPseudoValues[i, j, which(k2_ij_logi)] + 
          (fomArray[i, j] - mean(jkPseudoValues[i, j, which(k2_ij_logi)]))
      } else {
        # FOMs defined over ALL cases
        jkPseudoValues[i, j, which(k_ij_logi)] <- 
          jkPseudoValues[i, j, which(k_ij_logi)] + 
          (fomArray[i, j] - mean(jkPseudoValues[i, j, which(k_ij_logi)]))
      }
      caseTransitions[j] <- lastCase
      lastCase <- (lastCase + K_ij) %% K
    }
  }
  
  caseTransitions <- c(caseTransitions, K)
  return(list(
    jkPseudoValues = jkPseudoValues, 
    jkFomValues = jkFomValues,
    caseTransitions = caseTransitions
  ))
}




UtilPseudoValuesOrg <- function(dataset, FOM, FPFValue) {
  
  # return(UtilPseudoValues(dataset, FOM, FPFValue))
  
  # # cannot use "MaxNLF", "ExpTrnsfmSp", "HrSp" etc. here
  # # TBA this restriction will eventually be removed
  # if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp", "MaxLLF", "HrSe"))
  #   stop("Cannot use MaxNLF, ExpTrnsfmSp, HrSp, MaxLLF, HrSe FOMs with SPLIT-PLOT-A dataset")
  #

  if (!(dataset$descriptions$design %in% c("FCTRL", "SPLIT-PLOT-A", "SPLIT-PLOT-C"))) stop("Unrecognized study design: should be FCTRL, SPLIT-PLOT-A or SPLIT-PLOT-C")

  dataType <- dataset$descriptions$type
  if (dataType != "LROC") {
    NL <- dataset$ratings$NL
    LL <- dataset$ratings$LL
  } else {
    if (FOM == "Wilcoxon"){
      datasetRoc <- DfLroc2Roc(dataset)
      NL <- datasetRoc$ratings$NL
      LL <- datasetRoc$ratings$LL
    } else if (FOM %in% c("PCL", "ALROC")){
      NL <- dataset$ratings$NL
      LL <- dataset$ratings$LL
    } else stop("incorrect FOM for LROC data")
  }

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
      k_ij_logi <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) # must include abnormal cases
      # i.e., k1-indices of all cases meeting the i,j criteria
      k2_ij_logi <- !is.na(t[i,j,,2])[(K1+1):K]
      if ((sum(k_ij_logi) == 0) || (sum(k2_ij_logi) == 0)) next
      # i.e., k2-indices of all cases meeting the i,j criteria
      perCase_ij <- dataset$lesions$perCase[k2_ij_logi]
      # i.e., perCase indices for all abnormal cases meeting the i,j criteria
      k1_ij <- sum(!is.na(t[i,j,,1]))
      k2_ij <- sum(!is.na(t[i,j,,2]))
      k_ij <- k1_ij + k2_ij
      lID_ij <- dataset$lesions$IDs[k2_ij_logi,1:maxLL, drop = FALSE]
      lW_ij <- dataset$lesions$weights[k2_ij_logi,1:maxLL, drop = FALSE]
      nl_ij <- NL[i, j, k_ij_logi, 1:maxNL]; dim(nl_ij) <- c(k_ij, maxNL)
      # i.e., NL ratings for all cases meeting the i,j criteria
      ll_ij <- LL[i, j, k2_ij_logi, 1:maxLL]; dim(ll_ij) <- c(k2_ij, maxLL)
      # i.e., LL ratings for all cases meeting the i,j criteria
      fom_ij <-
        MyFom_ij(nl_ij, ll_ij, perCase_ij, lID_ij, lW_ij, maxNL, maxLL,
                 k1_ij, k2_ij, FOM, FPFValue)
      for (k in 1:k_ij) {
        kIndx <- which(k_ij_logi)[k];if (is.na(kIndx)) stop("Indexing error in UtilPseudoValues, line 90")
        if (k <= k1_ij) {
          nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(k_ij - 1, maxNL)
          llij_jk <- ll_ij;dim(llij_jk) <- c(k2_ij, maxLL)
          if (is.na(jkFomValues[i, j, kIndx])) {
            jkFomValues[i, j, kIndx] <-
              MyFom_ij(nlij_jk, llij_jk, perCase_ij, lID_ij, lW_ij, maxNL, maxLL,
                       k1_ij - 1, k2_ij, FOM, FPFValue)
          } else stop("double initialization 1")
        } else {
          nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(k_ij - 1, maxNL)
          llij_jk <- ll_ij[-(k - k1_ij), ];dim(llij_jk) <- c(k2_ij - 1, maxLL)
          lV_j_jk <- perCase_ij[-(k - k1_ij)]
          lW_j_jk <- lW_ij[-(k - k1_ij), ];dim(lW_j_jk) <- c(k2_ij - 1, maxLL)
          lID_j_jk <- lID_ij[-(k - k1_ij), ];dim(lID_j_jk) <- c(k2_ij - 1, maxLL)
          if (is.na(jkFomValues[i, j, kIndx])) {
            jkFomValues[i, j, kIndx] <-
              MyFom_ij(nlij_jk, llij_jk, lV_j_jk, lID_j_jk, lW_j_jk, maxNL, maxLL,
                       k1_ij, k2_ij - 1, FOM, FPFValue)
          } else stop("double initialization 2")
        }
        if (is.na(jkPseudoValues[i, j, kIndx])) {
          jkPseudoValues[i, j, kIndx] <-
            fom_ij * k_ij - jkFomValues[i, j, kIndx] * (k_ij - 1)
        } else stop("double initialization 3")
      }
      # case_sub <- (lastCase+1):(lastCase+k_ij) # error here affected SpA
      # centering correction aka "normalization" by Hillis
      jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] +
        (fom_ij - mean(jkPseudoValues[i, j, which(k_ij_logi)]))
      caseTransitions[j] <- lastCase
      lastCase <- (lastCase + k_ij) %% K
    }
  }

  caseTransitions <- c(caseTransitions, K)
  return(list(
    jkPseudoValues = jkPseudoValues,
    jkFomValues = jkFomValues,
    caseTransitions = caseTransitions
  ))
  
}



