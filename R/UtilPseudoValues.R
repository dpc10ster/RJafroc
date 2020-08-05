#' Calculate pseudovalues
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


UtilPseudoValues <- function(dataset, FOM, FPFValue) {
  
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
      # if ((all(is.na(t[i,j,,1]))) || all(is.na(t[i,j,,2]))) next 
      # if t[] for all normal or abnormal cases for selected i,j are NAs, skip 
      k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) 
      # k1-indices of all cases meeting the i,j criteria, must include abnormal cases
      k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K]
      if ((sum(k1_ij_sub) == 0) || (sum(k2_ij_sub) == 0)) next
      # k2-indices of all cases meeting the i,j criteria
      perCase_ij <- dataset$lesions$perCase[k2_ij_sub] 
      # perCase indices for all abnormal cases meeting the i,j criteria
      k1_ij <- sum(!is.na(t[i,j,,1]))
      k2_ij <- sum(!is.na(t[i,j,,2]))
      k_ij <- k1_ij + k2_ij
      lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL, drop = FALSE]
      lW_ij <- dataset$lesions$weights[k2_ij_sub,1:maxLL, drop = FALSE]
      nl_ij <- NL[i, j, k1_ij_sub, 1:maxNL]; dim(nl_ij) <- c(k_ij, maxNL) 
      # NL ratings for all cases meeting the i,j criteria
      ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL]; dim(ll_ij) <- c(k2_ij, maxLL)
      # LL ratings for all cases meeting the i,j criteria
      fom_ij <- 
        MyFom_ij(nl_ij, ll_ij, perCase_ij, lID_ij, lW_ij, maxNL, maxLL, 
                 k1_ij, k2_ij, FOM, FPFValue)
      for (k in 1:k_ij) {
        kIndx <- which(k1_ij_sub)[k]
        if (k <= k1_ij) {
          nlij_jk <- nl_ij[-k, ]
          llij_jk <- ll_ij[, ]
          dim(nlij_jk) <- c(k_ij - 1, maxNL)
          dim(llij_jk) <- c(k2_ij, maxLL)
          if (is.na(jkFomValues[i, j, kIndx])) {
            jkFomValues[i, j, kIndx] <- 
              MyFom_ij(nlij_jk, llij_jk, perCase_ij, lID_ij, lW_ij, maxNL, maxLL, 
                       k1_ij - 1, k2_ij, FOM, FPFValue)
          } else stop("double initialization 1")
        } else {
          nlij_jk <- nl_ij[-k, ]
          llij_jk <- ll_ij[-(k - k1_ij), ]
          dim(nlij_jk) <- c(k_ij - 1, maxNL)
          dim(llij_jk) <- c(k2_ij - 1, maxLL)
          lV_j_jk <- perCase_ij[-(k - k1_ij)]
          lW_j_jk <- lW_ij[-(k - k1_ij), ]
          dim(lW_j_jk) <- c(k2_ij - 1, maxLL)
          lID_j_jk <- lID_ij[-(k - k1_ij), ]
          dim(lID_j_jk) <- c(k2_ij - 1, maxLL)
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
        (fom_ij - mean(jkPseudoValues[i, j, which(k1_ij_sub)]))
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


UtilPseudoValuesAbnormals <- function(dataset, FOM, FPFValue) {
  
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
  jkFomValues <- array(dim = c(I,J,K2))
  jkPseudoValues <- array(dim =c(I,J,K2))
  lastCase <- 0
  caseTransitions <- array(dim = J)
  for (i in 1:I) {
    for (j in 1:J) {
      # if ((all(is.na(t[i,j,,1]))) || all(is.na(t[i,j,,2]))) next 
      # if t[] for all normal or abnormal cases for selected i,j are NAs, skip 
      k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) 
      # k1-indices of all cases meeting the i,j criteria, must include abnormal cases
      k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K]
      if ((sum(k1_ij_sub) == 0) || (sum(k2_ij_sub) == 0)) next
      # k2-indices of all cases meeting the i,j criteria
      perCase_ij <- dataset$lesions$perCase[k2_ij_sub] 
      # perCase indices for all abnormal cases meeting the i,j criteria
      k1_ij <- sum(!is.na(t[i,j,,1]))
      k2_ij <- sum(!is.na(t[i,j,,2]))
      k_ij <- k1_ij + k2_ij
      lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL, drop = FALSE]
      lW_ij <- dataset$lesions$weights[k2_ij_sub,1:maxLL, drop = FALSE]
      nl_ij <- NL[i, j, k1_ij_sub, 1:maxNL]; dim(nl_ij) <- c(k_ij, maxNL) 
      # NL ratings for all cases meeting the i,j criteria
      ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL]; dim(ll_ij) <- c(k2_ij, maxLL)
      # LL ratings for all cases meeting the i,j criteria
      fom_ij <- 
        MyFom_ij(nl_ij, ll_ij, perCase_ij, lID_ij, lW_ij, maxNL, maxLL, 
                 k1_ij, k2_ij, FOM, FPFValue)
      for (k in 1:k_ij) {
        kIndx <- which(k1_ij_sub)[k]
        if (k <= k1_ij) {
          next
          nlij_jk <- nl_ij[-k, ]
          llij_jk <- ll_ij[, ]
          dim(nlij_jk) <- c(k_ij - 1, maxNL)
          dim(llij_jk) <- c(k2_ij, maxLL)
          if (is.na(jkFomValues[i, j, kIndx])) {
            jkFomValues[i, j, kIndx] <- 
              MyFom_ij(nlij_jk, llij_jk, perCase_ij, lID_ij, lW_ij, maxNL, maxLL, 
                       k1_ij - 1, k2_ij, FOM, FPFValue)
          } else stop("double initialization 1")
        } else {
          nlij_jk <- nl_ij[-k, ]
          llij_jk <- ll_ij[-(k - k1_ij), ]
          dim(nlij_jk) <- c(k_ij - 1, maxNL)
          dim(llij_jk) <- c(k2_ij - 1, maxLL)
          lV_j_jk <- perCase_ij[-(k - k1_ij)]
          lW_j_jk <- lW_ij[-(k - k1_ij), ]
          dim(lW_j_jk) <- c(k2_ij - 1, maxLL)
          lID_j_jk <- lID_ij[-(k - k1_ij), ]
          dim(lID_j_jk) <- c(k2_ij - 1, maxLL)
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
        (fom_ij - mean(jkPseudoValues[i, j, which(k1_ij_sub)]))
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


