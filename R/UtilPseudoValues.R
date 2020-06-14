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

# UtilPseudoValues.R had errors (prior to v1.3.1) insofar as it was 
# dropping the 1 dimension in 
# IDs and weights; was affecting StSingleModality when used with 
# wAFROC FOM. This part of the code needs further checking; 
# no essential changes made in MyFOM.cpp and MyFom_ij.R.
# v.1.3.1.9000: added SPLIT-PLOT capability
# 
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
  perCase <- dataset$lesions$perCase
  IDs <- dataset$lesions$IDs
  weights <- dataset$lesions$weights
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  # `as.matrix` is NOT absolutely necessary as `mean()` function is not used here
  fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  if (dataset$descriptions$design == "FCTRL") {
    if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
      # first type of end-point based FOM
      jkFomValues <- array(dim = c(I, J, K1))
      jkPseudoValues <- array(dim = c(I, J, K1))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K1) {
            nl <- NL[i, j, -k, ]
            ll <- LL[i, j, , ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2, max(perCase))
            jkFomValues[i, j, k] <- MyFom_ij(nl, ll, perCase, IDs, weights, maxNL, maxLL, K1 - 1, K2, FOM, FPFValue)
            jkPseudoValues[i, j, k] <- fomArray[i, j] * K1 - jkFomValues[i, j, k] * (K1 - 1)
          }
          jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] + (fomArray[i, j] - mean(jkPseudoValues[i, j, ]))
        }
      }
    } else if (FOM %in% c("MaxLLF", "HrSe")) {
      # second type of end-point based FOM
      jkFomValues <- array(dim = c(I, J, K2))
      jkPseudoValues <- array(dim = c(I, J, K2))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K2) {
            nl <- NL[i, j, -(k + K1), ]
            ll <- LL[i, j, -k, ]
            dim(nl) <- c(K - 1, maxNL)
            dim(ll) <- c(K2 - 1, max(perCase))
            lesID <- IDs[-k, ]
            dim(lesID) <- c(K2 - 1, max(perCase))
            lesWght <- weights[-k, ]
            dim(lesWght) <- c(K2 - 1, max(perCase))
            jkFomValues[i, j, k] <- MyFom_ij(nl, ll, perCase[-k], lesID, lesWght, maxNL, maxLL, K1, K2 - 1, FOM, FPFValue)
            jkPseudoValues[i, j, k] <- fomArray[i, j] * K2 - jkFomValues[i, j, k] * (K2 - 1)
          }
          jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] + (fomArray[i, j] - mean(jkPseudoValues[i, j, ]))
        }
      }
    } else {
      # full curve based FOM
      jkFomValues <- array(dim = c(I, J, K))
      jkPseudoValues <- array(dim = c(I, J, K))
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K) {
            if (k <= K1) {
              nl <- NL[i, j, -k, ]
              ll <- LL[i, j, , ]
              dim(nl) <- c(K - 1, maxNL)
              dim(ll) <- c(K2, max(perCase))
              jkFomValues[i, j, k] <- MyFom_ij(nl, ll, perCase, IDs, weights, maxNL, maxLL, K1 - 1, K2, FOM, FPFValue)
            } else {
              nl <- NL[i, j, -k, ]
              ll <- LL[i, j, -(k - K1), ]
              dim(nl) <- c(K - 1, maxNL)
              dim(ll) <- c(K2 - 1, max(perCase))
              lesWght <- weights[-(k - K1), ]
              dim(lesWght) <- c(K2 - 1, max(perCase))
              lesID <- IDs[-(k - K1), ]
              dim(lesID) <- c(K2 - 1, max(perCase))
              jkFomValues[i, j, k] <- MyFom_ij(nl, ll, perCase[-(k - K1)], lesID, lesWght, maxNL, maxLL, K1, K2 - 1, FOM, FPFValue)
            }
            jkPseudoValues[i, j, k] <- fomArray[i, j] * K - jkFomValues[i, j, k] * (K - 1)
          }
          jkPseudoValues[i, j, ] <- jkPseudoValues[i, j, ] + (fomArray[i, j] - mean(jkPseudoValues[i, j, ]))
        }
      }
    }
    return(list(
      jkPseudoValues = jkPseudoValues, 
      jkFomValues = jkFomValues,
      caseTransitions = NULL
    ))
  } else if (dataset$descriptions$design == "SPLIT-PLOT") {
    # cannot use "MaxNLF", "ExpTrnsfmSp", "HrSp" etc. here 
    if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp", "MaxLLF", "HrSe")) 
      stop("Cannot use MaxNLF, ExpTrnsfmSp, HrSp, MaxLLF, HrSe FOMs with SPLIT-PLOT dataset")
    if (dataset$descriptions$design != "SPLIT-PLOT") stop("Dataset has to be split-plot for this function to be called")
    t <- dataset$descriptions$truthTableStr
    jkFomValues <- array(dim = c(I,J,K))
    jkPseudoValues <- array(dim =c(I,J,K))
    lastCase <- 0
    caseTransitions <- array(dim = J)
    for (j in 1:J) {
      k1_j_sub <- !is.na(t[1,j,,1]) | !is.na(t[1,j,,2])
      k2_j_sub <- !is.na(t[1,j,,2])[(K1+1):K]
      lV_j <- dataset$lesions$perCase[k2_j_sub]
      maxLL_j <- max(lV_j)
      k1_j <- sum(!is.na(t[1,j,,1]))
      k2_j <- sum(!is.na(t[1,j,,2]))
      k_j <- k1_j + k2_j
      lID_j <- dataset$lesions$IDs[k2_j_sub,1:maxLL_j, drop = FALSE]
      lW_j <- dataset$lesions$weights[k2_j_sub,1:maxLL_j, drop = FALSE]
      for (i in 1:I) {
        nl_j <- NL[i, j, k1_j_sub, ]
        ll_j <- LL[i, j, k2_j_sub, 1:maxLL_j]
        dim(nl_j) <- c(k1_j+k2_j, maxNL)
        dim(ll_j) <- c(k2_j, maxLL_j)
        fom_ijk <- MyFom_ij(nl_j, ll_j, lV_j, lID_j, lW_j, maxNL, maxLL_j, k1_j, k2_j, FOM, FPFValue)
        for (k in 1:k_j) {
          if (k <= k1_j) {
            nlj_jk <- nl_j[-k, ]
            llj_jk <- ll_j[, ]
            dim(nlj_jk) <- c(k_j - 1, maxNL)
            dim(llj_jk) <- c(k2_j, max(lV_j))
            jkFomValues[i, j, lastCase+k] <- MyFom_ij(nlj_jk, llj_jk, lV_j, lID_j, lW_j, maxNL, maxLL_j, k1_j - 1, k2_j, FOM, FPFValue)
          } else {
            nlj_jk <- nl_j[-k, ]
            llj_jk <- ll_j[-(k - k1_j), ]
            dim(nlj_jk) <- c(k_j - 1, maxNL)
            dim(llj_jk) <- c(k2_j - 1, maxLL_j)
            lV_j_jk <- lV_j[-(k - k1_j)]
            lW_j_jk <- lW_j[-(k - k1_j), ]
            dim(lW_j_jk) <- c(k2_j - 1, maxLL_j)
            lID_j_jk <- lID_j[-(k - k1_j), ]
            dim(lID_j_jk) <- c(k2_j - 1, maxLL_j)
            jkFomValues[i, j, lastCase+k] <- MyFom_ij(nlj_jk, llj_jk, lV_j_jk, lID_j_jk, lW_j_jk, maxNL, maxLL_j, k1_j, k2_j - 1, FOM, FPFValue)
          }
          jkPseudoValues[i, j, lastCase+k] <- fom_ijk * k_j - jkFomValues[i, j, k] * (k_j - 1)
        }
        case_sub <- (lastCase+1):(lastCase+k_j)
        # centering correction aka "normalization" by Hillis
        jkPseudoValues[i, j, case_sub] <- jkPseudoValues[i, j, case_sub] + (fom_ijk - mean(jkPseudoValues[i, j, case_sub]))
      }
      caseTransitions[j] <- lastCase
      lastCase <- lastCase + k_j
    }
    caseTransitions <- c(caseTransitions, lastCase)
    return(list(
      jkPseudoValues = jkPseudoValues,
      jkFomValues = jkFomValues,
      caseTransitions = caseTransitions
    ))
  } else stop("Unrecognized study design, should be FCTRL or SPLIT-PLOT")
}

