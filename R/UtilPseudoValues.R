#' Pseudovalues for given dataset and FOM
#' 
#' Returns \strong{centered} jackknife pseudovalues AND jackknife FOM values, 
#'    for factorial OR split-plot-a OR split-plot-c study designs
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}};
#'    must be factorial, or split-plot-a or split-plot-c.
#' @param FOM The figure of merit to be used in the calculation. 
#'    The default is \code{"FOM_wAFROC"}. See \code{\link{UtilFigureOfMerit}}.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list containing two arrays containing the pseudovalues
#'    and the jackknife FOM values of the datasets (a third returned value 
#'    is for internal use). 
#' 
#' @note Each returned array has dimension \code{c(I,J,K)}, where \code{K} depends on the
#'    FOM: \code{K1} for FOMs that are based on normal cases only, \code{K2} for FOMs that are 
#'    based on abnormal cases only, and \code{K} for FOMs that are based on normal and 
#'    abnormal cases.
#' 
#' 
#' @examples
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

  # account for 15+ FOMs  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
    # FOMs defined over NORMAL cases
    jkFomValues <- array(dim = c(I, J, K1))
    jkPseudoValues <- array(dim = c(I, J, K1))
  }  else if (FOM %in% c("MaxLLF", "HrSe")) { # after checking StOldCode.R, HrSe belongs in this group, depends only on abnormal cases
    # FOMs defined over ABNORMAL cases
    jkFomValues <- array(dim = c(I, J, K2))
    jkPseudoValues <- array(dim = c(I, J, K2))
  } else if (FOM %in% c("Wilcoxon", "HrAuc", "SongA1", 
                        "AFROC", "AFROC1", "wAFROC1", "wAFROC",
                        "MaxNLFAllCases", "ROI", "SongA2",
                        "PCL", "ALROC")) { # TBA may not handle ROI correctly
    # FOMs defined over ALL cases
    jkFomValues <- array(dim = c(I, J, K))
    jkPseudoValues <- array(dim = c(I, J, K))
  } else stop("Illegal FOM specified")
  
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
          # NOTATION
          # kIndxNor: case index for the 3rd dimension of normal cases, 
          # ranges from 1 to K1
          kIndxNor <- which(k1_ij_logi)[k];if (is.na(kIndxNor)) 
            stop("Indexing error in UtilPseudoValues")
          # FOMs defined over NORMAL cases
          nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
          llij_jk <- ll_ij;dim(llij_jk) <- c(K2_ij, maxLL)
          lV_j_jk <- perCase_ij
          lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
          lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
          if (is.na(jkFomValues[i, j, kIndxNor])) {
            jkFomValues[i, j, kIndxNor] <- 
              MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                       lID_j_jk, lW_j_jk, maxNL, maxLL, 
                       K1_ij - 1, K2_ij, FOM, FPFValue)
          } else stop("overwriting UtilPseudoValues")
          if (is.na(jkPseudoValues[i, j, kIndxNor])) {
            jkPseudoValues[i, j, kIndxNor] <- 
              fomArray[i, j] * K1_ij - jkFomValues[i, j, kIndxNor] * (K1_ij - 1)
          } else stop("overwriting UtilPseudoValues")
        }
      } else if (FOM %in% c("MaxLLF", "HrSe")) { 
        # FOMs defined over ABNORMAL cases
        for (k in 1:K2_ij) {
          # NOTATION
          # kIndxAbn: case index for the 3rd dimension of abormnal cases, 
          # ranges from 1 to K2
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
          # NOTATION
          # kIndxAll: case index for the 3rd dimension of all cases, 
          # ranges from 1 to K
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




