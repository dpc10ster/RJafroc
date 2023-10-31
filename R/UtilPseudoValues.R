#' Pseudovalues for given factorial or crossed modality dataset and FOM
#' 
#' Returns \strong{centered} jackknife pseudovalues AND jackknife FOM values, 
#'    for factorial study designs
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}.
#'    
#' @param FOM The figure of merit to be used. The default is \code{"FOM_wAFROC"}. 
#'     See \code{\link{UtilFigureOfMerit}}.
#'    
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' 
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
#' UtilPseudoValues(datasetXModality, FOM = "wAFROC")
#' 
#' @export

UtilPseudoValues <- function(dataset, FOM, FPFValue = 0.2) {
  
  isValidDataset(dataset, FOM)
  
  if (dataset$descriptions$design == "FCTRL") { 
    # factorial one-treatment dataset 
    
    if (all(is.na(dataset$descriptions$truthTableStr))){
      dataset$descriptions$truthTableStr <- AddTruthTableStr(dataset, 
                                                             dataset$descriptions$type, 
                                                             dataset$lesions$perCase) # added 9/16/2023
    }
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
    if (FOM %in% c("MaxNLF", "HrSp")) {
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
    foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
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
        nl_i1i2j <- NL[i, j, k_ij_logi, 1:maxNL]; dim(nl_i1i2j) <- c(K_ij, maxNL)
        # i.e., NL ratings for all cases meeting the i,j criteria
        ll_i1i2j <- LL[i, j, k2_ij_logi, 1:maxLL]; dim(ll_i1i2j) <- c(K2_ij, maxLL)
        # i.e., LL ratings for all cases meeting the i,j criteria
        
        if (FOM %in% c("MaxNLF", "HrSp")) {
          for (k in 1:K1_ij) {
            # NOTATION
            # kIndxNor: case index for the 3rd dimension of normal cases, 
            # ranges from 1 to K1
            kIndxNor <- which(k1_ij_logi)[k];if (is.na(kIndxNor)) 
              stop("Indexing error kIndxNor in UtilPseudoValues")
            # FOMs defined over NORMAL cases
            nlij_jk <- nl_i1i2j[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_i1i2j;dim(llij_jk) <- c(K2_ij, maxLL)
            lV_j_jk <- perCase_ij
            lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
            lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
            if (is.na(jkFomValues[i, j, kIndxNor])) {
              jkFomValues[i, j, kIndxNor] <- 
                MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                         lID_j_jk, lW_j_jk, maxNL, maxLL, 
                         K1_ij - 1, K2_ij, FOM, FPFValue)
            } else stop("overwriting jkFomValues[i, j, kIndxNor] in UtilPseudoValues")
            if (is.na(jkPseudoValues[i, j, kIndxNor])) {
              jkPseudoValues[i, j, kIndxNor] <- 
                foms[i, j] * K1_ij - jkFomValues[i, j, kIndxNor] * (K1_ij - 1)
            } else stop("overwriting jkPseudoValues[i, j, kIndxNor] in UtilPseudoValues")
          }
        } else if (FOM %in% c("MaxLLF", "HrSe")) { 
          # FOMs defined over ABNORMAL cases
          for (k in 1:K2_ij) {
            # NOTATION
            # kIndxAbn: case index for the 3rd dimension of abormnal cases, 
            # ranges from 1 to K2
            kIndxAbn <- which(k2_ij_logi)[k];if (is.na(kIndxAbn)) 
              stop("Indexing error kIndxAbn in UtilPseudoValues")
            nlij_jk <- nl_i1i2j[-(k+K1_ij), ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_i1i2j[-k, ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
            lV_j_jk <- perCase_ij[-k]
            lW_j_jk <- lW_ij[-k, ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
            lID_j_jk <- lID_ij[-k, ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
            if (is.na(jkFomValues[i, j, kIndxAbn])) {
              jkFomValues[i, j, kIndxAbn] <- 
                MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                         lID_j_jk, lW_j_jk, maxNL, maxLL, 
                         K1_ij, K2_ij - 1, FOM, FPFValue)
            } else stop("overwriting jkFomValues[i, j, kIndxAbn] in UtilPseudoValues 3")
            if (is.na(jkPseudoValues[i, j, kIndxAbn])) {
              jkPseudoValues[i, j, kIndxAbn] <- 
                foms[i, j] * K2_ij - jkFomValues[i, j, kIndxAbn] * (K2_ij - 1)
            } else stop("overwriting jkFomValues[i, j, kIndxAbn] in UtilPseudoValues")
          }
        } else { 
          # FOMs defined over ALL cases
          for (k in 1:K_ij) {
            # NOTATION
            # kIndxAll: case index for the 3rd dimension of all cases, 
            # ranges from 1 to K
            kIndxAll <- which(k_ij_logi)[k];if (is.na(kIndxAll)) 
              stop("Indexing error kIndxAll in UtilPseudoValues")
            if (k <= K1_ij) {
              nlij_jk <- nl_i1i2j[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
              llij_jk <- ll_i1i2j;dim(llij_jk) <- c(K2_ij, maxLL)
              lV_j_jk <- perCase_ij
              lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
              lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
              if (is.na(jkFomValues[i, j, kIndxAll])) {
                jkFomValues[i, j, kIndxAll] <- 
                  MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                           lID_j_jk, lW_j_jk, maxNL, maxLL, 
                           K1_ij - 1, K2_ij, FOM, FPFValue)
              } else stop("overwriting jkFomValues[i, j, kIndxAll] in UtilPseudoValues")
              if (is.na(jkPseudoValues[i, j, kIndxAll])) {
                jkPseudoValues[i, j, kIndxAll] <- 
                  foms[i, j] * K_ij - jkFomValues[i, j, kIndxAll] * (K_ij - 1)
              } else stop("overwriting jkFomValues[i, j, kIndxAll] in UtilPseudoValues")
            } else {
              nlij_jk <- nl_i1i2j[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
              llij_jk <- ll_i1i2j[-(k - K1_ij), ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
              lV_j_jk <- perCase_ij[-(k - K1_ij)]
              lW_j_jk <- lW_ij[-(k - K1_ij), ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
              lID_j_jk <- lID_ij[-(k - K1_ij), ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
              if (is.na(jkFomValues[i, j, kIndxAll])) {
                jkFomValues[i, j, kIndxAll] <- 
                  MyFom_ij(nlij_jk, llij_jk, lV_j_jk, 
                           lID_j_jk, lW_j_jk, maxNL, maxLL, 
                           K1_ij, K2_ij - 1, FOM, FPFValue)
              } else stop("overwriting jkFomValues[i, j, kIndxAll] in UtilPseudoValues")
              if (is.na(jkPseudoValues[i, j, kIndxAll])) {
                jkPseudoValues[i, j, kIndxAll] <- 
                  foms[i, j] * K_ij - jkFomValues[i, j, kIndxAll] * (K_ij - 1)
              } else stop("overwriting jkFomValues[i, j, kIndxAll] in UtilPseudoValues")
            }
          }
        }
        # center the pseudovalues so that mean (jkPseudoValues) == mean(jkFomValues)
        if (FOM %in% c("MaxNLF", "HrSp")) {
          # FOMs defined over NORMAL cases
          jkPseudoValues[i, j, which(k1_ij_logi)] <- 
            jkPseudoValues[i, j, which(k1_ij_logi)] + 
            (foms[i, j] - mean(jkPseudoValues[i, j, which(k1_ij_logi)]))
        }  else if (FOM %in% c("MaxLLF", "HrSe")) {
          # FOMs defined over ABNORMAL cases
          jkPseudoValues[i, j, which(k2_ij_logi)] <- 
            jkPseudoValues[i, j, which(k2_ij_logi)] + 
            (foms[i, j] - mean(jkPseudoValues[i, j, which(k2_ij_logi)]))
        } else {
          # FOMs defined over ALL cases
          jkPseudoValues[i, j, which(k_ij_logi)] <- 
            jkPseudoValues[i, j, which(k_ij_logi)] + 
            (foms[i, j] - mean(jkPseudoValues[i, j, which(k_ij_logi)]))
        }
      }
    }
  } else { 
    # cross-modality factorial dataset, two treatment factors
    # took out truthTableStr, too complicated
    # took out LROC and ROI
    # took out normal case only FOMs
    # took out abnormal case only FOMs
    # only allow "Wilcoxon", "AFROC", "AFROC1", "wAFROC1", "wAFROC" FOMs
    
    I1 <- dim(dataset$ratings$NL)[1]
    I2 <- dim(dataset$ratings$NL)[2]
    J <- dim(dataset$ratings$NL)[3]
    K <- dim(dataset$ratings$NL)[4]
    K2 <- dim(dataset$ratings$LL)[4]
    K1 <- K - K2
    maxNL <- dim(dataset$ratings$NL)[5]
    maxLL <- dim(dataset$ratings$LL)[5]
    perCase <- dataset$lesions$perCase 
    
    dataType <- dataset$descriptions$type
    NL <- dataset$ratings$NL
    LL <- dataset$ratings$LL
    
    jkFom <- list()
    jkPseudo <- list()
    
    foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    # don't need the averaged FOMs here
    # foms <- ConvArr2List(dataset, fomsTemp)[[2]] # the raw FOMs, before averaging
    
    jkFomValues <- array(dim = c(I1, I2, J, K))
    jkPseudoValues <- array(dim = c(I1, I2, J, K))
    
    for (i1 in 1:I1) { # first modality
      for (i2 in 1:I2) { # second modality
        for (j in 1:J) {
          lID <- dataset$lesions$IDs[,1:maxLL, drop = FALSE]
          lW <- dataset$lesions$weights[,1:maxLL, drop = FALSE]
          nl_i1i2j <- NL[i1, i2, j, , 1:maxNL]; dim(nl_i1i2j) <- c(K, maxNL)
          ll_i1i2j <- LL[i1, i2, j, , 1:maxLL]; dim(ll_i1i2j) <- c(K2, maxLL)
          for (k in 1:K) {
            if (k <= K1) {
              nl_i1i2j_jk <- nl_i1i2j[-k, ];dim(nl_i1i2j_jk) <- c(K - 1, maxNL)
              ll_i1i2j_jk <- ll_i1i2j;dim(ll_i1i2j_jk) <- c(K2, maxLL)
              lV_i1i2j_jk <- dataset$lesions$perCase
              lID_i1i2j_jk <- lID;dim(lID_i1i2j_jk) <- c(K2, maxLL)
              lW_i1i2j_jk <- lW;dim(lW_i1i2j_jk) <- c(K2, maxLL)
              if (is.na(jkFomValues[i1, i2, j, k])) {
                jkFomValues[i1, i2, j, k] <- 
                  MyFom_ij(nl_i1i2j_jk, ll_i1i2j_jk, lV_i1i2j_jk, 
                           lID_i1i2j_jk, lW_i1i2j_jk, maxNL, maxLL, 
                           K1 - 1, K2, FOM, FPFValue)
              } else stop("overwriting jkFomValues[i1, i2, j, k] in UtilPseudoValues")
              if (is.na(jkPseudoValues[i1, i2, j, k])) {
                jkPseudoValues[i1, i2, j, k] <- 
                  foms[i1, i2, j] * K - jkFomValues[i1, i2, j, k] * (K - 1)
              } else stop("overwriting jkFomValues[i1, i2, j, k] in UtilPseudoValues")
            } else {
              nl_i1i2j_jk <- nl_i1i2j[-k, ];dim(nl_i1i2j_jk) <- c(K - 1, maxNL)
              ll_i1i2j_jk <- ll_i1i2j[-(k - K1), ];dim(ll_i1i2j_jk) <- c(K2 - 1, maxLL)
              lV_i1i2j_jk <- perCase[-(k - K1)]
              lW_i1i2j_jk <- lW[-(k - K1), ];dim(lW_i1i2j_jk) <- c(K2 - 1, maxLL)
              lID_i1i2j_jk <- lID[-(k - K1), ];dim(lID_i1i2j_jk) <- c(K2 - 1, maxLL)
              if (is.na(jkFomValues[i1, i2, j, k])) {
                jkFomValues[i1, i2, j, k] <- 
                  MyFom_ij(nl_i1i2j_jk, ll_i1i2j_jk, lV_i1i2j_jk, 
                           lID_i1i2j_jk, lW_i1i2j_jk, maxNL, maxLL, 
                           K1, K2 - 1, FOM, FPFValue)
              } else stop("overwriting jkFomValues[i1, i2, j, k] UtilPseudoValues")
              if (is.na(jkPseudoValues[i1, i2, j, k])) {
                jkPseudoValues[i1, i2, j, k] <- 
                  foms[i1, i2, j] * K - jkFomValues[i1, i2, j, k] * (K - 1)
              } else stop("overwriting jkFomValues[i1, i2, j, k] UtilPseudoValues")
            }
          }
        }
        # center the pseudovalues so that mean (jkPseudoValues) == mean(jkFomValues)
        jkPseudoValues[i1, i2, j, k] <- 
          jkPseudoValues[i1, i2, j, k] + 
          (foms[i1, i2, j] - mean(jkPseudoValues[i1, i2, j, ]))
      }
      #    }
    }
  }
  return(list(
    jkPseudoValues = jkPseudoValues, 
    jkFomValues = jkFomValues
  ))
  
  # NOTE 
  # mean(jkPseudoValues) == mean(jkFomValues)
  # Browse[2]> mean(jkPseudoValues)
  # [1] 0.8680476
  # Browse[2]> mean(jkFomValues)
  # [1] 0.8680476
  # 
  
}

