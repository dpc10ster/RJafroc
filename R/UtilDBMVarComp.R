#' Utility for Dorfman-Berbaum-Metz variance components
#' 
#' @param dataset The dataset object
#' @param FOM The figure of merit
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list containing the variance components.
#'   
#' 
#' @examples 
#' result <- UtilDBMVarComp(dataset02, FOM = "Wilcoxon")
#'
#'   
#' @export
#' 
UtilDBMVarComp <- function (dataset, FOM, FPFValue = 0.2)
{
  
  if (dataset$descriptions$design == "FCTRL") { 
    # factorial one-treatment dataset
    
    I <- dim(dataset$ratings$NL)[1]
    J <- dim(dataset$ratings$NL)[2]
    
    modalityID <- dataset$descriptions$modalityID
    readerID <- paste0("rdr", dataset$descriptions$readerID)
    
    psVals <- UtilPseudoValues(dataset, FOM, FPFValue)$jkPseudoValues
    K <- length(psVals[1,1,])
    
    msT <- 0
    for (i in 1:I) {
      msT <- msT + (mean(psVals[i, , ]) - mean(psVals))^2
    }
    msT <- msT * K * J/(I - 1)
    
    msR <- 0
    for (j in 1:J) {
      msR <- msR + (mean(psVals[, j, ]) - mean(psVals))^2
    }
    msR <- msR * K * I/(J - 1)
    
    msC <- 0
    for (k in 1:K) { 
      msC <- msC + (mean(psVals[, , k]) - mean(psVals))^2
    }
    msC <- msC * I * J/(K - 1)
    
    msTR <- 0
    for (i in 1:I) {
      for (j in 1:J) {
        msTR <- msTR + (mean(psVals[i, j, ]) - mean(psVals[i, , ]) - mean(psVals[, j, ]) + mean(psVals))^2
      }
    }
    msTR <- msTR * K/((I - 1) * (J - 1))
    
    msTC <- 0
    for (i in 1:I) {
      for (k in 1:K) {
        msTC <- msTC + (mean(psVals[i, , k]) - mean(psVals[i, , ]) - mean(psVals[, , k]) + mean(psVals))^2
      }
    }
    msTC <- msTC * J/((I - 1) * (K - 1))
    
    msRC <- 0
    for (j in 1:J) {
      for (k in 1:K) {
        msRC <- msRC + (mean(psVals[, j, k]) - mean(psVals[, j, ]) - mean(psVals[, , k]) + mean(psVals))^2
      }
    }
    msRC <- msRC * I/((J - 1) * (K - 1))
    
    msErr <- 0
    for (i in 1:I) {
      for (j in 1:J) {
        for (k in 1:K) {
          msErr <- msErr + 
            (psVals[i, j, k] - mean(psVals[i, j, ]) - mean(psVals[i, , k]) - mean(psVals[, j, k]) + 
               mean(psVals[i, , ]) + mean(psVals[, j, ]) + mean(psVals[, , k]) - mean(psVals))^2
        }
      }
    }
    msErr <- msErr/((I - 1) * (J - 1) * (K - 1))
    
    VarR <- (msR - msTR - msRC + msErr)/(I * K)
    VarC <- (msC - msTC - msRC + msErr)/(I * J)
    VarTR <- (msTR - msErr)/K
    VarTC <- (msTC - msErr)/J
    VarRC <- (msRC - msErr)/I
    VarErr <- msErr
    
    VarCom <- data.frame(Estimates = c(VarR, VarC, VarTR, VarTC, VarRC, VarErr), 
                         row.names = c("VarR", "VarC", "VarTR", "VarTC", "VarRC", "VarErr"), 
                         stringsAsFactors = FALSE)
    
    msArray <- c(msT, msR, msC, msTR, msTC, msRC, msErr)
    dfArray <- c(I - 1, 
                 J - 1, 
                 K - 1, 
                 (I - 1) * (J - 1), 
                 (I - 1) * (K - 1), 
                 (J - 1) * (K - 1), 
                 (I - 1) * (J - 1) * (K - 1))
    ssArray <- msArray * dfArray
    msArray <- c(msArray, NA)
    dfArray <- c(dfArray, sum(dfArray))
    ssArray <- c(ssArray, sum(ssArray))
    
    TRCanova <- data.frame("SS" = ssArray, 
                           "DF" = dfArray, 
                           "MS" = msArray,
                           row.names = c("T", "R", "C", "TR", "TC", "RC", "TRC", "Total"),
                           stringsAsFactors = FALSE)  
    
    IndividualTrt <- array(0, dim = c(I))
    msCIndividualTrt <- array(0, dim = c(I))
    msRCIndividualTrt <- array(0, dim = c(I))
    for (i in 1:I) {
      for (j in 1:J) {
        IndividualTrt[i] <- IndividualTrt[i] + (mean(psVals[i, j, ]) - mean(psVals[i, , ]))^2
      }
      IndividualTrt[i] <- IndividualTrt[i] * K/(J - 1)
      
      for (k in 1:K) {
        msCIndividualTrt[i] <- msCIndividualTrt[i] + (mean(psVals[i, , k]) - mean(psVals[i, , ]))^2
      }
      msCIndividualTrt[i] <- msCIndividualTrt[i] * J/(K - 1)
      
      for (j in 1:J) {
        for (k in 1:K) {
          msRCIndividualTrt[i] <- msRCIndividualTrt[i] + (mean(psVals[i, j, k]) - mean(psVals[i, j, ]) - mean(psVals[i, , k]) + mean(psVals[i, , ]))^2
        }
      }
      msRCIndividualTrt[i] <- msRCIndividualTrt[i]/((J - 1) * (K - 1))
    }
    dfArraySingle <- c(J - 1, K - 1, (J - 1) * (K - 1))
    msArraySingle <- t(cbind(IndividualTrt, msCIndividualTrt, msRCIndividualTrt))
    IndividualTrt <- data.frame(DF = dfArraySingle, 
                                row.names = c("msR", "msC", "msRC"), 
                                stringsAsFactors = FALSE)
    colNames <- "DF"
    for (i in 1:I) {
      IndividualTrt <- cbind(IndividualTrt, as.data.frame(msArraySingle[,i]))
      colNames <- c(colNames, paste0("trt", modalityID[i]))
    }
    colnames(IndividualTrt) <- colNames
    
    ssT <- array(0, dim = c(J))
    ssC <- array(0, dim = c(J))
    ssTC <- array(0, dim = c(J))
    for (j in 1:J) {
      for (i in 1:I) {
        ssT[j] <- ssT[j] + (mean(psVals[i, j, ]) - mean(psVals[, j, ]))^2
      }
      ssT[j] <- ssT[j] * K
      
      for (k in 1:K) {
        ssC[j] <- ssC[j] + (mean(psVals[, j, k]) - mean(psVals[, j, ]))^2
      }
      ssC[j] <- ssC[j] * I
      
      for (i in 1:I) {
        for (k in 1:K) {
          ssTC[j] <- ssTC[j] + (mean(psVals[i, j, k]) - mean(psVals[i, j, ]) - mean(psVals[, j, k]) + mean(psVals[, j, ]))^2
        }
      }
    }
    
    dfArray <- c(I - 1, K - 1, (I - 1) * (K - 1))
    ssArray <- t(cbind(ssT, ssC, ssTC))
    msArray <- ssArray/dfArray
    IndividualRdr <- data.frame(DF = dfArray, 
                                readerID = msArray, 
                                row.names = c("msT", "msC", "msTC"), 
                                stringsAsFactors = FALSE)
    colnames(IndividualRdr) <- c("DF", readerID)
    
    return(list(
      VarCom = VarCom,
      TRCanova = TRCanova,
      IndividualTrt = IndividualTrt,
      IndividualRdr = IndividualRdr
    ))
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    modalityID1 <- dataset$descriptions$modalityID1
    modalityID2 <- dataset$descriptions$modalityID2
    I1 <- length(modalityID1)
    I2 <- length(modalityID2)
    I <- c(I2, I1)
    J <- dim(dataset$ratings$NL)[3]
    modalityID <- list(modalityID2, modalityID1)
    readerID <- paste0("rdr", dataset$descriptions$readerID)
    
    psVals <- UtilPseudoValues(dataset, FOM, FPFValue)$jkPseudoValues
    K <- length(psVals[1,1,1,])
    PV <- ConvArr2List(dataset, psVals)

    VarCom  <- list()
    TRCanova  <- list()
    IndividualTrt  <- list()
    IndividualRdr  <- list()
    for (avgIndx in 1:2) { # treatment index that FOM was averaged over
      if (I[avgIndx] > 1) {     
        msT <- 0
        for (i in 1:I[avgIndx]) {
          msT <- msT + (mean(PV[[avgIndx]][i,,]) - mean(PV[[avgIndx]]))^2
        }
        msT <- msT * K * J/(I[avgIndx] - 1)
      } else msT <- NA
      
      if (J > 1) {
        msR <- 0
        for (j in 1:J) {
          msR <- msR + (mean(PV[[avgIndx]][,j,]) - mean(PV[[avgIndx]]))^2
        }
        msR <- I[avgIndx] * msR/(J - 1)
      } else msR <- NA
      
      msC <- 0
      for (k in 1:K) { 
        msC <- msC + (mean(PV[[avgIndx]][,,k]) - mean(PV[[avgIndx]]))^2
      }
      msC <- msC * I[avgIndx] * J/(K - 1)
      
      msTR <- 0
      for (i in 1:I[avgIndx]) {
        for (j in 1:J) {
          msTR <- msTR + 
            (mean(PV[[avgIndx]][i,j,]) - 
               mean(PV[[avgIndx]][i,,]) - 
               mean(PV[[avgIndx]][, j, ]) + 
               mean(PV[[avgIndx]]))^2
        }
      }
      msTR <- msTR * K/((I[avgIndx] - 1) * (J - 1))
      
      msTC <- 0
      for (i in 1:I[avgIndx]) {
        for (k in 1:K) {
          msTC <- msTC + 
            (mean(PV[[avgIndx]][i,,k]) - 
               mean(PV[[avgIndx]][i,,]) - 
               mean(PV[[avgIndx]][,,k]) + 
               mean(PV[[avgIndx]]))^2
        }
      }
      msTC <- msTC * J/((I[avgIndx] - 1) * (K - 1))
      
      msRC <- 0
      for (j in 1:J) {
        for (k in 1:K) {
          msRC <- msRC + 
            (mean(PV[[avgIndx]][,j,k]) - 
               mean(PV[[avgIndx]][,j,]) - 
               mean(PV[[avgIndx]][,,k]) + 
               mean(PV[[avgIndx]]))^2
        }
      }
      msRC <- msRC * I[avgIndx]/((J - 1) * (K - 1))
      
      msErr <- 0
      for (i in 1:I[avgIndx]) {
        for (j in 1:J) {
          for (k in 1:K) {
            msErr <- msErr + 
              (PV[[avgIndx]][i,j,k] - 
                 mean(PV[[avgIndx]][i,j,]) - 
                 mean(PV[[avgIndx]][i,,k]) - 
                 mean(PV[[avgIndx]][,j,k]) + 
                 mean(PV[[avgIndx]][i,,]) + 
                 mean(PV[[avgIndx]][,j,]) + 
                 mean(PV[[avgIndx]][,,k]) - 
                 mean(PV[[avgIndx]]))^2
          }
        }
      }
      msErr <- msErr/((I[avgIndx] - 1) * (J - 1) * (K - 1))
      
      VarR <- (msR - msTR - msRC + msErr)/(I[avgIndx] * K)
      VarC <- (msC - msTC - msRC + msErr)/(I[avgIndx] * J)
      VarTR <- (msTR - msErr)/K
      VarTC <- (msTC - msErr)/J
      VarRC <- (msRC - msErr)/I[avgIndx]
      VarErr <- msErr
      
      VarCom[[avgIndx]] <- data.frame(Estimates = c(VarR, VarC, VarTR, VarTC, VarRC, VarErr), 
                           row.names = c("VarR", "VarC", "VarTR", "VarTC", "VarRC", "VarErr"), 
                           stringsAsFactors = FALSE)
      
      msArray <- c(msT, msR, msC, msTR, msTC, msRC, msErr)
      dfArray <- c(I[avgIndx] - 1, 
                   J - 1, 
                   K - 1, 
                   (I[avgIndx] - 1) * (J - 1), 
                   (I[avgIndx] - 1) * (K - 1), 
                   (J - 1) * (K - 1), 
                   (I[avgIndx] - 1) * (J - 1) * (K - 1))
      ssArray <- msArray * dfArray
      msArray <- c(msArray, NA)
      dfArray <- c(dfArray, sum(dfArray))
      ssArray <- c(ssArray, sum(ssArray))
      
      TRCanova[[avgIndx]] <- data.frame("SS" = ssArray, 
                             "DF" = dfArray, 
                             "MS" = msArray,
                             row.names = c("T", "R", "C", "TR", "TC", "RC", "TRC", "Total"),
                             stringsAsFactors = FALSE)  
      
      indTrt <- array(0, dim = c(I[avgIndx]))
      msCindTrt <- array(0, dim = c(I[avgIndx]))
      msRCindTrt <- array(0, dim = c(I[avgIndx]))
      for (i in 1:I[avgIndx]) {
        for (j in 1:J) {
          indTrt[i] <- indTrt[i] + (mean(PV[[avgIndx]][i,j,]) - mean(PV[[avgIndx]][i,,]))^2
        }
        indTrt[i] <- indTrt[i] * K/(J - 1)
        
        for (k in 1:K) {
          msCindTrt[i] <- msCindTrt[i] + (mean(PV[[avgIndx]][i,,k]) - mean(PV[[avgIndx]][i,,]))^2
        }
        msCindTrt[i] <- msCindTrt[i] * J/(K - 1)
        
        for (j in 1:J) {
          for (k in 1:K) {
            msRCindTrt[i] <- msRCindTrt[i] + 
              (mean(PV[[avgIndx]][i,j,k]) - 
                 mean(PV[[avgIndx]][i,j,]) - 
                 mean(PV[[avgIndx]][i,,k]) + 
                 mean(PV[[avgIndx]][i,,]))^2
          }
        }
        msRCindTrt[i] <- msRCindTrt[i]/((J - 1) * (K - 1))
      }
      dfArraySingle <- c(J - 1, K - 1, (J - 1) * (K - 1))
      msArraySingle <- t(cbind(indTrt, msCindTrt, msRCindTrt))
      indTrt <- data.frame(DF = dfArraySingle, 
                           row.names = c("msR", "msC", "msRC"), 
                           stringsAsFactors = FALSE)
      colNames <- "DF"
      for (i in 1:I[avgIndx]) {
        indTrt <- cbind(indTrt, as.data.frame(msArraySingle[,i]))
        colNames <- c(colNames, paste0("trt", modalityID[[avgIndx]][i]))
      }
      colnames(indTrt) <- colNames
      IndividualTrt[[avgIndx]] <- indTrt
      
      ssT <- array(0, dim = c(J))
      ssC <- array(0, dim = c(J))
      ssTC <- array(0, dim = c(J))
      for (j in 1:J) {
        for (i in 1:I[avgIndx]) {
          ssT[j] <- ssT[j] + (mean(PV[[avgIndx]][i,j,]) - mean(PV[[avgIndx]][,j,]))^2
        }
        ssT[j] <- ssT[j] * K
        
        for (k in 1:K) {
          ssC[j] <- ssC[j] + (mean(PV[[avgIndx]][,j,k]) - mean(PV[[avgIndx]][,j,]))^2
        }
        ssC[j] <- ssC[j] * I[avgIndx]
        
        for (i in 1:I[avgIndx]) {
          for (k in 1:K) {
            ssTC[j] <- ssTC[j] + 
              (mean(PV[[avgIndx]][i,j,k]) - 
                 mean(PV[[avgIndx]][i,j,]) - 
                 mean(PV[[avgIndx]][,j,k]) + 
                 mean(PV[[avgIndx]][,j,]))^2
          }
        }
      }
      
      dfArray <- c(I[avgIndx] - 1, K - 1, (I[avgIndx] - 1) * (K - 1))
      ssArray <- t(cbind(ssT, ssC, ssTC))
      msArray <- ssArray/dfArray
      IndividualRdr[[avgIndx]] <- data.frame(DF = dfArray, 
                                  readerID = msArray, 
                                  row.names = c("msT", "msC", "msTC"), 
                                  stringsAsFactors = FALSE)
      colnames(IndividualRdr[[avgIndx]]) <- c("DF", readerID)
    }
    names(VarCom) <- c("AvgMod1", "AvgMod2")
    names(TRCanova) <- c("AvgMod1", "AvgMod2")
    names(IndividualTrt) <- c("AvgMod1", "AvgMod2")
    names(IndividualRdr) <- c("AvgMod1", "AvgMod2")
    
    return(list(
      VarCom = VarCom,
      TRCanova = TRCanova,
      IndividualTrt = IndividualTrt,
      IndividualRdr = IndividualRdr
    ))
  }
  
} 
