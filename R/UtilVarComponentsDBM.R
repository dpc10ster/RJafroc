#' Utility for Dorfman-Berbaum-Metz variance components
#' 
#' @param dataset The dataset object
#' @param FOM The figure of merit
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list object containing the variance components.
#'   
#' 
#' @examples 
#' UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")
#'
#'   
#' @export
#' 
UtilVarComponentsDBM <- function (dataset, FOM, FPFValue = 0.2)
{
  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
# K <- dim(dataset$ratings$NL)[3] # this does not generalize to all FOMs
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  
  psVals <- UtilPseudoValues(dataset, FOM, FPFValue)$jkPseudoValues
  # the k-dimension of psVals can change
  # depending on choice of FOM; e.g., for maxLLF, it is K2
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
    colNames <- c(colNames, paste0("Trt", modalityID[i]))
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
  colnames(IndividualRdr) <- c("DF", paste0("rdr", readerID))
  
  return(list(
    VarCom = VarCom,
    TRCanova = TRCanova,
    IndividualTrt = IndividualTrt,
    IndividualRdr = IndividualRdr
  ))
}
