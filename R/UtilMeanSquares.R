#' Calculate mean squares
#' 
#' Calculates the mean squares used in the DBMH and ORH methods
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}.
#' @param FOM The figure of merit to be used in the calculation. The default 
#'    is \code{"FOM_wAFROC"}. See \code{\link{UtilFigureOfMerit}}.
#' @param method The method, in which the mean squares are calculated. The two 
#'    valid options are \code{"DBMH"} (default) and \code{"ORH"}. 
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list containing all possible mean squares
#' 
#' @details 
#' For \code{DBMH} method, \code{msT, msTR, msTC, msTRC} will not be available 
#'    if the dataset contains only one treatment. Similarly, 
#'    \code{msR, msTR, msRC, msTRC} will not be returned for single reader dataset. 
#'    For \code{ORH} method, \code{msT, msR, msTR} will be returned for multiple 
#'    reader multiple treatment dataset. \code{msT} is not available for single 
#'    treatment dataset, and \code{msR} is not available for single reader dataset.
#' 
#' @examples
#' UtilMeanSquares(dataset02, FOM = "Wilcoxon")
#' UtilMeanSquares(dataset05, FOM = "wAFROC", method = "ORH")
#' 
#' @export

UtilMeanSquares <- function(dataset, FOM = "Wilcoxon", FPFValue = 0.2, method = "DBMH"){
  dataType <- dataset$dataType
  if (dataType != "LROC") {
    NL <- dataset$NL
    LL <- dataset$LL
  } else {
    if (FOM == "Wilcoxon"){
      datasetRoc <- DfLroc2Roc(dataset)
      NL <- datasetRoc$NL
      LL <- datasetRoc$LL
    } else if (FOM %in% c("PCL", "ALROC")){
      NL <- dataset$NL
      LL <- dataset$LLCl
    } else stop("incorrect FOM for LROC data")
  }
  # lesionVector <- dataset$lesionVector
  # lesionID <- dataset$lesionID
  # lesionWeight <- dataset$lesionWeight
  # maxNL <- dim(NL)[4]
  # maxLL <- dim(LL)[4]
  # dataType <- dataset$dataType
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  if (method == "DBMH") {
    pseudoValues <- UtilPseudoValues(dataset, FOM, FPFValue)$jkPseudoValues
    #
    # extensive changes made here DPC 6/30/19 for DBMH method
    # basically redefine K as number of diseased cases or number of non-diseased
    # case, or all cases, depending on the FOM
    # these changes affect FOMs that do NOT involve all cases
    # No changes are needed for ORH method
    #
    if (FOM %in% c("MaxLLF", "HrSe")) {
      Ktemp <- K2 # K should be # of diseased cases
    } else if (FOM %in% c("MaxNLF", "HrSp", "MaxNLFAllCases", "ExpTrnsfmSp")) {
      Ktemp <- K1 # K should be # of non-diseased cases
    } else {
      Ktemp <- K # K should be # of all cases
    }
    # end changes
    
    if (I != 1 ) {
      msT <- 0
      for (i in 1:I) {
        msT <- msT + (mean(pseudoValues[i, , ]) - mean(pseudoValues))^2
      }
      msT <- msT * Ktemp * J/(I - 1)
      
      msTC <- 0
      for (i in 1:I) {
        for (k in 1:Ktemp) { 
          msTC <- msTC + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
        }
        msTC <- msTC * J/((I - 1) * (Ktemp - 1)) # Error noted by Erin Greco; minus one was missing 
        
        msCSingleT <- rep(0, I)
        for (i in 1:I){
          for (k in 1:Ktemp) {
            msCSingleT[i] <- msCSingleT[i] + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]))^2
          }
          msCSingleT[i] <- msCSingleT[i] * J/(Ktemp - 1)
        }
      } 
    }
    
    if (J != 1){
      msR <- 0
      for (j in 1:J) {
        msR <- msR + (mean(pseudoValues[, j, ]) - mean(pseudoValues))^2
      }
      msR <- msR * Ktemp * I/(J - 1)
      
      msRC <- 0
      for (j in 1:J) {
        for (k in 1:Ktemp) {
          msRC <- msRC + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
        }
      }
      msRC <- msRC * I/((J - 1) * (Ktemp - 1))
      
      msCSingleR <- rep(0, J)
      for (j in 1:J){
        for (k in 1:Ktemp) {
          msCSingleR[j] <- msCSingleR[j] + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]))^2
        }
        msCSingleR[j] <- msCSingleR[j] * I/(Ktemp - 1)
      }
    }
    
    msC <- 0
    for (k in 1:Ktemp) {
      msC <- msC + (mean(pseudoValues[, , k]) - mean(pseudoValues))^2
    }
    msC <- msC * I * J/(Ktemp - 1)
    
    if (I != 1 && J != 1){
      msTR <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          msTR <- msTR + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, j, ]) + mean(pseudoValues))^2
        }
      }
      msTR <- msTR * Ktemp/((I - 1) * (J - 1))
      
      msTRC <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:Ktemp) {
            msTRC <- msTRC + (pseudoValues[i, j, k] - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) - mean(pseudoValues[, j, k]) + 
                                mean(pseudoValues[i, , ]) + mean(pseudoValues[, j, ]) + mean(pseudoValues[, , k]) - mean(pseudoValues))^2
          }
        }
      }
      msTRC <- msTRC/((I - 1) * (J - 1) * (Ktemp - 1))
    }
    
    if (I == 1 && J == 1) {
      return(list(
        msC = msC
      ))
    } else if (I == 1) {
      return(list(
        msR = msR,
        msC = msC,
        msRC = msRC,
        msCSingleR = msCSingleR
      ))
    } else if (J == 1) {
      return(list(
        msT = msT,
        msC = msC,
        msTC = msTC, 
        msCSingleT = msCSingleT
      ))
    } else {
      return(list(
        msT = msT,
        msR = msR,
        msC = msC,
        msTR = msTR,
        msTC = msTC,
        msRC = msRC,
        msTRC = msTRC, 
        msCSingleT = msCSingleT,
        msCSingleR = msCSingleR
      ))
    }
  } else if (method == "ORH"){
    
    if (I == 1 && J == 1){
      errMsg <- "The mean squares cannot be calculated for single reader single treatment dataset."
      stop(errMsg)
    }
    
    fomArray <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    fomMean <- mean(fomArray)
    
    if (I != 1){
      msT <- 0
      for (i in 1:I) {
        msT <- msT + (mean(fomArray[i, ]) - fomMean)^2
      }
      msT <- J * msT/(I - 1)
    }
    
    if (J != 1) {
      msR <- 0
      for (j in 1:J) {
        msR <- msR + (mean(fomArray[, j]) - fomMean)^2
      }
      msR <- I * msR/(J - 1)
    }
    
    if (I != 1 && J != 1){
      msTR <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          msTR <- msTR + (fomArray[i, j] - mean(fomArray[i, ]) - mean(fomArray[, j]) + fomMean)^2
        }
      }
      msTR <- msTR/((J - 1) * (I - 1))
      return(list(
        msT = msT,
        msR = msR,
        msTR = msTR
      ))
    } else if (I == 1) {
      return(list(
        msR = msR
      ))
    }else if (J == 1) {
      return(list(
        msT = msT
      ))
    }
    
  } else {
    errMsg <- sprintf("%s is not a valid method; must use 'DBMH' or 'ORH'", method)
    stop(errMsg)
  }
  
}

