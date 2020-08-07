#' Calculate mean squares for factorial dataset
#' 
#' Calculates the mean squares used in the DBM and ORH methods for factorial dataset
#' 
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}.
#' @param FOM The figure of merit to be used in the calculation. The default 
#'    is \code{"FOM_wAFROC"}. See \code{\link{UtilFigureOfMerit}}.
#' @param method The method, in which the mean squares are calculated. The two 
#'    valid choices are \code{"DBM"} (default) and \code{"OR"}. 
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return A list containing all possible mean squares
#' 
#' @details 
#' For \code{DBM} method, \code{msT, msTR, msTC, msTRC} will not be available 
#'    if the dataset contains only one treatment. Similarly, 
#'    \code{msR, msTR, msRC, msTRC} will not be returned for single reader dataset. 
#'    For \code{ORH} method, \code{msT, msR, msTR} will be returned for multiple 
#'    reader multiple treatment dataset. \code{msT} is not available for single 
#'    treatment dataset, and \code{msR} is not available for single reader dataset.
#' 
#' @examples
#' UtilMeanSquares(dataset02, FOM = "Wilcoxon")
#' UtilMeanSquares(dataset05, FOM = "wAFROC", method = "OR")
#' 
#' @export

UtilMeanSquares <- function(dataset, FOM = "Wilcoxon", FPFValue = 0.2, method = "DBM"){

  if (dataset$descriptions$design != "FCTRL") stop("This function requires a FCTRL dataset")
  
  if ((dataset$descriptions$type == "ROC") && (FOM != "Wilcoxon")) stop("ROC dataset requires Wilcoxon FOM") 
  if ((dataset$descriptions$type == "FROC") && (FOM == "Wilcoxon")) stop("FROC dataset cannot have Wilcoxon FOM") 
  
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
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)

  if (method == "DBM") {
    pseudoValues <- UtilPseudoValues(dataset, FOM, FPFValue)$jkPseudoValues
    # 8/7/20: the single line code at end of this comment block corrects an 
    # error affecting FOM = MaxNLFAllCases.
    # The C++-code uses all cases, so K should be K1 + K2; the previous code was 
    # using K1 while the pseudovalue function was jackknifing all cases.
    # Discovered this error while running test_UtilMeanSquares for 
    # contextStr <- "UtilMeanSquares", d = 2, f = 7 and m = 1
    # The new code picks up proper value for K from dim(pseudoValues)[3]
    # K is the number of diseased cases or number of non-diseased
    # case, or all cases, depending on the FOM.
    # These changes only affect FOMs that do NOT involve all cases.
    # No changes are needed for OR method.
    # The previous bad code follows (moved to RJafrocMaintenance/MovedFromRJafroc):
    # if (FOM %in% c("MaxLLF", "HrSe")) {
    #   Ktemp <- K2 # K should be # of diseased cases
    # } else if (FOM %in% c("MaxNLF", "HrSp", "MaxNLFAllCases", "ExpTrnsfmSp")) {
    #   Ktemp <- K1 # K should be # of non-diseased casesd
    # } else {
    #   Ktemp <- K # K should be # of all cases
    # }
    # New code follows:
    K <- dim(pseudoValues)[3]

    if (I != 1 ) {
      msT <- 0
      for (i in 1:I) {
        msT <- msT + (mean(pseudoValues[i, , ]) - mean(pseudoValues))^2
      }
      msT <- msT * K * J/(I - 1)
      
      msTC <- 0
      for (i in 1:I) {
        for (k in 1:K) { 
          msTC <- msTC + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
        }
      } # the for loop should end here
      msTC <- msTC * J/((I - 1) * (K - 1)) # Error noted by Erin Greco; minus one was missing 
      # found another error in msTC while doing RJafrocBook 4/17/20
      # was being cute by putting end of for loop further down; messes up division at line above
      # separated the two loops as shown above and below
      msCSingleT <- rep(0, I)
      for (i in 1:I){ # separated loop here
        for (k in 1:K) {
          msCSingleT[i] <- msCSingleT[i] + (mean(pseudoValues[i, , k]) - mean(pseudoValues[i, , ]))^2
        }
        msCSingleT[i] <- msCSingleT[i] * J/(K - 1)
      }
    }
    
    if (J != 1){
      msR <- 0
      for (j in 1:J) {
        msR <- msR + (mean(pseudoValues[, j, ]) - mean(pseudoValues))^2
      }
      msR <- msR * K * I/(J - 1)
      
      msRC <- 0
      for (j in 1:J) {
        for (k in 1:K) {
          msRC <- msRC + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]) - mean(pseudoValues[, , k]) + mean(pseudoValues))^2
        }
      }
      msRC <- msRC * I/((J - 1) * (K - 1))
      
      msCSingleR <- rep(0, J)
      for (j in 1:J){
        for (k in 1:K) {
          msCSingleR[j] <- msCSingleR[j] + (mean(pseudoValues[, j, k]) - mean(pseudoValues[, j, ]))^2
        }
        msCSingleR[j] <- msCSingleR[j] * I/(K - 1)
      }
    }
    
    msC <- 0
    for (k in 1:K) {
      msC <- msC + (mean(pseudoValues[, , k]) - mean(pseudoValues))^2
    }
    msC <- msC * I * J/(K - 1)
    
    if (I != 1 && J != 1){
      msTR <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          msTR <- msTR + (mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , ]) - mean(pseudoValues[, j, ]) + mean(pseudoValues))^2
        }
      }
      msTR <- msTR * K/((I - 1) * (J - 1))
      
      msTRC <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          for (k in 1:K) {
            msTRC <- msTRC + (pseudoValues[i, j, k] - mean(pseudoValues[i, j, ]) - mean(pseudoValues[i, , k]) - mean(pseudoValues[, j, k]) + 
                                mean(pseudoValues[i, , ]) + mean(pseudoValues[, j, ]) + mean(pseudoValues[, , k]) - mean(pseudoValues))^2
          }
        }
      }
      msTRC <- msTRC/((I - 1) * (J - 1) * (K - 1))
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
  } else if (method == "OR"){
    
    if (I == 1 && J == 1){
      errMsg <- "The mean squares cannot be calculated for single reader single treatment dataset."
      stop(errMsg)
    }
    
    # `as.matrix` is absolutely necessary if following `mean()` function is to work
    fomArray <- as.matrix(UtilFigureOfMerit(dataset, FOM, FPFValue))
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
    errMsg <- sprintf("%s is not a valid method; must use 'DBM' or 'ORH'", method)
    stop(errMsg)
  }
  
}

