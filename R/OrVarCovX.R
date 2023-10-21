OrVarCovX <- function (dsX, FOM, FPFValue, covEstMethod, nBoots, seed)
{  
  I1 <- dim(dsX$ratings$NL)[1]
  I2 <- dim(dsX$ratings$NL)[2]
  J <- dim(dsX$ratings$NL)[3]
  
  fomsTemp <- UtilFigureOfMerit(dsX, FOM, FPFValue)
  foms <- FomAvgXModality(dsX, fomsTemp)
  fomMean <- mean(foms[[1]]) # same as mean(foms[[2]])
  
  I <- c(I2,I1) # average over 1st modality has number of levels equal to that of 2nd modality
  
  msT <- array(0, dim = 2)
  msR <- array(0, dim = 2)
  msTR <- array(0, dim = 2)
  msArray <- list()
  dfArray <- list()
  ssArray <- list()
  TRanova <- list()
  msR_i <- list()
  cov2EachTrt <- list()
  varEachTrt <- list()
  
  for (avgIndx in 1:2) { # treatment index that FOM was averaged over
    if (I[avgIndx] > 1) {
      for (ii in 1:I[avgIndx]) {
        msT[avgIndx] <- msT[avgIndx] + (mean(foms[[avgIndx]][ii,]) - fomMean)^2
      }
      msT[avgIndx] <- J * msT[avgIndx]/(I[avgIndx] - 1)
    } else msT[avgIndx] <- NA
    
    if (J > 1) {
      for (j in 1:J) {
        msR[avgIndx] <- msR[avgIndx] + (mean(foms[[avgIndx]][, j]) - fomMean)^2
      }
      msR[avgIndx] <- I[avgIndx] * msR[avgIndx]/(J - 1)
    } else msR[avgIndx] <- NA
    
    if ((I[avgIndx] > 1) && (J > 1)) {
      for (ii in 1:I[avgIndx]) {
        for (j in 1:J) {
          msTR[avgIndx] <- msTR[avgIndx] + (foms[[avgIndx]][ii, j] - mean(foms[[avgIndx]][ii, ]) - mean(foms[[avgIndx]][, j]) + fomMean)^2
        }
      }
      msTR[avgIndx] <- msTR[avgIndx]/((J - 1) * (I[avgIndx] - 1))
    } else msTR[avgIndx] <- NA
    
    msArray[[avgIndx]] <- c(msT[avgIndx], msR[avgIndx], msTR[avgIndx])
    dfArray[[avgIndx]] <- c(I[avgIndx] - 1, J - 1, (I[avgIndx] - 1) * (J - 1))
    ssArray[[avgIndx]] <- msArray[[avgIndx]] * dfArray[[avgIndx]]
    
    TRanova[[avgIndx]] <- data.frame("SS" = ssArray[[avgIndx]], 
                               "DF" = dfArray[[avgIndx]], 
                               "MS" = msArray[[avgIndx]],
                               stringsAsFactors = FALSE)  
    rownames(TRanova[[avgIndx]]) <- c("T", "R", "TR")
    
    # single modality msR_i ############################################################
    if (J > 1) {
      msR_i[[avgIndx]] <- array(0, dim = I[avgIndx])
      for (ii in 1:I[avgIndx]) {
        for (j in 1:J) {
          msR_i[[avgIndx]][ii] <- msR_i[[avgIndx]][ii] + (foms[[avgIndx]][ii, j] -  mean(foms[[avgIndx]][ii,]))^2
        }
      }
      msR_i[[avgIndx]][avgIndx] <- msR_i[[avgIndx]][avgIndx]/(J - 1)
    } else msR_i[[avgIndx]][ii] <- NA
    
    # ???DPC???
    # following call gets all the needed variance and covariance components
    # VarCovALL <- OrVarCov(dsX, FOM, FPFValue, nBoots, covEstMethod, seed)
    
  }
  
}
