OrFinalOutputX <- function (foms, VarCovALL, modalityID, readerID)
{
  
  if (!is.list(foms)) stop("foms has to be a list variable.\n")
  if (!is.list(VarCovALL)) stop("VarCovALL has to be a list variable.\n")
  if (!is.list(modalityID)) stop("modalityID has to be a list variable.\n")
  
  I1 <- dim(foms[[1]])[1]
  I2 <- dim(foms[[2]])[1]
  J <- dim(foms[[1]])[2]
  
  fomMean <- mean(foms[[1]]) # same as mean(foms[[2]])
  
  I <- c(I2,I1)
  
  TRanova <- list()
  cov2EachTrt <- list()
  varEachTrt <- list()
  VarCom <- list()
  IndividualTrt <- list()
  IndividualRdr <- list()
  
  for (avgIndx in 1:2) { # treatment index that FOM was averaged over
    if (I[avgIndx] > 1) {
      msT <- 0
      for (i in 1:I[avgIndx]) {
        msT <- msT + (mean(foms[[avgIndx]][i,]) - fomMean)^2
      }
      msT <- J * msT/(I[avgIndx] - 1)
    } else msT <- NA
    
    if (J > 1) {
      msR <- 0
      for (j in 1:J) {
        msR <- msR + (mean(foms[[avgIndx]][, j]) - fomMean)^2
      }
      msR <- I[avgIndx] * msR/(J - 1)
    } else msR <- NA
    
    if ((I[avgIndx] > 1) && (J > 1)) {
      msTR <- 0
      for (i in 1:I[avgIndx]) {
        for (j in 1:J) {
          msTR <- msTR + 
            (foms[[avgIndx]][i, j] - 
               mean(foms[[avgIndx]][i, ]) - mean(foms[[avgIndx]][, j]) + fomMean)^2
        }
      }
      msTR <- msTR/((J - 1) * (I[avgIndx] - 1))
    } else msTR <- NA
    
    msArray <- c(msT, msR, msTR)
    dfArray <- c(I[avgIndx] - 1, J - 1, (I[avgIndx] - 1) * (J - 1))
    ssArray <- msArray[[avgIndx]] * dfArray[[avgIndx]]
    
    TRanova[[avgIndx]] <- data.frame("SS" = ssArray, 
                                     "DF" = dfArray, 
                                     "MS" = msArray,
                                     stringsAsFactors = FALSE)  
    rownames(TRanova[[avgIndx]]) <- c("T", "R", "TR")
    
    # single modality msR_i ############################################################
    if (J > 1) {
      msR_i <- array(0, dim = I[avgIndx])
      for (i in 1:I[avgIndx]) {
        for (j in 1:J) {
          msR_i[i] <- msR_i[i] + (foms[[avgIndx]][i, j] -  mean(foms[[avgIndx]][i,]))^2
        }
      }
      msR_i <- msR_i/(J - 1)
    } else msR_i <- NA

    varEachTrt <- VarCovALL$vcEachTrt[[avgIndx]][,1]
    cov2EachTrt <- VarCovALL$vcEachTrt[[avgIndx]][,2]

    modID <- as.vector(modalityID[[avgIndx]])
    IndividualTrt[[avgIndx]] <- data.frame(DF = rep(J-1, I[avgIndx]),
                                msREachTrt = msR_i,
                                varEachTrt = varEachTrt,
                                cov2EachTrt = cov2EachTrt,
                                row.names = paste0("trt", modID),
                                stringsAsFactors = FALSE)

    # single reader msT_j ###############################################################
    if (I[avgIndx] > 1) {
      msT_j <- array(0, dim = J)
      for (j in 1:J) {
        for (i in 1:I[avgIndx]) {
          msT_j[j] <- msT_j[j] + (mean(foms[[avgIndx]][i, j]) -  mean(foms[[avgIndx]][,j]))^2
        }
        msT_j[j] <- msT_j[j]/(I[avgIndx] - 1)
      }
    } else msT_j <- NA

    varEachRdr <- VarCovALL$vcEachRdr[[avgIndx]][,1]
    cov1EachRdr <- VarCovALL$vcEachRdr[[avgIndx]][,2]

    rdrID <- as.vector(readerID)
    if (I[avgIndx] > 1) {
      IndividualRdr[[avgIndx]] <- data.frame(DF = rep(I[avgIndx]-1, J),
                                  msTEachRdr = msT_j,
                                  varEachRdr = varEachRdr,
                                  cov1EachRdr = cov1EachRdr,
                                  row.names = paste0("rdr", rdrID),
                                  stringsAsFactors = FALSE)
    } else IndividualRdr[[avgIndx]] <- NA

    Var <- VarCovALL$vc[[avgIndx]][1]
    Cov1 <- VarCovALL$vc[[avgIndx]][2]
    Cov2 <- VarCovALL$vc[[avgIndx]][3]
    Cov3 <- VarCovALL$vc[[avgIndx]][4]

    if (I[avgIndx] > 1) {
      VarTR <- msTR - Var + Cov1 + max(Cov2 - Cov3, 0)
    } else VarTR <- NA

    VarR <- (msR - VarTR - Var + Cov2 - (I[avgIndx]-1)*(Cov1 - Cov3))/I[avgIndx]
    VarCom[[avgIndx]] <- data.frame(Estimates = c(VarR[avgIndx], VarTR[avgIndx], Cov1, Cov2, Cov3, Var),
                         Rhos = c(NA, NA, Cov1/Var, Cov2/Var, Cov3/Var, NA),
                         row.names = c("VarR", "VarTR", "Cov1", "Cov2", "Cov3", "Var"),
                         stringsAsFactors = FALSE)
  
  }
  
  return(list(
    TRanova = TRanova,
    VarCom = VarCom,
    IndividualTrt = IndividualTrt,
    IndividualRdr = IndividualRdr
  ))
}