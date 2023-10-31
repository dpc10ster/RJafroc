OrFinalOutput <- function (foms, VarCov, modalityID, readerID)
{
  
  if (!is.list(foms)) {
    # factorial one-treatment dataset 
    
    I <- dim(foms)[1]
    J <- dim(foms)[2]
    
    fomMean <- mean(foms)
    
    if (I > 1) {
      msT <- 0
      for (i in 1:I) {
        msT <- msT + (mean(foms[i, ]) - fomMean)^2
      }
      msT <- J * msT/(I - 1)
    } else msT <- NA
    
    if (J > 1) {
      msR <- 0
      for (j in 1:J) {
        msR <- msR + (mean(foms[, j]) - fomMean)^2
      }
      msR <- I * msR/(J - 1)
    } else msR <- NA
    
    if ((I > 1) && (J > 1)) {
      msTR <- 0
      for (i in 1:I) {
        for (j in 1:J) {
          msTR <- msTR + (foms[i, j] - mean(foms[i, ]) - mean(foms[, j]) + fomMean)^2
        }
      }
      msTR <- msTR/((J - 1) * (I - 1))
    } else msTR <- NA
    
    msArray <- c(msT, msR, msTR)
    dfArray <- c(I - 1, J - 1, (I - 1) * (J - 1))
    ssArray <- msArray * dfArray
    
    TRanova <- data.frame("SS" = ssArray, 
                          "DF" = dfArray, 
                          "MS" = msArray,
                          stringsAsFactors = FALSE)  
    rownames(TRanova) <- c("T", "R", "TR")
    
    # single modality msR_i ############################################################
    if (J > 1) {
      msR_i <- array(0, dim = I)
      for (i in 1:I) {
        for (j in 1:J) {
          msR_i[i] <- msR_i[i] + (foms[i, j] -  mean(foms[i,]))^2
        }
      }
      msR_i <- msR_i/(J - 1)
    } else msR_i <- NA
    
    varEachTrt <- VarCov$vcEachTrt[,1]
    cov2EachTrt <- VarCov$vcEachTrt[,2]
    
    modID <- as.vector(modalityID)
    IndividualTrt <- data.frame(DF = rep(J-1, I), 
                                msREachTrt = msR_i, 
                                varEachTrt = varEachTrt, 
                                cov2EachTrt = cov2EachTrt, 
                                row.names = paste0("trt", modID),
                                stringsAsFactors = FALSE)
    
    # single reader msT_j ###############################################################
    if (I > 1) {
      msT_j <- array(0, dim = J)
      for (j in 1:J) {
        for (i in 1:I) {
          msT_j[j] <- msT_j[j] + (mean(foms[i, j]) -  mean(foms[,j]))^2
        }
        msT_j[j] <- msT_j[j]/(I - 1)
      }
    } else msT_j <- NA
    
    varEachRdr <- VarCov$vcEachRdr[,1]
    cov1EachRdr <- VarCov$vcEachRdr[,2]
    
    rdrID <- as.vector(readerID)
    if (I > 1) {
      IndividualRdr <- data.frame(DF = rep(I-1, J), 
                                  msTEachRdr = msT_j, 
                                  varEachRdr = varEachRdr, 
                                  cov1EachRdr = cov1EachRdr, 
                                  row.names = paste0("rdr", rdrID),
                                  stringsAsFactors = FALSE)
    } else IndividualRdr <- NA
    #####################################################################################
    Var <- VarCov$vc[1]
    Cov1 <- VarCov$vc[2]
    Cov2 <- VarCov$vc[3]
    Cov3 <- VarCov$vc[4]
    
    if (I > 1) {
      # Following equation is in marginal means paper, page 333
      # and in Hillis 2011 Eqn 9
      VarTR <- msTR - Var + Cov1 + max(Cov2 - Cov3, 0)
      # NOTE on discrepancy between Var(R) and Var(TR) values reported by
      # OR-DBM MRMC 2.51 Build 20181028 and my code for Franken dataset
      # Their code does not implement the max() constraint while mine does
      # my code reports VarTR = -0.00068389146 while their code reports
      # VarTR = -0.00071276; This is shown explicitly next:
      # msTR - Var + Cov1 + max(Cov2 - Cov3, 0) = -0.00068389146 
      # msTR - Var + Cov1 +     Cov2 - Cov3     = -0.00071276 
      # This also affects the VarR values calculated next (see next block of comments)
      # Cov1, Cov2, Cov3 and Var are the same between both codes
    } else VarTR <- NA
    
    # See Hillis 2006 Table 1 2nd eauation
    VarR <- (msR - VarTR - Var + Cov2 - (I-1)*(Cov1 - Cov3))/I
    # Their code reports: VarR = 0.00003766 
    # my code reports: VarR = 2.3319942e-05
    # This is shown explicitly next:
    # (msR - Var - (I - 1) * Cov1 + Cov2 + (I - 1) * Cov3 - (-0.00071276))/I = 3.7754211e-05
    # (msR - Var - (I - 1) * Cov1 + Cov2 + (I - 1) * Cov3 - VarTR)/I = 2.3319942e-05
    VarCom <- data.frame(Estimates = c(VarR, VarTR, Cov1, Cov2, Cov3, Var), 
                         Rhos = c(NA, NA, Cov1/Var, Cov2/Var, Cov3/Var, NA),
                         row.names = c("VarR", "VarTR", "Cov1", "Cov2", "Cov3", "Var"),
                         stringsAsFactors = FALSE)
    
    vc <- list(
      TRanova = TRanova,
      VarCom = VarCom,
      IndividualTrt = IndividualTrt,
      IndividualRdr = IndividualRdr
    )  
    
  } else {
    
    I1 <- dim(foms[[2]])[1]
    I2 <- dim(foms[[1]])[1]
    J <- dim(foms[[1]])[2]
    
    fomMean <- mean(foms[[1]]) # same as mean(foms[[2]])
    
    TRanova <- list()
    cov2EachTrt <- list()
    varEachTrt <- list()
    VarCom <- list()
    IndividualTrt <- list()
    IndividualRdr <- list()
    
    VarR <- array(dim = 2)
    VarTR <- array(dim = 2)
    I <- c(I2,I1)
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
      
      varEachTrt <- VarCov$vcEachTrt[[avgIndx]][,1]
      cov2EachTrt <- VarCov$vcEachTrt[[avgIndx]][,2]
      
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
      
      varEachRdr <- VarCov$vcEachRdr[[avgIndx]][,1]
      cov1EachRdr <- VarCov$vcEachRdr[[avgIndx]][,2]
      
      rdrID <- as.vector(readerID)
      if (I[avgIndx] > 1) {
        IndividualRdr[[avgIndx]] <- data.frame(DF = rep(I[avgIndx]-1, J),
                                               msTEachRdr = msT_j,
                                               varEachRdr = varEachRdr,
                                               cov1EachRdr = cov1EachRdr,
                                               row.names = paste0("rdr", rdrID),
                                               stringsAsFactors = FALSE)
      } else IndividualRdr[[avgIndx]] <- NA
      
      Var <- VarCov$vc[[avgIndx]][1]
      Cov1 <- VarCov$vc[[avgIndx]][2]
      Cov2 <- VarCov$vc[[avgIndx]][3]
      Cov3 <- VarCov$vc[[avgIndx]][4]
      
      if (I[avgIndx] > 1) {
        VarTR[avgIndx] <- msTR - Var + Cov1 + max(Cov2 - Cov3, 0)
      } else VarTR[avgIndx] <- NA
      
      VarR[avgIndx] <- (msR - VarTR[avgIndx] - Var + Cov2 - (I[avgIndx]-1)*(Cov1 - Cov3))/I[avgIndx]
      VarCom[[avgIndx]] <- data.frame(
        Estimates = c(VarR[avgIndx], VarTR[avgIndx], Cov1, Cov2, Cov3, Var),
        Rhos = c(NA, NA, Cov1/Var, Cov2/Var, Cov3/Var, NA),
        row.names = c("VarR", "VarTR", "Cov1", "Cov2", "Cov3", "Var"),
        stringsAsFactors = FALSE)
      
    }
    
    vc <- list(
      TRanova = TRanova,
      VarCom = VarCom,
      IndividualTrt = IndividualTrt,
      IndividualRdr = IndividualRdr
    )
    names(vc$TRanova) <- c("AvgMod1", "AvgMod2")
    names(vc$VarCom) <- c("AvgMod1", "AvgMod2")
    names(vc$IndividualTrt) <- c("AvgMod1", "AvgMod2")
    names(vc$IndividualRdr) <- c("AvgMod1", "AvgMod2")
  }  
  
  return(vc)  
  
}