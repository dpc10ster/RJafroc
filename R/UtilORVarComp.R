#' Obuchowski-Rockette variance components for dataset
#' 
#' @param dataset Factorial one-treatment or cross-modality two-treatment dataset
#' 
#' @param FOM Figure of merit
#' 
#' @param covEstMethod The covariance estimation method, "jackknife" 
#'     (the default) or "bootstrap" or "DeLong" ("DeLong" is applicable only for 
#'     FOM = "Wilcoxon").
#'     
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" 
#'     or "ALROC": the \code{FPFValue} at which to evaluate a partial curve 
#'     based figure of merit. The default is 0.2.
#'     
#' @param nBoots  The number of bootstraps (default 200).Only needed for 
#'     covEstMethod = "bootstrap". 
#'     
#' @param seed  Only needed for the bootstrap covariance estimation method. 
#'     The initial seed for the random number generator, the default is 
#'     \code{NULL}, for random seed. 
#'     
#' @return A list containing the following \code{data.frames}: 
#'     \itemize{
#'     \item{\code{foms}}: the figures of merit for different modality-reader combinations 
#'     \item{\code{TRanova}}: the OR modality-reader ANOVA table 
#'     \item{\code{VarCom}}: the OR variance-components \code{Cov1}, \code{Cov2}, 
#'     \code{Cov3}, \code{Var} and correlations \code{rho1}, \code{rho2} and \code{rho3} 
#'     \item{\code{IndividualTrt}}: the individual modality mean-squares, \code{Var} and \code{Cov2} values
#'     \item{\code{IndividualRdr}}: the individual reader mean-squares, \code{Var} and \code{Cov1} values
#'     }
#'   
#' @details The variance components are identical to those obtained using 
#'     \link{St} with \code{method = "OR"}.
#' 
#' @examples 
#' ## use the default jackknife for covEstMethod
#' vc <- UtilORVarComp(dataset02, FOM = "Wilcoxon")
#'
#' ##UtilORVarComp(dataset02, FOM = "Wilcoxon", covEstMethod = "bootstrap", 
#' ##nBoots = 2000, seed = 100)$VarCom 
#' 
#' ##UtilORVarComp(dataset02, FOM = "Wilcoxon", covEstMethod = "DeLong")$VarCom
#' 
#' vc <- UtilORVarComp(datasetX, FOM = "wAFROC") 
#'   
#' @export
#' 
UtilORVarComp <- function (dataset, 
                           FOM, 
                           covEstMethod = "jackknife", 
                           FPFValue = 0.2, 
                           nBoots = 200, 
                           seed = NULL)
{
  
  if (dataset$descriptions$design == "FCTRL") { 
    # factorial one-treatment dataset
    
    modalityID <- dataset$descriptions$modalityID
    readerID <- dataset$descriptions$readerID
    
    foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    jkFomValues <- UtilPseudoValues(dataset, FOM, FPFValue)$jkFomValues
    
    VarCovOR <- SmpldFom2ORCov(jkFomValues, 
                               modalityID, 
                               readerID, 
                               covEstMethod, 
                               FPFValue, 
                               nBoots, 
                               seed)
    
    Output <- OROutput(foms, 
                       VarCovOR, 
                       modalityID, 
                       readerID)
    
  } else {
    # cross-modality factorial dataset, two treatment factors
    
    modalityID1 <- dataset$descriptions$modalityID1
    modalityID2 <- dataset$descriptions$modalityID2
    modalityID <- list(modalityID2, modalityID1)
    readerID <- dataset$descriptions$readerID
    
    foms_array <- UtilFigureOfMerit(dataset, FOM, FPFValue)
    foms <- Arr2List(dataset, foms_array)
    
    jkFomValues <- UtilPseudoValues(dataset, FOM, FPFValue)$jkFomValues
    
    VarCovOR <- SmpldFom2ORCov(jkFomValues, 
                               modalityID, 
                               readerID, 
                               covEstMethod, 
                               FPFValue, 
                               nBoots, 
                               seed)
    
    Output <- OROutput(foms, VarCovOR, modalityID, readerID)
    
  } 
  return(Output)
}




SmpldFom2ORCov <- function(jkFomValues, 
                           modalityID, 
                           readerID, 
                           covEstMethod, 
                           FPFValue, 
                           nBoots, 
                           seed) 
{
  
  if (covEstMethod == "jackknife") {
    
    if (length(dim(jkFomValues)) == 3) {
      # factorial one-treatment dataset
      
      I <- dim(jkFomValues)[1]
      J <- dim(jkFomValues)[2]
      
      x <- FOM2ORVarComp(jkFomValues, varInflFactor = TRUE, flag = "IJ")
      vc <- array(dim = 4)
      vc[1] <- x$Var
      vc[2] <- x$Cov1
      vc[3] <- x$Cov2
      vc[4] <- x$Cov3
      names(vc) <- c("Var", "Cov1", "Cov2", "Cov3")
      
      vcEachTrt <- array(dim = c(I,2))
      for (i in 1:I) {
        x <- FOM2ORVarComp(jkFomValues[i,,], varInflFactor = TRUE, flag = "J")
        vcEachTrt[i,1] <- x$Var 
        vcEachTrt[i,2] <- x$Cov2    
      }
      rownames(vcEachTrt) <- paste0("trt", modalityID)
      colnames(vcEachTrt) <- c("Var", "Cov2")
      
      vcEachRdr <- array(dim = c(J,2))
      for (j in 1:J) {
        x <- FOM2ORVarComp(jkFomValues[,j,], varInflFactor = TRUE, flag = "I")
        vcEachRdr[j,1] <- x$Var
        vcEachRdr[j,2] <- x$Cov1
      }
      rownames(vcEachRdr) <- paste0("rdr", readerID)
      colnames(vcEachRdr) <- c("Var", "Cov1")
      
    } else if (length(dim(jkFomValues)) == 4) {
      # cross-modality factorial dataset, two treatment factors
      
      I1 <- dim(jkFomValues)[1]
      I2 <- dim(jkFomValues)[2]
      J <- dim(jkFomValues)[3]
      I <- c(I2, I1)
      
      fomAvgArray <- list()
      vc <- list()
      vcEachTrt <- list()
      vcEachRdr <- list()
      
      for (avgIndx in 1:2) {
        
        # average over first modality and all readers
        fomAvgArray[[avgIndx]] <- apply(jkFomValues, (1:4)[-avgIndx], mean) 
        
        x <- FOM2ORVarComp(fomAvgArray[[avgIndx]], varInflFactor = TRUE, flag = "IJ")
        vc[[avgIndx]] <- array(dim = 4)
        vc[[avgIndx]][1] <- x$Var
        vc[[avgIndx]][2] <- x$Cov1
        vc[[avgIndx]][3] <- x$Cov2
        vc[[avgIndx]][4] <- x$Cov3
        names(vc[[avgIndx]]) <- c("Var", "Cov1", "Cov2", "Cov3")
        
        vcEachTrt[[avgIndx]] <- array(dim = c(I[avgIndx],2))
        for (i in 1:I[avgIndx]) {
          x <- FOM2ORVarComp(fomAvgArray[[avgIndx]][i,,], varInflFactor = TRUE, flag = "J")
          vcEachTrt[[avgIndx]][i,1] <- x$Var 
          vcEachTrt[[avgIndx]][i,2] <- x$Cov2    
        }
        rownames(vcEachTrt[[avgIndx]]) <- modalityID[[avgIndx]]
        colnames(vcEachTrt[[avgIndx]]) <- c("Var", "Cov2")
        
        vcEachRdr[[avgIndx]] <- array(dim = c(J,2))
        for (j in 1:J) {
          x <- FOM2ORVarComp(fomAvgArray[[avgIndx]][,j,], varInflFactor = TRUE, flag = "I")
          vcEachRdr[[avgIndx]][j,1] <- x$Var
          vcEachRdr[[avgIndx]][j,2] <- x$Cov1
        }
        rownames(vcEachRdr[[avgIndx]]) <- readerID
        colnames(vcEachRdr[[avgIndx]]) <- c("Var", "Cov1")
      }
    }
    
    return (list(
      vc = vc,
      vcEachTrt = vcEachTrt,
      vcEachRdr = vcEachRdr
    ))
    
  } else if (covEstMethod == "bootstrap") {
    
    stop("code needs fixing: SmpldFom2ORCov bootstrap")
    # ret <- varCompBS (dataset, FOM, FPFValue, nBoots, seed)
    
  } else if (covEstMethod == "DeLong") {
    
    stop("code needs fixing: SmpldFom2ORCov DeLong")
    # ret <- varCompDeLong (dataset)
    
  } 
  
}  




OROutput <- function (foms, VarCov, modalityID, readerID)
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
    # cross-modality factorial dataset, two treatment factors
    
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
                                               row.names = paste0("rdr" ,rdrID),
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



