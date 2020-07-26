#' Significance testing using Obuchowski-Rockette variance components for split plot datasets
#' 
#' @param dataset The split plot dataset object, types A and C are currently supported
#' @param FOM The figure of merit
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#'     
#' @return A list object containing the following \code{data.frames}: 
#'     \itemize{
#'     \item{\code{foms}}: the figures of merit for different treatment-reader combinations 
#'     \item{\code{TRanova}}: the OR treatment-reader ANOVA table 
#'     \item{\code{VarCom}}: the OR variance-components \code{Cov1}, \code{Cov2}, 
#'     \code{Cov3}, \code{Var} and correlations \code{rho1}, \code{rho2} and \code{rho3} 
#'     \item{\code{IndividualTrt}}: the individual treatment mean-squares, \code{Var} and \code{Cov2} values
#'     \item{\code{IndividualRdr}}: the individual reader mean-squares, \code{Var} and \code{Cov1} values
#'     }
#'   
#' @details The variance components are obtained using \link{StSignificanceTesting} 
#'     with \code{method = "OR"}.
#' 
#' @examples 
#'   
#' @export
#' 

StORHAnalysisSp <- function(dataset, FOM, FPFValue, alpha = 0.05, analysisOption = "ALL")  
{
  covEstMethod <- "jackknife"
  if (dataset$descriptions$design == "SPLIT-PLOT-A") 
    StORHAnalysisSpA(
    dataset, FOM, FPFValue, alpha, analysisOption = "ALL")  
  
  else if (dataset$descriptions$design == "SPLIT-PLOT-C") 
    StORHAnalysisSpC(
      dataset, FOM, FPFValue, alpha, analysisOption = "ALL")  
  
  else stop("Incorrect study design: must be SPLIT-PLOT-A or SPLIT-PLOT-C")
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  
  # `as.matrix` is NOT absolutely necessary as `mean()` function is not used
  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  ret <- VarComponentsORSpA(dataset, FOM, FPFValue, covEstMethod, nBoots)
  
  TRanova <- ret$TRanova
  VarCom <-  ret$VarCom
  IndividualTrt <- ret$IndividualTrt
  IndividualRdr <- ret$IndividualRdr
  
  ANOVA <- list()
  ANOVA$TRanova <- TRanova
  ANOVA$VarCom <- VarCom
  ANOVA$IndividualTrt <- IndividualTrt
  ANOVA$IndividualRdr <- IndividualRdr
  
  trtMeans <- rowMeans(foms)
  trtMeans <- as.data.frame(trtMeans)
  colnames(trtMeans) <- "Estimate"
  
  trtMeanDiffs <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
      diffTRName[ii] <- paste0("trt", modalityID[i], sep = "-", "trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("Estimate" = trtMeanDiffs,
                             row.names = diffTRName,
                             stringsAsFactors = FALSE)
  
  FOMs <- list(
    foms = foms,
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs
  )
  
  if (analysisOption == "RRRC") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
  
} 



StORHAnalysisSpA <- function(dataset, FOM, FPFValue, alpha = 0.05, analysisOption = "ALL")  
{
  
  covEstMethod <- "jackknife"
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  
  # `as.matrix` is NOT absolutely necessary as `mean()` function is not used
  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  ret <- VarComponentsORSpA(dataset, FOM, FPFValue, covEstMethod, nBoots)
  
  TRanova <- ret$TRanova
  VarCom <-  ret$VarCom
  IndividualTrt <- ret$IndividualTrt
  IndividualRdr <- ret$IndividualRdr
  
  ANOVA <- list()
  ANOVA$TRanova <- TRanova
  ANOVA$VarCom <- VarCom
  ANOVA$IndividualTrt <- IndividualTrt
  ANOVA$IndividualRdr <- IndividualRdr
  
  trtMeans <- rowMeans(foms)
  trtMeans <- as.data.frame(trtMeans)
  colnames(trtMeans) <- "Estimate"
  
  trtMeanDiffs <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
      diffTRName[ii] <- paste0("trt", modalityID[i], sep = "-", "trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("Estimate" = trtMeanDiffs,
                             row.names = diffTRName,
                             stringsAsFactors = FALSE)
  
  FOMs <- list(
    foms = foms,
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs
  )
  
  if (analysisOption == "RRRC") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
  
} 


StORHAnalysisSpC <- function(dataset, FOM, FPFValue, alpha = 0.05, analysisOption = "ALL")  
{
  
  covEstMethod <- "jackknife"
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  
  # `as.matrix` is NOT absolutely necessary as `mean()` function is not used
  foms <- UtilFigureOfMerit(dataset, FOM, FPFValue)
  
  ret <- VarComponentsORSpC(dataset, FOM, FPFValue, covEstMethod, nBoots)
  
  TRanova <- ret$TRanova
  VarCom <-  ret$VarCom
  IndividualTrt <- ret$IndividualTrt
  IndividualRdr <- ret$IndividualRdr
  
  ANOVA <- list()
  ANOVA$TRanova <- TRanova
  ANOVA$VarCom <- VarCom
  ANOVA$IndividualTrt <- IndividualTrt
  ANOVA$IndividualRdr <- IndividualRdr
  
  trtMeans <- rowMeans(foms)
  trtMeans <- as.data.frame(trtMeans)
  colnames(trtMeans) <- "Estimate"
  
  trtMeanDiffs <- array(dim = choose(I, 2))
  diffTRName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
      diffTRName[ii] <- paste0("trt", modalityID[i], sep = "-", "trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("Estimate" = trtMeanDiffs,
                             row.names = diffTRName,
                             stringsAsFactors = FALSE)
  
  FOMs <- list(
    foms = foms,
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs
  )
  
  if (analysisOption == "RRRC") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- ORSummaryRRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    FRRC <- ORSummaryFRRC(dataset, FOMs, ANOVA, alpha, diffTRName)
    RRFC <- ORSummaryRRFC(dataset, FOMs, ANOVA, alpha, diffTRName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
  
} 


VarComponentsORSpA <- function (dataset, FOM, FPFValue = 0.2, 
                                covEstMethod = "jackknife", nBoots = 200, seed = NULL)
{
  
  if (dataset$descriptions$design != "SPLIT-PLOT-A") stop("This functions requires a SPLIT-PLOT-A dataset")  
  
  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
  
  # Foms is local value; 
  # `as.matrix` is absolutely necessary if following `mean()` function is to work
  Foms <- as.matrix(UtilFigureOfMerit(dataset, FOM, FPFValue))
  
  fomMean <- mean(Foms[,]) # this fails if `Foms` is a dataframe; true for `mean` and `median`
  
  if (I > 1) {
    msT <- 0
    for (i in 1:I) {
      msT <- msT + (mean(Foms[i, ]) - fomMean)^2
    }
    msT <- J * msT/(I - 1)
  } else msT <- NA
  
  if (J > 1) {
    msR <- 0
    for (j in 1:J) {
      msR <- msR + (mean(Foms[, j]) - fomMean)^2
    }
    msR <- I * msR/(J - 1)
  } else msR <- NA
  
  if ((I > 1) && (J > 1)) {
    msTR <- 0
    for (i in 1:I) {
      for (j in 1:J) {
        msTR <- msTR + (Foms[i, j] - mean(Foms[i, ]) - mean(Foms[, j]) + fomMean)^2
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
  
  # single treatment msR_i ############################################################
  if (J > 1) {
    msR_i <- array(0, dim = I)
    for (i in 1:I) {
      for (j in 1:J) {
        msR_i[i] <- msR_i[i] + (Foms[i, j] -  mean(Foms[i,]))^2
      }
    }
    msR_i <- msR_i/(J - 1)
  } else msR_i <- NA
  
  cov2EachTrt <- vector(length = I)
  varEachTrt <- vector(length = I)
  for (i in 1:I) {
    dsi <- DfExtractDataset(dataset, trts = i)
    ret <- OrVarCovMatrixSpA(dsi, FOM, FPFValue, nBoots, covEstMethod, seed)
    varEachTrt[i] <- ret$Var
    cov2EachTrt[i] <- ret$Cov2
  }
  
  modID <- as.vector(dataset$descriptions$modalityID)
  IndividualTrt <- data.frame(DF = rep(J-1, I), 
                              msREachTrt = msR_i, 
                              varEachTrt = varEachTrt, 
                              cov2EachTrt = cov2EachTrt, 
                              row.names = paste0("trt", modID),
                              stringsAsFactors = FALSE)
  # } else IndividualTrt <- NA # these are not defined for split-plot-c datasets
  
  # single reader msT_j ###############################################################
  if (I > 1) {
    msT_j <- array(0, dim = J)
    for (j in 1:J) {
      for (i in 1:I) {
        msT_j[j] <- msT_j[j] + (mean(Foms[i, j]) -  mean(Foms[,j]))^2
      }
      msT_j[j] <- msT_j[j]/(I - 1)
    }
  } else msT_j <- NA
  
  varEachRdr <- vector(length = J)
  cov1EachRdr <- vector(length = J)
  for (j in 1:J) {
    dsj <- DfExtractDataset(dataset, rdrs = j)
    ret <- OrVarCovMatrixSpA(dsj, FOM, FPFValue, nBoots, covEstMethod, seed)
    varEachRdr[j] <- ret$Var
    cov1EachRdr[j] <- ret$Cov1
  }
  
  rdrID <- as.vector(dataset$descriptions$readerID)
  if (I > 1) {
    IndividualRdr <- data.frame(DF = rep(I-1, J), 
                                msTEachRdr = msT_j, 
                                varEachRdr = varEachRdr, 
                                cov1EachRdr = cov1EachRdr, 
                                row.names = paste0("rdr", rdrID),
                                stringsAsFactors = FALSE)
  } else IndividualRdr <- NA
  #####################################################################################
  ret <- OrVarCovMatrixSpA(dataset, FOM, FPFValue, nBoots, covEstMethod, seed)
  Var <- ret$Var
  Cov1 <- ret$Cov1
  Cov2 <- ret$Cov2
  Cov3 <- ret$Cov3
  
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
  return(list(
    TRanova = TRanova,
    VarCom = VarCom,
    IndividualTrt = IndividualTrt,
    IndividualRdr = IndividualRdr
  ))
  
}


VarComponentsORSpC <- function (dataset, FOM, FPFValue = 0.2, 
                                covEstMethod = "jackknife", nBoots = 200, seed = NULL)
{
  
  if (dataset$descriptions$design != "SPLIT-PLOT-C") stop("This functions requires a SPLIT-PLOT-C dataset")  
  
  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
  
  # Foms is local value; 
  # `as.matrix` is absolutely necessary if following `mean()` function is to work
  Foms <- as.matrix(UtilFigureOfMerit(dataset, FOM, FPFValue))
  
  fomMean <- mean(Foms[,]) # this fails if `Foms` is a dataframe; true for `mean` and `median`
  
  if (I > 1) {
    msT <- 0
    for (i in 1:I) {
      msT <- msT + (mean(Foms[i, ]) - fomMean)^2
    }
    msT <- J * msT/(I - 1)
  } else msT <- NA
  
  if (J > 1) {
    msR <- 0
    for (j in 1:J) {
      msR <- msR + (mean(Foms[, j]) - fomMean)^2
    }
    msR <- I * msR/(J - 1)
  } else msR <- NA
  
  if ((I > 1) && (J > 1)) {
    msTR <- 0
    for (i in 1:I) {
      for (j in 1:J) {
        msTR <- msTR + (Foms[i, j] - mean(Foms[i, ]) - mean(Foms[, j]) + fomMean)^2
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
  
  # single treatment msR_i ############################################################
  if (J > 1) {
    msR_i <- array(0, dim = I)
    for (i in 1:I) {
      for (j in 1:J) {
        msR_i[i] <- msR_i[i] + (Foms[i, j] -  mean(Foms[i,]))^2
      }
    }
    msR_i <- msR_i/(J - 1)
  } else msR_i <- NA
  
  cov2EachTrt <- vector(length = I)
  varEachTrt <- vector(length = I)
  for (i in 1:I) {
    dsi <- DfExtractDataset(dataset, trts = i)
    ret <- OrVarCovMatrixSpC(dsi, FOM, FPFValue, nBoots, covEstMethod, seed)
    varEachTrt[i] <- ret$Var
    cov2EachTrt[i] <- ret$Cov2
  }
  
  modID <- as.vector(dataset$descriptions$modalityID)
  IndividualTrt <- data.frame(DF = rep(J-1, I), 
                              msREachTrt = msR_i, 
                              varEachTrt = varEachTrt, 
                              cov2EachTrt = cov2EachTrt, 
                              row.names = paste0("trt", modID),
                              stringsAsFactors = FALSE)
  # } else IndividualTrt <- NA # these are not defined for split-plot-c datasets
  
  # single reader msT_j ###############################################################
  if (I > 1) {
    msT_j <- array(0, dim = J)
    for (j in 1:J) {
      for (i in 1:I) {
        msT_j[j] <- msT_j[j] + (mean(Foms[i, j]) -  mean(Foms[,j]))^2
      }
      msT_j[j] <- msT_j[j]/(I - 1)
    }
  } else msT_j <- NA
  
  varEachRdr <- vector(length = J)
  cov1EachRdr <- vector(length = J)
  for (j in 1:J) {
    dsj <- DfExtractDataset(dataset, rdrs = j)
    ret <- OrVarCovMatrixSpC(dsj, FOM, FPFValue, nBoots, covEstMethod, seed)
    varEachRdr[j] <- ret$Var
    cov1EachRdr[j] <- ret$Cov1
  }
  
  rdrID <- as.vector(dataset$descriptions$readerID)
  if (I > 1) {
    IndividualRdr <- data.frame(DF = rep(I-1, J), 
                                msTEachRdr = msT_j, 
                                varEachRdr = varEachRdr, 
                                cov1EachRdr = cov1EachRdr, 
                                row.names = paste0("rdr", rdrID),
                                stringsAsFactors = FALSE)
  } else IndividualRdr <- NA
  #####################################################################################
  ret <- OrVarCovMatrixSpC(dataset, FOM, FPFValue, nBoots, covEstMethod, seed)
  Var <- ret$Var
  Cov1 <- ret$Cov1
  Cov2 <- ret$Cov2
  Cov3 <- ret$Cov3
  
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
  return(list(
    TRanova = TRanova,
    VarCom = VarCom,
    IndividualTrt = IndividualTrt,
    IndividualRdr = IndividualRdr
  ))
  
}


