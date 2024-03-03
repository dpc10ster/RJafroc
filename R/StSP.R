#' Perform significance testing for split-plot datasets
#' 
#' @description  Performs Obuchowski-Rockette significance testing for 
#'    split-plot datasets as described in Hillis 2014. 
#'
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}. 
#'     The dataset design must be "SPLIT-PLOT-A" or "SPLIT-PLOT-C". 
#' \itemize{ 
#'     \item SPLIT-PLOT-A = Reader nested within test; Hillis 2014 Table VII part (a). 
#'     \item SPLIT-PLOT-C = Case nested within reader; Hillis 2014 Table VII part (c). 
#'     \item See Note.
#' }     
#' @param FOM The figure of merit
#' 
#' @param alpha The significance level of the test, default is 0.05
#'    
#' @param analysisOption Factors regarded as random vs. those regarded as fixed:
#' \itemize{ 
#'    \item \code{"RRRC"} = random-reader random case, the default,
#'    \item \code{"FRRC"} = fixed-reader random case, 
#'    \item \code{"RRFC"} = random-reader fixed case, 
#' } 
#'    
#' @return Results of the analysis
#' 
#' 
#' @examples
#' ## Analyze included dataset 
#' fileName <- system.file("extdata", "/toyFiles/ROC/rocSpA.xlsx", 
#' package = "RJafroc", mustWork = TRUE)
#' dsSpA <- DfReadSP(fileName)
#' ret <- StSP(dsSpA, FOM = "Wilcoxon")
#' 
#' ## Analyze embedded dataset 
#' ret <- StSP(datasetFROCSpC, FOM = "wAFROC")
#' 
#' 
#' @note Two split-plot designs are supported; these are described in 
#'      Table VII in Hillis 2014:
#' \itemize{ 
#'    \item SPLIT-PLOT-A = {Reader nested within test, labeled (a) in Table VII;
#'    see for example \code{rocSpA.xlsx}}
#'    \item SPLIT-PLOT-C = {Case nested within reader, labeled (c) in Table VII;
#'    see for example \code{rocSpC.xlsx}}
#' }
#' The included toy ROC datasets are in directory \code{inst/extdata/toyFiles/ROC}.
#' 
#' 
#' @references 
#' Hillis SL (2014) A marginal-mean ANOVA approach for analyzing multireader
#' multicase radiological imaging data, Statistics in medicine 33, 330-360.
#' 
#'
#'
#' @importFrom stats pf pt qt cov pchisq pnorm qnorm
#' @importFrom Rcpp evalCpp
#' @useDynLib RJafroc
#'
#'      
#' @export
StSP <- function(dataset, FOM, alpha = 0.05, analysisOption = "RRRC")
{
  
  if (dataset$descriptions$design == "SPLIT-PLOT-A") {
    
    return(OR_SP_A(dataset, FOM, alpha, analysisOption))
    
  } else if (dataset$descriptions$design == "SPLIT-PLOT-C") {
    
    return(OR_SP_C(dataset, FOM, alpha, analysisOption))
    
  }
}



# Implement formulae in Hillis 2014 modified for unequal numbers of readers 
# In different treatments; this code assumes two treatments
OR_SP_A <- function(dataset, FOM, alpha , analysisOption)  
{
  
  I <- dim(dataset$ratings$NL)[1]; if (I != 2) stop(" ORAnalysisSplitPlotA assumes two treatments")
  J <- dim(dataset$ratings$NL)[2]
  modalityID <- dataset$descriptions$modalityID
  
  theta_ij <- as.matrix(FOM_SP(dataset, FOM))
  
  theta_i_dot <- array(dim = I) # average over readers, the indices are i followed by j
  J_i <- array(dim = I) # number of readers in treatment i
  for (i in 1:I) {
    J_i[i] <- length(theta_ij[i,][!is.na(theta_ij[i,])])
    theta_i_dot[i] <- mean(theta_ij[i,][!is.na(theta_ij[i,])])
  }
  theta_dot_dot <- mean(theta_i_dot) # average over rdrs and trts
  
  trtMeanDiffs <- theta_i_dot[1] - theta_i_dot[2] # restricted to two modalities for now
  diffTRName <- paste0("trt", modalityID[1], sep = "-", "trt", modalityID[2])
  trtMeanDiffs_df <- data.frame("Estimate" = trtMeanDiffs,
                                row.names = diffTRName,
                                stringsAsFactors = FALSE)
  
  FOMs <- list(
    foms = theta_ij,
    trtMeans = theta_i_dot,
    trtMeanDiffs = trtMeanDiffs_df
  )
  
  # msT denotes MS(T), in Hillis 2014 p 344
  msT <- 0
  for (i in 1:I) {
    # adapted from Hillis 2014, definition of MS(T), just after Eqn. 4
    # move the treatment specific J_i[i] inside the i-summation showed there
    msT <- msT + J_i[i]*(mean(theta_i_dot[i]) - theta_dot_dot)^2
  }
  msT <- msT/(I - 1) # the common J used to appear here: J*msT/(I - 1)
  
  # following is needed for CI on avg. FOM, see below
  # MS(R)_i = sum_j(theta_dot_j - theta_dot_dot)^2/(J_i-1)
  msR_i <- array(0, dim = I)
  for (i in 1:I) {
    for (j in 1:J_i[i]) {
      msR_i[i] <- msR_i[i] + 
        (mean(theta_ij[,j][!is.na(theta_ij[,j])]) - theta_dot_dot)^2
      # first term on rhs is theta_dot_j
    }
    msR_i <- msR_i / (J_i[i] - 1 )
  }
  
  # msR_T_ denotes MS[R(T)], in Hillis 2014 p 344, where R(T) is reader nested within treatment
  # adapted from Hillis 2014, definition of MS[R(T)], last line on page 344
  # move the treatment specific J_i[i] inside the i-summation showed there
  msR_T_ <- 0
  msR_T_i <- rep(0,I)
  for (i in 1:I) {
    for (j in 1:J) {
      if (is.na(theta_ij[i, j])) next
      msR_T_i[i] <- msR_T_i[i] + (theta_ij[i, j] - theta_i_dot[i])^2 / (J_i[i] - 1)
    }
  }
  msR_T_ <- sum(msR_T_i) / I
  
  trtNames <- NULL
  for (i in 1:I) {
    trtNames <- c(trtNames, paste0("trt", modalityID[i]))
  }
  
  ret1 <- Pseudovalues(dataset, FOM)
  ret <- FOMijk2VarCovSpA(ret1$jkFomValues, varInflFactor = TRUE)
  Var_i <- ret$Var_i
  Cov2_i <- ret$Cov2_i
  Cov3_i <- ret$Cov3_i
  
  ANOVA <- data.frame("msT" = c(msT, NA),
                      "msR" = msR_i,
                      "msR_T_" = msR_T_,
                      "Var_i" = Var_i,
                      "Cov2_i" = Cov2_i,
                      "Cov3_i" = Cov3_i,
                      stringsAsFactors = FALSE)  
  rownames(ANOVA) <- trtNames
  
  # now do the F_OR statistic; den = denominator of last equation on page 344
  # minus the MS[R(T)] term
  # the contributions of the two reader groups have been separately added
  den <- 0
  for (i in 1:I) den <- den + J_i[i] * max(Cov2_i[i]-Cov3_i[i],0)
  F_OR <- msT/(msR_T_ + den) # add the the MS[R(T)] term to den
  # Note: above expression reduces to equal-reader form
  
  # now implement the df3 formula on page 345
  # Assumption: degrees of freedom for each treatment can be added
  df2 <- 0
  for (i in 1:I) { 
    # temp = num. of expression on r.h.s. of Eqn. 27, 
    # separated into contribution from each treatment
    temp <- (msR_T_i[i] +  J_i[i] * max(Cov2_i[i]-Cov3_i[i],0))^2
    # divide by the denominator and multiply by (J_i[i]-1)
    # The I term is not needed, as we are summing contribution from each treatment
    temp <- temp /(msR_T_i[i]^2)*(J_i[i]-1)
    # cat("df2 for modality = ", temp, "\n")
    # Add to df2
    df2 <- df2 + temp
  } 
  # above expression reduces to equal-reader form 
  # note absence of I-factor on rhs
  # because it is effectively "in there" due to the summation over i
  
  # compute the p-value
  pValue <- 1 - pf(F_OR, I - 1, df2)
  # F_OR_num_den = array with two elements, the numerator of the expression for
  # F_OR followed by the denominator
  # DF = degrees of freedom array, numerator followed by denominator
  RRRC <- list()
  RRRC$FTests <- data.frame(DF = c((I-1),df2),
                            "F_OR_num_den" = c(msT, msR_T_ + den),
                            FStat = c(F_OR, NA),
                            p = c(pValue, NA),
                            row.names = c("Treatment", "Error"),
                            stringsAsFactors = FALSE)
  
  # confidence intervals for difference FOM, trtMeanDiffs, 
  # as on page 346, first para; note that trtMeanDiffs = theta_1_dot - theta_2_dot
  # l_1 = 1; l_2 = - 1
  # Note to myself: Hillis does not state a constraint on the l_i, namely they 
  # should sum to zero; otherwise one could use l_1 = l_2 = 1/2 and compute the CI
  # on the average FOM using the very same method, which is not right
  V_hat <- 0
  for (i in 1:I) {
    V_hat <- V_hat + 
      (1 / J_i[i]) * 2 * (msR_T_i[i] + J_i[i] * max(Cov2_i[i]-Cov3_i[i],0))
  }
  stdErr <- sqrt(V_hat)
  
  # again, this is restricted to two modalities  
  alpha <- 0.05
  CI <- sort(c(trtMeanDiffs - qt(alpha/2, df2) * stdErr, 
               trtMeanDiffs + qt(alpha/2, df2) * stdErr))
  RRRC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                               StdErr = stdErr, 
                               DF = df2, 
                               CILower = CI[1],
                               CIUpper = CI[2], 
                               row.names = "trt1-trt2", 
                               stringsAsFactors = FALSE)
  
  # confidence intervals for average FOM, as on page 346, first para
  # second method, using only data from each modality
  V_hat_i <- array(0, dim = I)
  df2_i <- array(dim = I)
  stdErr_i <- array(dim = I)
  ciAvgRdrEachTrt <- array(dim = c(I,2))
  ci <- data.frame()
  for (i in 1:I) {
    V_hat_i[i] <- {
      V_hat_i[i] + 
        # this is the formula for V_hat_i[i] in the text area in the middle of page 346
        # after we account for the different number of readers in each treatment
        (1 / J_i[i]) * (msR_i[i] + J_i[i] * max(Cov2_i[i],0))
    }
    # this is the formula for df2_i[i] in the text area in the middle of page 346
    # after we account for the different number of readers in each treatment
    df2_i[i] <- (msR_i[i] +  J_i[i] * max(Cov2_i[i],0))^2/(msR_i[i]^2)*(J_i[i]-1)
    stdErr_i[i] <- sqrt(V_hat_i[i])
    ciAvgRdrEachTrt[i,] <- sort(c(theta_i_dot[i] - qt(alpha/2, df2_i[i]) * stdErr_i[i], 
                                  theta_i_dot[i] + qt(alpha/2, df2_i[i]) * stdErr_i[i]))
    rowName <- paste0("trt", modalityID[i])
    ci <- rbind(ci, data.frame(Estimate = theta_i_dot[i], 
                               StdErr = stdErr_i[i],
                               DF = df2_i[i],
                               CILower = ciAvgRdrEachTrt[i,1],
                               CIUpper = ciAvgRdrEachTrt[i,2],
                               row.names = rowName,
                               stringsAsFactors = FALSE))
  }
  RRRC$ciAvgRdrEachTrt <- ci  
  
  return(list(
    FOMs = FOMs,
    ANOVA = ANOVA,
    RRRC = RRRC
  ))
  
} 



OR_SP_C <- function(dataset, FOM, alpha, analysisOption)  
{
  
  RRRC <- NULL
  FRRC <- NULL
  RRFC <- NULL
  
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  
  # `as.matrix` is NOT absolutely necessary as `mean()` function is not used
  foms <- FOM_SP(dataset, FOM)
  
  ret <- ORVarComponentsSpC(dataset, FOM)
  
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
  diffTrtName <- array(dim = choose(I, 2))
  ii <- 1
  for (i in 1:I) {
    if (i == I) 
      break
    for (ip in (i + 1):I) {
      trtMeanDiffs[ii] <- trtMeans[i,1] - trtMeans[ip,1]
      diffTrtName[ii] <- paste0("trt", modalityID[i], sep = "-", "trt", modalityID[ip]) # !sic
      ii <- ii + 1
    }
  }
  trtMeanDiffs <- data.frame("Estimate" = trtMeanDiffs,
                             row.names = diffTrtName,
                             stringsAsFactors = FALSE)
  
  FOMs <- list(
    foms = foms,
    trtMeans = trtMeans,
    trtMeanDiffs = trtMeanDiffs
  )
  
  if (analysisOption == "RRRC") {
    RRRC <- OR_RRRC_SP(dataset, FOMs, ANOVA, alpha, diffTrtName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC
    ))
  }  
  
  if (analysisOption == "FRRC") {
    FRRC <- OR_FRRC_SP(dataset, FOMs, ANOVA, alpha, diffTrtName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      FRRC = FRRC
    ))
  }  
  
  if (analysisOption == "RRFC") {
    RRFC <- OR_RRFC_SP(dataset, FOMs, ANOVA, alpha, diffTrtName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRFC = RRFC
    ))
  }  
  
  if (analysisOption == "ALL") {
    RRRC <- OR_RRRC_SP(dataset, FOMs, ANOVA, alpha, diffTrtName)
    FRRC <- OR_FRRC_SP(dataset, FOMs, ANOVA, alpha, diffTrtName)
    RRFC <- OR_RRFC_SP(dataset, FOMs, ANOVA, alpha, diffTrtName)
    return(list(
      FOMs = FOMs,
      ANOVA = ANOVA,
      RRRC = RRRC,
      FRRC = FRRC,
      RRFC = RRFC
    ))
  }  else stop("Incorrect analysisOption: must be `RRRC`, `FRRC`, `RRFC` or `ALL`")
  
} 



ORVarComponentsSpC <- function (dataset, FOM)
{
  
  if (dataset$descriptions$design != "SPLIT-PLOT-C") stop("This functions requires a SPLIT-PLOT-C dataset")  
  
  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
  
  theta_ij <- as.matrix(FOM_SP(dataset, FOM))
  
  theta_dot_dot <- mean(theta_ij[,]) # this fails if `theta_ij` is a dataframe; true for `mean` and `median`
  
  if (I > 1) {
    msT <- 0
    for (i in 1:I) {
      msT <- msT + (mean(theta_ij[i, ]) - theta_dot_dot)^2
    }
    msT <- J * msT/(I - 1)
  } else msT <- NA
  
  if (J > 1) {
    msR <- 0
    for (j in 1:J) {
      msR <- msR + (mean(theta_ij[, j]) - theta_dot_dot)^2
    }
    msR <- I * msR/(J - 1)
  } else msR <- NA
  
  if ((I > 1) && (J > 1)) {
    msTR <- 0
    for (i in 1:I) {
      for (j in 1:J) {
        msTR <- msTR + (theta_ij[i, j] - mean(theta_ij[i, ]) - mean(theta_ij[, j]) + theta_dot_dot)^2
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
        msR_i[i] <- msR_i[i] + (theta_ij[i, j] -  mean(theta_ij[i,]))^2
      }
    }
    msR_i <- msR_i/(J - 1)
  } else msR_i <- NA
  
  cov2EachTrt <- vector(length = I)
  varEachTrt <- vector(length = I)
  for (i in 1:I) {
    dsi <- DfExtractDataset(dataset, trts = i)
    ret <- OrVarCovMatrixSpC(dsi, FOM)
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
        msT_j[j] <- msT_j[j] + (mean(theta_ij[i, j]) -  mean(theta_ij[,j]))^2
      }
      msT_j[j] <- msT_j[j]/(I - 1)
    }
  } else msT_j <- NA
  
  varEachRdr <- vector(length = J)
  cov1EachRdr <- vector(length = J)
  for (j in 1:J) {
    dsj <- DfExtractDataset(dataset, rdrs = j)
    ret <- OrVarCovMatrixSpC(dsj, FOM)
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
  ret <- OrVarCovMatrixSpC(dataset, FOM)
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



OrVarCovMatrixSpC <- function(dataset, FOM) 
{
  if (dataset$descriptions$design != "SPLIT-PLOT-C") stop("This functions requires a split plot C dataset")  
  
  I <- length(dataset$ratings$NL[,1,1,1])
  J <- length(dataset$ratings$NL[1,,1,1])
  
  ret <- Pseudovalues(dataset, FOM)
  Var <- array(dim = J)
  Cov1 <- array(dim = J)
  caseTransitions <- ret$caseTransitions
  for (j in 1:J) {
    jkFOMs <- ret$jkFomValues[,j,(caseTransitions[j]+1):(caseTransitions[j+1]), drop = FALSE]
    kj <- length(jkFOMs)/I
    dim(jkFOMs) <- c(I,1,kj)
    x <- FOMijk2VarCov(jkFOMs, varInflFactor = FALSE)
    # not sure which way to go: was doing this until 2/18/20
    # Var[j]  <-  x$Var * (K-1)^2/K
    # Cov1[j]  <-  x$Cov1 * (K-1)^2/K
    # following seems more reasonable as reader j only interprets kj cases
    # updated file ~Dropbox/RJafrocChecks/StfrocSp.xlsx
    Var[j]  <-  x$Var * (kj-1)^2/kj
    Cov1[j]  <-  x$Cov1 * (kj-1)^2/kj
  }
  Cov <- list(
    Var = mean(Var),
    Cov1 = mean(Cov1),
    Cov2 = 0,
    Cov3 = 0
  )
  
  return(Cov)
  
}  



FOMijk2VarCov <- function(resampleFOMijk, varInflFactor) {
  I <- dim(resampleFOMijk)[1]
  J <- dim(resampleFOMijk)[2]
  K <- dim(resampleFOMijk)[3]
  
  covariances <- array(dim = c(I, I, J, J))
  
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          covariances[i, ip, j, jp] <- cov(resampleFOMijk[i, j, ], resampleFOMijk[ip, jp, ])
        }
      }
    }
  }
  
  Var <- 0
  count <- 0
  I <- dim(covariances)[1]
  J <- dim(covariances)[3]
  for (i in 1:I) {
    for (j in 1:J) {
      Var <- Var + covariances[i, i, j, j]
      count <- count + 1
    }
  }
  if (count > 0) Var <- Var/count else Var <- 0
  
  Cov1 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        if (ip != i) {
          Cov1 <- Cov1 + covariances[i, ip, j, j]
          count <- count + 1
        }
      }
    }
  }
  if (count > 0) Cov1 <- Cov1/count else Cov1 <- 0
  
  Cov2 <- 0
  count <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      for (jp in 1:J) {
        if (j != jp) {
          Cov2 <- Cov2 + covariances[i, i, j, jp]
          count <- count + 1
        }
      }
    }
  }
  if (count > 0) Cov2 <- Cov2/count else Cov2 <- 0
  
  Cov3 <- 0
  count <- 0
  for (i in 1:I) {
    for (ip in 1:I) {
      if (i != ip) {
        for (j in 1:J) {
          for (jp in 1:J) {
            if (j != jp) {
              Cov3 <- Cov3 + covariances[i, ip, j, jp]
              count <- count + 1
            }
          }
        }
      }
    }
  }
  if (count > 0) Cov3 <- Cov3/count else Cov3 <- 0
  
  if (varInflFactor)  {
    Var <-  Var * (K - 1)^2/K  # see paper by Efron and Stein 
    Cov1 <-  Cov1 * (K - 1)^2/K
    Cov2  <-  Cov2 * (K - 1)^2/K
    Cov3 <-  Cov3  * (K - 1)^2/K
  }
  
  return(list(
    Var = Var,
    Cov1 = Cov1,
    Cov2 = Cov2,
    Cov3 = Cov3
  ))
  
}



FOMijk2VarCovSpA <- function(resampleFOMijk, varInflFactor) {
  
  I <- dim(resampleFOMijk)[1]
  J <- dim(resampleFOMijk)[2]
  K <- dim(resampleFOMijk)[3]
  
  covariances <- array(dim = c(I, I, J, J))
  for (i in 1:I) {
    for (ip in 1:I) {
      for (j in 1:J) {
        for (jp in 1:J) {
          if (any(is.na(resampleFOMijk[i, j, ])) || any(is.na(resampleFOMijk[ip, jp, ]))) next
          covariances[i, ip, j, jp] <- cov(resampleFOMijk[i, j, ], resampleFOMijk[ip, jp, ])
        }
      }
    }
  }
  
  # See dropbox/RjafrocMaintenance/SpAMethods/Cov2Cov3Str.xlsx for hand calculations
  Var_i <- rep(0,I)
  count_i <- rep(0,I)
  for (i in 1:I) {
    rdr_i <- which(!is.na(resampleFOMijk[i,,1]))
    for (j in 1:length(rdr_i)) {
      if (is.na(covariances[i, i, rdr_i[j], rdr_i[j]])) next
      Var_i[i] <- Var_i[i] + covariances[i, i, rdr_i[j], rdr_i[j]]
      count_i[i] <- count_i[i] + 1
    }
    if (count_i[i] > 0) Var_i[i] <- Var_i[i]/count_i[i] else Var_i[i] <- 0
  }
  
  Cov2_i <- rep(0,I)
  count_i <- rep(0,I)
  for (i in 1:I) {
    rdr_i <- which(!is.na(resampleFOMijk[i,,1]))
    for (j in 1:length(rdr_i)) {
      for (jp in 1:length(rdr_i)) {
        if (rdr_i[j] != rdr_i[jp]) {
          if (is.na(covariances[i, i, rdr_i[j], rdr_i[jp]])) next
          Cov2_i[i] <- Cov2_i[i] + covariances[i, i, rdr_i[j], rdr_i[jp]]
          count_i[i] <- count_i[i] + 1
        }
      }
    }
    if (count_i[i] > 0) Cov2_i[i] <- Cov2_i[i]/count_i[i] else Cov2_i[i] <- 0
  }
  
  Cov3_i <- rep(0,I)
  count_i <- rep(0,I)
  for (i in 1:I) {
    for (ip in 1:I) {
      rdr_i <- which(!is.na(resampleFOMijk[i,,1]))
      rdr_ip <- which(!is.na(resampleFOMijk[ip,,1]))
      if (i != ip) {
        for (j in 1:length(rdr_i)) {
          for (jp in 1:length(rdr_ip)) {
            if (rdr_i[j] != rdr_ip[jp]) {
              if (is.na(covariances[i, ip, rdr_i[j], rdr_ip[jp]])) next
              Cov3_i[i] <- Cov3_i[i] + covariances[i, ip, rdr_i[j], rdr_ip[jp]]
              count_i[i] <- count_i[i] + 1
            }
          }
        }
      }
    }
    if (count_i[i] > 0) Cov3_i[i] <- Cov3_i[i]/count_i[i] else Cov3_i[i] <- 0
  }
  
  if (varInflFactor)  {
    Var_i <-  Var_i * (K - 1)^2/K  # see paper by Efron and Stein 
    Cov2_i  <-  Cov2_i * (K - 1)^2/K
    Cov3_i <-  Cov3_i  * (K - 1)^2/K
  }
  
  return(list(Var_i = Var_i,
              Cov2_i = Cov2_i,
              Cov3_i = Cov3_i
  ))
  
}



OR_FRRC_SP <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
  #   ===========================================================================
  #     *****    Analysis 2 (OR Analysis): Fixed Readers and Random Cases     *****
  #   ===========================================================================
  #       (Results apply to the population of cases but only for the readers used in
  #        this study. Chi-square or Z tests are used; these are appropriate for 
  #        moderate or large case sample sizes.)
  #     
  readerID <- dataset$descriptions$readerID
  modalityID <- dataset$descriptions$modalityID
  I <- length(modalityID)
  J <- length(readerID)
  K <- dim(dataset$ratings$NL)[3]
  
  foms <-  FOMs$foms
  trtMeanDiffs  <-  FOMs$trtMeanDiffs
  
  #  a) Chi-square test for H0: Treatments have the same AUC
  #  Note: The chi-square statistic is denoted by X2 or by X2(df), where df is its 
  #  corresponding degrees of freedom.
  # 
  
  # Need to check presence of max in following formula
  # max() is not present in formulae, but cant hurt, in my opinion
  FRRC <- list()
  if (I > 1) {
    if (J > 1) {
      msDen <- ANOVA$VarCom["Var","Estimates"] - ANOVA$VarCom["Cov1","Estimates"] + 
        (J - 1) * max(ANOVA$VarCom["Cov2","Estimates"] - ANOVA$VarCom["Cov3","Estimates"] ,0)
    }
    # following has to handled explicitly as otherwise it will return NA
    else msDen <- ANOVA$VarCom["Var","Estimates"] - ANOVA$VarCom["Cov1","Estimates"]
    chisqVal <- (I-1)*ANOVA$TRanova["T","MS"]/msDen
    p <- 1 - pchisq(chisqVal, I - 1)
    FRRC$FTests <- data.frame(MS = c(ANOVA$TRanova["T", "MS"], msDen),
                              Chisq = c(chisqVal,NA),
                              DF = c(I - 1, NA),
                              p = c(p,NA),
                              row.names = c("Treatment", "Error"),
                              stringsAsFactors = FALSE)
    # X2 = (t-1)*MS(T)/[Var(error) - Cov1 + (r-1)*max(Cov2 - Cov3,0)]
    
    #   b) 95% confidence intervals and hypothesis tests (H0: difference = 0)
    #   for treatment AUC differences
    stdErr <- sqrt(2 * msDen/J)
    zStat <- vector()
    PrGTz <- vector()
    CI <- array(dim = c(choose(I,2),2))
    for (i in 1:choose(I,2)) {
      zStat[i] <- trtMeanDiffs[i,1]/stdErr
      PrGTz[i] <- 2 * pnorm(abs(zStat[i]), lower.tail = FALSE)
      CI[i, ] <- c(trtMeanDiffs[i,1] + qnorm(alpha/2) * stdErr, 
                   trtMeanDiffs[i,1] + qnorm(1-alpha/2) * stdErr)
    }
    FRRC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                                 StdErr = rep(stdErr, choose(I, 2)),
                                 z = zStat, 
                                 PrGTz = PrGTz, 
                                 CILower = CI[,1],
                                 CIUpper = CI[,2], 
                                 row.names = diffTRName,
                                 stringsAsFactors = FALSE)
  }
  # StdErr = sqrt{2/r * [(Var(error) - Cov1 + (r-1)*max(Cov2 - Cov3,0)]}
  # 95% CI: difference +- z(.025) * StdErr
  
  
  # c) Single treatment AUC 95% confidence intervals
  # (Each analysis is based only on data for the specified treatment, i.e., on
  #   the specific reader ANOVA of AUCs and error-variance and Cov2 estimates.)
  # 
  # Hillis 2007 5.3. not applicable here
  # FRRC: Rdr Avg FOM for each modality obeys a normal distribution
  stdErr <- vector()
  df <- vector()
  CI <- array(dim = c(I,2))
  ci <- data.frame()
  for (i in 1:I) {
    df[i] <- K - 1
    # Following equation is out of OR-DBM MRMC 2.51 <beta> Build  20181028 </beta> #
    # TBA Need a better reference #
    # See for example, inst/Iowa/VanDyke.txt, lines 228-243; shown next: #
    # RStudio debugger buggy when I have these comments, does not stop at break points #
    # LINE 228    c) Single treatment AUC 95% confidence intervals #
    # (Each analysis is based only on data for the specified treatment, i.e., on #
    # the specific reader ANOVA of AUCs and error-variance and Cov2 estimates.) #
    # 
    # Treatment      AUC      Std Error   95% Confidence Interval #
    # ----------  ----------  ----------  ------------------------- #
    #   1  0.89703704  0.02428971  (0.84943008 , 0.94464399) #
    #   2  0.94083736  0.01677632  (0.90795637 , 0.97371835) #
    # 
    # Treatment  Var(Error)     Cov2   #
    # ----------  ----------  ---------- #
    #   1  0.00101410  0.00048396 #
    #   2  0.00059047  0.00020419 #
    # 
    #           StdErr = sqrt{1/r * [Var(error) + (r-1)*max(Cov2,0)]} #
    # LINE 243: 95% CI: AUC +- z(.025) * StdErr #
    # the Var_i and Cov2_i values check out for dataset02 #
    stdErr[i] <- sqrt((ANOVA$IndividualTrt[i,"varEachTrt"] + 
                         # added max() function 8/25/20
                         (J-1)*max(ANOVA$IndividualTrt[i,"cov2EachTrt"],0))/J)
    CI[i, ] <- c(FOMs$trtMeans[i,1] + qnorm(alpha/2) * stdErr[i],
                 FOMs$trtMeans[i,1] + qnorm(1-alpha/2) * stdErr[i])
    rowName <- paste0("trt", modalityID[i])
    ci <- rbind(ci, data.frame(Estimate = FOMs$trtMeans[i,1], 
                               StdErr = stdErr[i],
                               DF = df[i],
                               CILower = CI[i,1],
                               CIUpper = CI[i,2],
                               row.names = rowName,
                               stringsAsFactors = FALSE))
  }
  FRRC$ciAvgRdrEachTrt <- ci
  
  if (I > 1) {
    #   d) Single-reader 95% confidence intervals and tests (H0: difference = 0) for 
    #    treatment AUC differences.
    #    (Each analysis is based only on data for the specified reader, i.e, on the 
    #    reader-specific AUC, error-variance and Cov1 estimates.)
    trtMeanDiffs <- array(dim = c(J, choose(I, 2)))
    Reader <- array(dim = c(J, choose(I, 2)))
    stdErr <- array(dim = c(J, choose(I, 2)))
    zStat <- array(dim = c(J, choose(I, 2)))
    trDiffNames <- array(dim = c(J, choose(I, 2)))
    PrGTz <- array(dim = c(J, choose(I, 2)))
    CIReader <- array(dim = c(J, choose(I, 2),2))
    ci <- data.frame()
    for (j in 1:J) {
      Reader[j,] <- rep(readerID[j], choose(I, 2))
      stdErr[j,] <- sqrt(2 * (ANOVA$IndividualRdr[j,"varEachRdr"] - ANOVA$IndividualRdr[j,"cov1EachRdr"]))
      pair <- 1
      for (i in 1:I) {
        if (i == I) break
        for (ip in (i + 1):I) {
          trtMeanDiffs[j, pair] <- foms[i, j] - foms[ip, j]
          trDiffNames[j,pair] <- diffTRName[pair]
          zStat[j,pair] <- trtMeanDiffs[j,pair]/stdErr[j,pair]
          PrGTz[j,pair] <- 2 * pnorm(abs(zStat[j,pair]), lower.tail = FALSE)
          CIReader[j, pair,] <- c(trtMeanDiffs[j,pair] + qnorm(alpha/2) * stdErr[j,pair], 
                                  trtMeanDiffs[j,pair] + qnorm(1-alpha/2) * stdErr[j,pair])
          rowName <- paste0("rdr", Reader[j,pair], "::", trDiffNames[j, pair])
          ci <- rbind(ci, data.frame(Estimate = trtMeanDiffs[j, pair], 
                                     StdErr = stdErr[j,pair], 
                                     z = zStat[j, pair], 
                                     PrGTz = PrGTz[j, pair], 
                                     CILower = CIReader[j, pair,1],
                                     CIUpper = CIReader[j, pair,2],
                                     row.names = rowName,
                                     stringsAsFactors = FALSE))
          pair <- pair + 1
        }
      }
    }
    FRRC$ciDiffTrtEachRdr <- ci
    # StdErr = sqrt[2*(Var(error) - Cov1)]
    # 95% CI: Difference +- z(.025) * StdErr
    
    FRRC$IndividualRdrVarCov1 <- ANOVA$IndividualRdr[,3:4]
  }
  
  return(FRRC) 
}



OR_RRFC_SP <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
  # ===========================================================================
  #   *****    Analysis 3 (OR Analysis): Random Readers and Fixed Cases     *****
  # ===========================================================================
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  
  trtMeans <-  FOMs$trtMeans
  trtMeanDiffs  <-  FOMs$trtMeanDiffs
  
  # since IndividualTrt["Cov2","VarCom"] and IndividualTrt["Cov3","VarCom"] are zeroes for split-plot-c, FTests will be 
  # identical to FTestsRRRC; other values below may differ
  # not sure about what is going one here; I am proceeding on assumption that
  # the only difference is setting IndividualTrt["Cov2","VarCom"] = IndividualTrt["Cov3","VarCom"] = 0, and reusing code from crossed
  # analysis
  
  # (Results apply to the population of readers but only for the cases used in
  #   this study)
  # 
  # These results result from using the OR model, but treating reader as a random 
  # factor and treatment and case as fixed factors.  Because case is treated as a fixed
  # factor, it follows that Cov1 = Cov2 = Cov3 = 0; i.e., there is no correlation
  # between reader-performance measures (e.g, AUCs) due to reading the same
  # cases.  Thus the OR model reduces to a conventional treatment x reader ANOVA
  # for the reader-performance outcomes, where reader is a random factor and
  # treatment is a fixed factor.  This is the same as a repeated measures ANOVA
  # where treatment is the repeated measures factor, i.e., readers provide an
  # outcome (e.g., AUC) under each treatment.
  # Note that the DBM and OR papers do not discuss this approach, but rather 
  # it is included here for completeness.
  # 
  # a) Test for H0: Treatments have the same AUC
  msDen <- ANOVA$TRanova["TR","MS"]
  f <- ANOVA$TRanova["T","MS"]/msDen
  ddf <- ((I - 1) * (J - 1))
  p <- 1 - pf(f, I - 1, ddf)
  RRFC <- list()
  RRFC$FTests <- data.frame(DF = c(I-1,(I-1)*(J-1)), 
                            MS = c(ANOVA$TRanova["T","MS"],ANOVA$TRanova["TR","MS"]), 
                            F = c(f,NA),  p = c(p,NA), 
                            row.names = c("T","TR"), 
                            stringsAsFactors = FALSE)
  
  #   b) 95% confidence intervals and hypothesis tests (H0: difference = 0)
  #   for treatment AUC differences
  
  stdErr <- sqrt(2 * msDen/J)
  tStat <- vector()
  PrGTt <- vector()
  CI <- array(dim = c(choose(I,2), 2))
  for (i in 1:choose(I,2)) {
    tStat[i] <- trtMeanDiffs[i,1]/stdErr
    PrGTt[i] <- 2 * pt(abs(tStat[i]), ddf, lower.tail = FALSE)  # correction, noted by user Lucy D'Agostino McGowan
    CI[i, ] <- c(trtMeanDiffs[i,1] + qt(alpha/2, ddf) * stdErr, 
                 trtMeanDiffs[i,1] + qt(1-alpha/2, ddf) * stdErr)
  }
  RRFC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                               StdErr = rep(stdErr, choose(I, 2)), 
                               DF = rep(ddf, choose(I, 2)), 
                               t = tStat, 
                               PrGTt = PrGTt, 
                               CILower = CI[,1],
                               CIUpper = CI[,2],
                               row.names = diffTRName, 
                               stringsAsFactors = FALSE)
  
  # StdErr = sqrt[2/r * MS(T*R)]
  # DF = df[MS(T*R)] = (t-1)(r-1)
  # 95% CI: Difference +- t(.025;df) * StdErr
  # Note: If there are only 2 treatments, this is equivalent to a paired t-test applied
  # to the AUCs
  
  #   c) Single treatment AUC 95% confidence intervals
  # (Each analysis is based only on data for the specified treatment, 
  #   i.e. on the treatment-specfic reader ANOVA of AUCs
  dfSingle <- array(dim = I)
  msDenSingle <- array(dim = I)
  stdErrSingle <- array(dim = I)
  CISingle <- array(dim = c(I, 2))
  for (i in 1:I) {
    msDenSingle[i] <- ANOVA$IndividualTrt[i, "msREachTrt"]
    dfSingle[i] <- (J - 1)
    stdErrSingle[i] <- sqrt(msDenSingle[i]/J)
    CISingle[i, ] <- sort(c(trtMeans[i,1] - 
                              qt(alpha/2, dfSingle[i]) * stdErrSingle[i], trtMeans[i,1] + 
                              qt(alpha/2, dfSingle[i]) * stdErrSingle[i]))
  }
  RRFC$ciAvgRdrEachTrt <- data.frame(Estimate = trtMeans, 
                                     StdErr = as.vector(stdErrSingle), 
                                     DF = as.vector(dfSingle), 
                                     CILower = CISingle[,1], 
                                     CIUpper = CISingle[,2], 
                                     row.names = paste0("Trt", modalityID), 
                                     stringsAsFactors = FALSE)
  
  # StdErr = sqrt[1/r * MS(R)]
  # DF = df[MS(R)] = r-1
  # 95% CI: AUC +- t(.025;df) * StdErr
  # Note: this is the conventional CI, treating the reader AUCs as a random sample.
  
  return(RRFC) 
}



OR_RRRC_SP <- function(dataset, FOMs, ANOVA, alpha, diffTRName) {
  # ===========================================================================
  #   *****    Analysis 1 (OR Analysis): Random Readers and Random Cases    *****
  # ===========================================================================
  #     (Results apply to the population of readers and cases)
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  I <- length(modalityID)
  J <- length(readerID)
  
  trtMeans <-  FOMs$trtMeans
  trtMeanDiffs  <-  FOMs$trtMeanDiffs
  
  TRanova <- ANOVA$TRanova
  VarCom <- ANOVA$VarCom
  
  # a) Test for H0: Treatments have the same AUC
  msDen <- TRanova["TR","MS"] + max(J * (VarCom["Cov2",1] - VarCom["Cov3",1]), 0)
  f <- TRanova["T","MS"]/msDen
  ddf <- msDen^2/((TRanova["TR","MS"])^2/((I - 1) * (J - 1)))
  p <- 1 - pf(f, I - 1, ddf)
  RRRC <- list()
  RRRC$FTests <- data.frame(DF = c((I-1),ddf),
                            MS = c(TRanova["T", "MS"], msDen),
                            FStat = c(f,NA),
                            p = c(p,NA),
                            row.names = c("Treatment", "Error"),
                            stringsAsFactors = FALSE)
  
  # Df(error term) = [MS(T*R) + r*max(Cov2 - Cov3,0)]**2/{MS(T*R)**2/[(t-1)(r-1)]}
  # Note: "Error term" is the denominator of the F statistic and is a linear
  # combination of mean squares, as defined above.  The value of this linear 
  # combination is given under the "Mean Square" column
  # Note: Df(error term) is called "ddf_H" in Hillis (2007).
  
  #   b) 95% confidence intervals and hypothesis tests (H0: difference = 0)
  #   for treatment AUC differences
  stdErr <- sqrt(2 * msDen/J)
  tStat <- vector()
  PrGTt <- vector()
  CI <- array(dim = c(length(trtMeanDiffs[,1]), 2))
  for (i in 1:length(trtMeanDiffs[,1])) {
    tStat[i] <- trtMeanDiffs[i,1]/stdErr
    PrGTt[i] <- 2*pt(abs(tStat[i]), ddf, lower.tail = FALSE)
    ci <- sort(c(trtMeanDiffs[i,1] - qt(alpha/2, ddf) * stdErr, trtMeanDiffs[i,1] + qt(alpha/2, ddf) * stdErr))
    if (length(ci) == 0){
      CI[i, ] <- c(NA, NA)
    }else{
      CI[i, ] <- ci
    }
  }
  RRRC$ciDiffTrt <- data.frame(Estimate = trtMeanDiffs, 
                               StdErr = rep(stdErr, choose(I, 2)), 
                               DF = rep(ddf, choose(I, 2)), 
                               t = tStat, 
                               PrGTt = PrGTt, 
                               CILower = CI[,1],
                               CIUpper = CI[,2], 
                               row.names = diffTRName, 
                               stringsAsFactors = FALSE)
  
  # StdErr = sqrt{(2/r)*[MS(T*R) + r*max(Cov2 - Cov3,0)]}
  # Df same as df(error term) from (a)
  # 95% CI: Difference +- t(.025;df) * StdErr
  
  # if (dataset$descriptions$design == "FCTRL") {
  #   c) Single-treatment 95% confidence intervals
  # (Each analysis is based only on data for the specified treatment, i.e., 
  #   on the treatment-specific reader ANOVA of AUCs and Cov2 estimates.)
  df <- array(dim = I)
  msDenSingle <- array(dim = I)
  stdErr <- array(dim = I)
  CI <- array(dim = c(I, 2))
  ci <- data.frame()
  for (i in 1:I) {
    # Hillis 2007 5.3. Single test inference using only corresponding data
    msDenSingle[i] <- ANOVA$IndividualTrt[i, "msREachTrt"] + max(J * ANOVA$IndividualTrt[i, "cov2EachTrt"], 0)
    df[i] <- (msDenSingle[i])^2/(ANOVA$IndividualTrt[i, "msREachTrt"])^2 * (J - 1)
    stdErr[i] <- sqrt(msDenSingle[i]/J) # Eqn. 25
    CI[i,] <- c(trtMeans[i,1] + qt(alpha/2, df[i]) * stdErr[i], 
                trtMeans[i,1] + qt(1-alpha/2, df[i]) * stdErr[i]) # Eqn. 25
    rowName <- paste0("trt", modalityID[i])
    ci <- rbind(ci, data.frame(Estimate = trtMeans[i,1], 
                               StdErr = stdErr[i],
                               DF = df[i],
                               CILower = CI[i,1],
                               CIUpper = CI[i,2],
                               Cov2 = ANOVA$IndividualTrt[i,"cov2EachTrt"],
                               row.names = rowName,
                               stringsAsFactors = FALSE))
  }
  RRRC$ciAvgRdrEachTrt <- ci
  
  # StdErr = sqrt{1/r * [MS(R) + r*max(Cov2,0)]}
  # Df = [MS(R)+ max(r*cov2,0)]**2/[(MS(R)**2/(r-1)]
  # Note: Df is called "ddf_H_single" in Hillis (2007)
  # 95% CI: AUC +- t(.025;df) * StdErr
  
  return(RRRC)
}



convert2dataset <- function(NL, LL, LL_IL, 
                            perCase, IDs, weights,
                            fileName, type, name, truthTableStr, design,
                            modalityID, readerID) {
  ratings <- list(NL = NL,
                  LL = LL,
                  LL_IL = LL_IL)
  
  lesions <- list(perCase = perCase,
                  IDs = IDs,
                  weights = weights)
  
  descriptions <- list(
    fileName = tools::file_path_sans_ext(basename(fileName)),
    type = type,
    name = name,
    truthTableStr = truthTableStr,
    design = design,
    modalityID = modalityID,
    readerID = readerID)
  
  dataset <- list(ratings = ratings, 
                  lesions = lesions, 
                  descriptions = descriptions)
  
  return(dataset)
  
}



FOM_SP <- function(dataset, FOM = "wAFROC") { # dpc
  
  dataType <- dataset$descriptions$type
  if (dataType == "ROC" && FOM != "Wilcoxon") {
    errMsg <- paste0("Must use Wilcoxon figure of merit with ROC data.")
    stop(errMsg)
  }
  
  if (dataType == "ROI" && FOM != "ROI") {
    cat("Incorrect FOM supplied for ROI data, changing to 'ROI'\n")
    FOM <- "ROI"
  }
  
  if (!(dataType %in% c("ROC", "LROC")) && FOM == "Wilcoxon")
    stop("Cannot use `Wilcoxon` FOM with `FROC` or `ROI` data.")
  
  if (dataType != "ROI" && FOM == "ROI") {
    errMsg <- paste0("Only ROI data can be analyzed using ROI figure of merit.")
    stop(errMsg)
  }
  
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2  
  
  if (K1 == 0 && !(FOM %in% c("AFROC1", "wAFROC1"))) {
    errMsg <- paste0("Only AFROC1 or wAFROC1 FOMs are allowed for datasets with zero non-diseased cases.")
    stop(errMsg)
  }
  
  design <- dataset$descriptions$design
  t <- dataset$descriptions$truthTableStr
  
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  fomArray <- array(dim = c(I, J))
  for (i in 1:I) {
    for (j in 1:J) {
      if (design == "SPLIT-PLOT-A") {
        if (all(is.na(t[i,j,,1]))) next # if t[] for all normal   cases for selected i,j are NAs, skip 
        if (all(is.na(t[i,j,,2]))) next # if t[] for all abnormal cases for selected i,j are NAs, skip 
        k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) # see comments for SPLIT-PLOT-C
        k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K] # ditto:
        nl_ij <- NL[i, j, k1_ij_sub, ]
        perCase_ij <- dataset$lesions$perCase[k2_ij_sub]
        maxLL_ij <- max(perCase_ij)
        ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL_ij]
        k1ij <- sum(!is.na(t[i,j,,1]))
        k2ij <- sum(!is.na(t[i,j,,2]))
        lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        lW_ij <- dataset$lesions$weights[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        dim(nl_ij) <- c(k1ij+k2ij, maxNL)
        dim(ll_ij) <- c(k2ij, maxLL_ij)
        fomArray[i, j] <- MyFom_ij_SP(nl_ij, ll_ij, perCase_ij, lID_ij, lW_ij, maxNL, maxLL_ij, k1ij, k2ij, FOM)
        next
      } else if (design == "SPLIT-PLOT-C") {
        if (all(is.na(t[i,j,,1]))) next # if t[] for all normal   cases for selected i,j are NAs, skip 
        if (all(is.na(t[i,j,,2]))) next # if t[] for all abnormal cases for selected i,j are NAs, skip 
        # k1 refers to normal   case k-indices
        # k2 refers to abnormal case k-indices
        k1_ij_sub <- !is.na(t[i,j,,1]) | !is.na(t[i,j,,2]) # k1-indices of all cases meeting the i,j criteria
        k2_ij_sub <- !is.na(t[i,j,,2])[(K1+1):K] # k2-indices of all cases meeting the i,j criteria
        nl_ij <- NL[i, j, k1_ij_sub, ] # NL ratings for all cases meeting the i,j criteria
        perCase_ij <- dataset$lesions$perCase[k2_ij_sub] # perCase indices for all abnormal cases meeting the i,j criteria
        maxLL_ij <- max(perCase_ij)
        ll_ij <- LL[i, j, k2_ij_sub, 1:maxLL_ij]
        k1ij <- sum(!is.na(t[i,j,,1]))
        k2ij <- sum(!is.na(t[i,j,,2]))
        lID_ij <- dataset$lesions$IDs[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        lW_jj <- dataset$lesions$weights[k2_ij_sub,1:maxLL_ij, drop = FALSE]
        dim(nl_ij) <- c(k1ij+k2ij, maxNL)
        dim(ll_ij) <- c(k2ij, maxLL_ij)
        fomArray[i, j] <- MyFom_ij_SP(nl_ij, ll_ij, perCase_ij, lID_ij, lW_jj, maxNL, maxLL_ij, k1ij, k2ij, FOM)
        next
      } else if (design == "FCTRL"){
        nl_ij <- NL[i, j, , ]
        ll_ij <- LL[i, j, , ]
        dim(nl_ij) <- c(K, maxNL)
        dim(ll_ij) <- c(K2, maxLL)
        fomArray[i, j] <- MyFom_ij_SP(nl_ij, ll_ij, dataset$lesions$perCase, dataset$lesions$IDs, dataset$lesions$weights, maxNL, maxLL, K1, K2, FOM)
      } else stop("Incorrect design, must be SPLIT-PLOT-A, SPLIT-PLOT-C or FCTRL")
    }
  }
  
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  rownames(fomArray) <- paste("trt", sep = "", modalityID)
  colnames(fomArray) <- paste("rdr", sep = "", readerID)
  return(as.data.frame(fomArray))
} 





Pseudovalues <- function(dataset, FOM) {
  dataType <- dataset$descriptions$type
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  maxNL <- dim(NL)[4]
  maxLL <- dim(LL)[4]
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2
  
  # account for 15+ FOMs  
  if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
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
  fomArray <- FOM_SP(dataset, FOM)
  lastCase <- 0
  caseTransitions <- array(dim = J)
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
      nl_ij <- NL[i, j, k_ij_logi, 1:maxNL]; dim(nl_ij) <- c(K_ij, maxNL)
      # i.e., NL ratings for all cases meeting the i,j criteria
      ll_ij <- LL[i, j, k2_ij_logi, 1:maxLL]; dim(ll_ij) <- c(K2_ij, maxLL)
      # i.e., LL ratings for all cases meeting the i,j criteria
      
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
        for (k in 1:K1_ij) {
          # NOTATION
          # kIndxNor: case index for the 3rd dimension of normal cases, 
          # ranges from 1 to K1
          kIndxNor <- which(k1_ij_logi)[k];if (is.na(kIndxNor)) 
            stop("Indexing error in Pseudovalues")
          # FOMs defined over NORMAL cases
          nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
          llij_jk <- ll_ij;dim(llij_jk) <- c(K2_ij, maxLL)
          lV_j_jk <- perCase_ij
          lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
          lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
          if (is.na(jkFomValues[i, j, kIndxNor])) {
            jkFomValues[i, j, kIndxNor] <- 
              MyFom_ij_SP(nlij_jk, llij_jk, lV_j_jk, 
                       lID_j_jk, lW_j_jk, maxNL, maxLL, 
                       K1_ij - 1, K2_ij, FOM)
          } else stop("overwriting Pseudovalues")
          if (is.na(jkPseudoValues[i, j, kIndxNor])) {
            jkPseudoValues[i, j, kIndxNor] <- 
              fomArray[i, j] * K1_ij - jkFomValues[i, j, kIndxNor] * (K1_ij - 1)
          } else stop("overwriting Pseudovalues")
        }
      } else if (FOM %in% c("MaxLLF", "HrSe")) { 
        # FOMs defined over ABNORMAL cases
        for (k in 1:K2_ij) {
          # NOTATION
          # kIndxAbn: case index for the 3rd dimension of abormnal cases, 
          # ranges from 1 to K2
          kIndxAbn <- which(k2_ij_logi)[k];if (is.na(kIndxAbn)) 
            stop("Indexing error in Pseudovalues")
          nlij_jk <- nl_ij[-(k+K1_ij), ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
          llij_jk <- ll_ij[-k, ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
          lV_j_jk <- perCase_ij[-k]
          lW_j_jk <- lW_ij[-k, ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
          lID_j_jk <- lID_ij[-k, ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
          if (is.na(jkFomValues[i, j, kIndxAbn])) {
            jkFomValues[i, j, kIndxAbn] <- 
              MyFom_ij_SP(nlij_jk, llij_jk, lV_j_jk, 
                       lID_j_jk, lW_j_jk, maxNL, maxLL, 
                       K1_ij, K2_ij - 1, FOM)
          } else stop("overwriting Pseudovalues 3")
          if (is.na(jkPseudoValues[i, j, kIndxAbn])) {
            jkPseudoValues[i, j, kIndxAbn] <- 
              fomArray[i, j] * K2_ij - jkFomValues[i, j, kIndxAbn] * (K2_ij - 1)
          } else stop("overwriting Pseudovalues")
        }
      } else { 
        # FOMs defined over ALL cases
        for (k in 1:K_ij) {
          # NOTATION
          # kIndxAll: case index for the 3rd dimension of all cases, 
          # ranges from 1 to K
          kIndxAll <- which(k_ij_logi)[k];if (is.na(kIndxAll)) 
            stop("Indexing error in Pseudovalues")
          if (k <= K1_ij) {
            nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_ij;dim(llij_jk) <- c(K2_ij, maxLL)
            lV_j_jk <- perCase_ij
            lID_j_jk <- lID_ij;dim(lID_j_jk) <- c(K2_ij, maxLL)
            lW_j_jk <- lW_ij;dim(lW_j_jk) <- c(K2_ij, maxLL)
            if (is.na(jkFomValues[i, j, kIndxAll])) {
              jkFomValues[i, j, kIndxAll] <- 
                MyFom_ij_SP(nlij_jk, llij_jk, lV_j_jk, 
                         lID_j_jk, lW_j_jk, maxNL, maxLL, 
                         K1_ij - 1, K2_ij, FOM)
            } else stop("overwriting Pseudovalues")
            if (is.na(jkPseudoValues[i, j, kIndxAll])) {
              jkPseudoValues[i, j, kIndxAll] <- 
                fomArray[i, j] * K_ij - jkFomValues[i, j, kIndxAll] * (K_ij - 1)
            } else stop("overwriting Pseudovalues")
          } else {
            nlij_jk <- nl_ij[-k, ];dim(nlij_jk) <- c(K_ij - 1, maxNL)
            llij_jk <- ll_ij[-(k - K1_ij), ];dim(llij_jk) <- c(K2_ij - 1, maxLL)
            lV_j_jk <- perCase_ij[-(k - K1_ij)]
            lW_j_jk <- lW_ij[-(k - K1_ij), ];dim(lW_j_jk) <- c(K2_ij - 1, maxLL)
            lID_j_jk <- lID_ij[-(k - K1_ij), ];dim(lID_j_jk) <- c(K2_ij - 1, maxLL)
            if (is.na(jkFomValues[i, j, kIndxAll])) {
              jkFomValues[i, j, kIndxAll] <- 
                MyFom_ij_SP(nlij_jk, llij_jk, lV_j_jk, 
                         lID_j_jk, lW_j_jk, maxNL, maxLL, 
                         K1_ij, K2_ij - 1, FOM)
            } else stop("overwriting Pseudovalues")
            if (is.na(jkPseudoValues[i, j, kIndxAll])) {
              jkPseudoValues[i, j, kIndxAll] <- 
                fomArray[i, j] * K_ij - jkFomValues[i, j, kIndxAll] * (K_ij - 1)
            } else stop("overwriting Pseudovalues")
          }
        }
      }
      # center the pseudovalues 
      if (FOM %in% c("MaxNLF", "ExpTrnsfmSp", "HrSp")) {
        # FOMs defined over NORMAL cases
        jkPseudoValues[i, j, which(k1_ij_logi)] <- 
          jkPseudoValues[i, j, which(k1_ij_logi)] + 
          (fomArray[i, j] - mean(jkPseudoValues[i, j, which(k1_ij_logi)]))
      }  else if (FOM %in% c("MaxLLF", "HrSe")) {
        # FOMs defined over ABNORMAL cases
        jkPseudoValues[i, j, which(k2_ij_logi)] <- 
          jkPseudoValues[i, j, which(k2_ij_logi)] + 
          (fomArray[i, j] - mean(jkPseudoValues[i, j, which(k2_ij_logi)]))
      } else {
        # FOMs defined over ALL cases
        jkPseudoValues[i, j, which(k_ij_logi)] <- 
          jkPseudoValues[i, j, which(k_ij_logi)] + 
          (fomArray[i, j] - mean(jkPseudoValues[i, j, which(k_ij_logi)]))
      }
      caseTransitions[j] <- lastCase
      lastCase <- (lastCase + K_ij) %% K
    }
  }
  
  caseTransitions <- c(caseTransitions, K)
  return(list(
    jkPseudoValues = jkPseudoValues, 
    jkFomValues = jkFomValues,
    caseTransitions = caseTransitions
  ))
}




#' @importFrom stats approx
MyFom_ij_SP <- function(nl, ll, 
                     perCase, lesionID, 
                     lesionWeight, maxNL, 
                     maxLL, K1, K2, FOM) {
  if (!FOM %in% c("Wilcoxon", "HrAuc", "HrSe", "HrSp", 
                  "AFROC1", "AFROC", 
                  "wAFROC1", "wAFROC", 
                  "MaxLLF", "MaxNLF", "MaxNLFAllCases")){
    errMsg <- paste0(FOM, " is not an available figure of merit.")
    stop(errMsg)
  }
  
  fom <- NA
  fom <- switch(FOM,
                "Wilcoxon" = TrapezoidalArea(nl, K1, ll, K2),
                "HrAuc" = HrAuc(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "HrSe" = HrSe(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "HrSp" = HrSp(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "AFROC1" = AFROC1(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "AFROC" = AFROC(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "wAFROC1" = wAFROC1(nl, ll, perCase, c(K1, K2), maxNL, maxLL, lesionWeight), 
                "wAFROC" = wAFROC(nl, ll, perCase, c(K1, K2), maxNL, maxLL, lesionWeight),
                "MaxLLF" = MaxLLF(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "MaxNLF" = MaxNLF(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
                "MaxNLFAllCases" = MaxNLFAllCases(nl, ll, perCase, c(K1, K2), maxNL, maxLL),
  )
  return(fom)
} 



#' Read a split plot data file
#'
#' @description Read disk file and create a dataset object
#'
#' @param fileName String specifying the name of the file.
#'
#' @return A dataset with the structure specified in
#'   \code{\link{RJafroc-package}}.
#'
#' @examples
#' fileName <- system.file("extdata", "toyFiles/ROC/rocSpA.xlsx",
#' package = "RJafroc", mustWork = TRUE)
#' ds <- DfReadSP(fileName)
#'
#'
#' @importFrom tools file_ext
#' @importFrom stringr str_trim str_length
#' @importFrom readxl excel_sheets
#' @export

DfReadSP <- function (fileName) 
{
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  wb <- readxl::excel_sheets(fileName)   # readxl
  temp <- sort(toupper(wb))
  if (!(temp[1] %in% c("FP", "NL"))) stop("FP or NL sheet not found\n")
  if (!(temp[2] %in% c("TP", "LL"))) stop("TP or LL sheet not found\n")
  if (!(temp[3] %in% c("TRUTH"))) stop("Truth sheet not found\n")
  sheetNames <- toupper(wb) 
  
  ########################## CHECK TRUTH TABLE ##############################
  # find the position of the TRUTH worksheet
  # this way it does not matter where it is, i.e., 1st, 2nd or 3rd tab position in the workbook
  truthFileIndex <- which(!is.na(match(sheetNames, "TRUTH")))
  if (length(truthFileIndex) == 0) stop("TRUTH table worksheet cannot be found in the Excel file.")
  truthTable <- data.frame( read_xlsx(fileName, truthFileIndex, range = cell_cols(1:6) ) )
  if (length(truthTable) != 6) stop("Old Excel format file encountered; cannot use newExcelFileFormat = TRUE")
  cTT <- checkTruthTableSP(truthTable) # cTT = checkTruthTableSP
  
  truthTableSort <- cTT$truthTableSort 
  rdrArr1D <- cTT$rdrArr1D
  trtArr1D <- cTT$trtArr1D
  truthTableStr <- cTT$truthTableStr
  truthCaseID <-  cTT$caseID # these need not be unique for FROC datasets, as more than one mark is possible
  type <- cTT$type
  design <- cTT$design
  weights <- cTT$weights
  perCase <- cTT$perCase
  IDs <- cTT$IDs
  lesionIDCol <- cTT$lesionIDCol
  normalCases <- cTT$normalCases
  abnormalCases <- unique(cTT$abnormalCases)
  
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  ########################### CHECK NL TABLE ################################
  nlFileIndex <- which(!is.na(match(sheetNames, c("FP", "NL"))))
  if (length(nlFileIndex) == 0) stop("FP/NL table worksheet cannot be found in the Excel file.")
  NLTable <- data.frame(read_xlsx(fileName, nlFileIndex, range = cell_cols(1:4)))
  
  # grep "^\\s*$" matches blank lines; see learnGrep in desktop
  # grep("^\\s*$", "") = 1
  # grep("^\\s*$", c("","")) = 1 2 etc
  # following replaces empty cells with NAs
  # it is not needed as the excel read function already does that
  # for (i in 1:4){
  #   NLTable[grep("^\\s*$", NLTable[ , i]), i] <- NA
  # }
  
  naRows <- colSums(is.na(NLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      NLTable <- NLTable[1:(nrow(NLTable) - max(naRows)), ]
    }
  }
  
  for (i in 3:4) {
    if (any(is.na(as.numeric(as.character(NLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(NLTable[, i])))) + 1
      errorMsg <- paste0("Data entry error at line ", paste(naLines, collapse = ", "), " in the FP worksheet")
      stop(errorMsg)
    }
  }
  
  NLReaderIDCol <- as.character(NLTable$ReaderID)
  NLModalityIDCol <- as.character(NLTable$ModalityID)
  NLCaseIDCol <- NLTable$CaseID
  if (is.null(NLTable$FP_Rating)) NLRatingCol <- NLTable$NL_Rating else NLRatingCol <- NLTable$FP_Rating
  
  if (any(!(NLCaseIDCol %in% truthCaseID))) {
    naCases <- NLCaseIDCol[which(!(NLCaseIDCol %in% truthCaseID))]
    errorMsg <- paste0("Case(s) ", paste(unique(naCases), collapse = ", "), 
                       " in the FP table cannot be found in TRUTH table.")
    stop(errorMsg)
  }
  
  ########################### CHECK LL TABLE ################################
  llFileIndex <- which(!is.na(match(sheetNames, c("TP", "LL"))))
  if (length(llFileIndex) == 0) stop("TP/LL table worksheet cannot be found in the Excel file.")
  LLTable <- data.frame(read_xlsx(fileName, llFileIndex, range = cell_cols(1:5) ))
  
  for (i in 1:5){
    LLTable[grep("^\\s*$", LLTable[ , i]), i] <- NA
  }
  
  naRows <- colSums(is.na(LLTable))
  if (max(naRows) > 0) {
    if (max(naRows) == min(naRows)) {
      LLTable <- LLTable[1:(nrow(LLTable) - max(naRows)), ]
    }
  }
  
  for (i in 3:5) {
    if (any(is.na(as.numeric(as.character(LLTable[, i]))))) {
      naLines <- which(is.na(as.numeric(as.character(LLTable[, i])))) + 1
      errorMsg <- paste0("There are unavailable cell(s) at the line(s) ", 
                         paste(naLines, collapse = ", "), " in the TP table.")
      stop(errorMsg)
    }
  }
  
  LLReaderIDCol <- as.character(LLTable$ReaderID)
  LLModalityIDCol <- as.character(LLTable$ModalityID)
  LLCaseIDCol <- LLTable$CaseID
  LLLesionIDCol <- LLTable$LesionID
  if (is.null(LLTable$TP_Rating)) LLRatingCol <- LLTable$LL_Rating else LLRatingCol <- LLTable$TP_Rating
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseIDCol[i]) & (lesionIDCol == LLLesionIDCol[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], 
                         " Reader(s) ", LLTable[i, 1], 
                         " Case(s) ", LLTable[i, 3], 
                         " Lesion(s) ", LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  if (any(LLCaseIDCol %in% normalCases)) {
    errorMsg <- paste0("Normal case(s) found in TP table.")
    stop(errorMsg)
  }
  
  for (i in 1:nrow(LLTable)) {
    lineNum <- which((truthCaseID == LLCaseIDCol[i]) & (lesionIDCol == LLLesionIDCol[i]))
    if (!length(lineNum)) {
      errorMsg <- paste0("Modality ", LLTable[i, 2], " Reader(s) ", 
                         LLTable[i, 1], " Case(s) ", LLTable[i, 3], " Lesion(s) ", 
                         LLTable[i, 4], " cannot be found in TRUTH table .")
      stop(errorMsg)
    }
  }
  
  if (anyDuplicated(LLTable[, 1:4])) {
    naLines <- which(duplicated(LLTable[, 1:4]))
    errorMsg <- paste0("Modality ", paste(LLTable[naLines, 2], collapse = ", "), 
                       " Reader(s) ", paste(LLTable[naLines, 1], collapse = ", "), 
                       " Case(s) ", paste(LLTable[naLines, 3], collapse = ", "), " Lesion(s) ", 
                       paste(LLTable[naLines, 4], collapse = ", "), 
                       " have multiple ratings in TP table .")
    stop(errorMsg)
  }
  
  modalityIDUnique <- as.character(unique(c(NLModalityIDCol, LLModalityIDCol)))
  I <- length(modalityIDUnique)
  readerIDUnique <- as.character(unique(c(NLReaderIDCol, LLReaderIDCol)))
  # following  to preserve ordering does not work as names are prededed with Rdr
  # readerIDUnique <- as.character(sort(unique(as.integer(c(NLReaderIDCol, LLReaderIDCol)))))
  J <- length(readerIDUnique)
  
  maxNL <- 0
  for (i in modalityIDUnique) {
    for (j in readerIDUnique) {
      casePresent_ij <- (NLModalityIDCol == i) & (NLReaderIDCol == j)
      if ((sum(casePresent_ij) == 0)) 
        next
      maxNL <- max(maxNL, max(table(NLCaseIDCol[casePresent_ij])))
    }
  }
  
  L <- length(NLModalityIDCol)
  NL <- array(dim = c(I, J, K, maxNL))
  NLRatingCol <- as.numeric(NLRatingCol)
  if(any(is.na(NLRatingCol))) stop ("found NAs in NLRatingCol in NL/FP sheet")
  ############################ INIT NL ARRAY ################################
  for (l in 1:L) {
    i <- which(trtArr1D == NLModalityIDCol[l])
    j <- which(rdrArr1D == NLReaderIDCol[l])
    k <- which(unique(truthTableSort$CaseID) == NLCaseIDCol[l])
    nMatches <- which((NLCaseIDCol == NLCaseIDCol[l]) & (NLModalityIDCol == NLModalityIDCol[l]) & (NLReaderIDCol == NLReaderIDCol[l]))
    if (NLCaseIDCol[l] %in% normalCases) tt2 <- truthTableStr[i,j,k,1] else tt2 <- truthTableStr[i,j,k,2] 
    if (is.na(tt2)) stop("Error in reading NL/FP table: is.na(tt2)") else {
      if (tt2 != 1)  stop("Error in reading NL/FP table: tt2 != 1") else 
        for (el in 1:length(nMatches)) {
          # if a modality-reader-case has multiple marks, then enter the corresponding ratings
          # the is.na() check ensures that an already recorded mark is not overwritten
          # CANNOT determine el as in the LL case, see below, since the number of FROC NL marks is potentially unlimited
          # The first rating comes from l, the next from l+1, etc.
          if (is.na( NL[i, j, k, el])) NL[i, j, k, el] <- NLRatingCol[l+el-1]
        }
    }
  }
  NL[is.na(NL)] <- UNINITIALIZED
  
  ############################ INIT LL ARRAY ################################
  L <- length(LLModalityIDCol)
  LL <- array(dim = c(I, J, K2, max(perCase)))
  LLRatingCol <- as.numeric(LLRatingCol)
  if(any(is.na(LLRatingCol))) stop ("found NAs in LLRatingCol in LL/TP sheet")
  for (l in 1:L) {
    i <- which(trtArr1D == LLModalityIDCol[l])
    j <- which(rdrArr1D == LLReaderIDCol[l])
    k <- which(unique(truthTableSort$CaseID) == LLCaseIDCol[l]) - K1 # offset into abnormal cases
    # CAN determine el since the number of FROC LL marks is LIMITED to number of lesions in case
    if (K1 != 0) {
      # this gives 0,1,2,..,max num of lesions
      # which includes zero, hence the minus 1
      el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l]) - 1
    } else {
      # this gives 1,2,..,max num of lesions
      # which does not include zero, hence no minus 1
      el <- which(unique(truthTableSort$LesionID) == LLLesionIDCol[l])
    }
    tt2 <- truthTableStr[i,j,k+K1,el+1]
    if (is.na(tt2)) next else {
      if (tt2 != 1)  stop("Error in reading LL/TP table") else 
        # the is.na() check ensures that an already recorded mark is not overwritten
        if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
    }
    # if (is.na(tt2)) stop("Error in reading LL/TP table") else {
    #   if (tt2 != 1)  stop("Error in reading LL/TP table") else 
    #     # the is.na() check ensures that an already recorded mark is not overwritten
    #     if (is.na( LL[i, j, k, el])) LL[i, j, k, el] <- LLRatingCol[l]
    # }
  }
  
  LL[is.na(LL)] <- UNINITIALIZED
  weights[is.na(weights)] <- UNINITIALIZED
  lesionIDCol[is.na(lesionIDCol)] <- UNINITIALIZED
  
  if (type == "ROC" && design == "FCTRL") {
    if (!(((max(table(truthCaseID)) == 1) && (maxNL == 1)) 
          && (all((NL[, , (K1 + 1):K, ] == UNINITIALIZED))) 
          && (all((NL[, , 1:K1, ] != UNINITIALIZED)))
          && (all((LL[, , 1:K2, ] != UNINITIALIZED))))) {
      stop("This does not appear to be an ROC dataset.")
    }    
  }
  
  modalityNames <- modalityIDUnique
  readerNames <- readerIDUnique
  
  names(modalityIDUnique) <- modalityNames; modalityID <- modalityIDUnique
  names(readerIDUnique) <- readerNames; readerID <- readerIDUnique
  
  name <- NA
  if ((design == "FCTRL") || (design == "CROSSED")) design <- "FCTRL"
  
  # return the ROC or FROC dataset object
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
} 



preCheck4BadEntries <- function(truthTable) {
  
  # check for blank cells in Truth worksheet
  errorMsg <- ""
  for (i in 1:5) {
    if (any(is.na(truthTable[, i]))) {
      # each blank Excel cell is returned as NA
      # blank lines in Excel sheet are ignored i.e. skipped, as if they were not there
      naLines <- which(is.na(truthTable[, i])) + 1
      errorMsg <- paste0(errorMsg, "\nThere are empty cells at line(s) ", 
                         paste(naLines, collapse = ", "), " in the TRUTH table.")
    }
  }
  if (errorMsg != "") stop(errorMsg)
  
  for (i in 1:3)
    if (any(is.na(suppressWarnings(as.numeric(as.character(truthTable[, i])))))) {
      suppressWarnings({naLines <- which(is.na(as.numeric(as.character(truthTable[, i])))) + 1})
      if (i == 1) errorMsg <- paste0(errorMsg, 
                                     "\nThere are non-integer values(s) for caseID at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
      if (i == 2) errorMsg <- paste0(errorMsg, 
                                     "\nThere are non-integer values(s) for LessionID at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
      if (i == 3) errorMsg <- paste0(errorMsg, 
                                     "\nThere are non-numeric values(s) for Weights at line(s) ", paste(naLines, collapse = ", "), " in the TRUTH table.")
    }
  if (errorMsg != "") stop(errorMsg)
  
  if (any(!is.wholenumber(as.numeric(truthTable[[1]])))) stop("Non-integer values in Truth worksheet column 1")
  if (any(!is.wholenumber(as.numeric(truthTable[[2]])))) stop("Non-integer values in Truth worksheet column 2")
  if (any(!is.double(as.numeric(truthTable[[3]])))) stop("Non-floating point values in Truth worksheet column 3")
  
  # code to check for sequential lesionIDs in Truth sheet: 0,0,1,2,0,1,2,3,0,1 etc
  # normal case lesionIDS are all 0
  # for each abnormal case, the lesionID starts from 1 and works up, sequentially, to number of lesions on the case
  # a case can start abruptly wiht lesionID = 0 or 1, but not with lesionID = 2 or more
  # if it starts with lesionID = 2 or more, the previous one must be one less, i.e., sequential
  t <- as.numeric(truthTable$LesionID) # at this stage the cells in truthTable could be characters, 
  # which would break the following code; hence we convert to numerics; the lesionID field is convertible
  # to integers, even if entered as characters; if not there is an error in the data file
  for (k in 1:length(t)) {
    if (t[k] %in% c(0,1)) next else {
      if (t[k] != (t[k-1] + 1)) {
        errorMsg <- paste0(errorMsg, "\nNon-sequential lesionID encountered at line(s) ",
                           paste(k + 1, collapse = ", "), " in the TRUTH table.")
      }
    }
  }
  if (errorMsg != "") stop(errorMsg)
  
}



# SPLIT-PLOT-A: Reader nested within test; Hillis 2014 Table VII part (a)
# SPLIT-PLOT-C: Case nested within reader; Hillis 2014 Table VII part (c)
checkTruthTableSP <- function (truthTable) 
{
  
  preCheck4BadEntries (truthTable)
  
  type <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[1]
  design <- (toupper(truthTable[,6][which(!is.na(truthTable[,6]))]))[2]
  if (design == "CROSSED") design <- "FCTRL"
  if (!(type %in% c("FROC", "ROC", "LROC"))) stop("Unsupported data type: must be ROC, FROC or LROC.\n")
  if (!(design %in% c("FCTRL", "SPLIT-PLOT-A", "SPLIT-PLOT-C"))) stop("Study design must be FCTRL, SPLIT-PLOT-A or SPLIT-PLOT-C\n")
  
  df <- truthTable[1:5]
  df["caseLevelTruth"] <- (truthTable$LesionID > 0)
  ########################################################
  # sort the TRUTH sheet of the Excel worksheet on the lesionID field
  # this puts normal cases first, regardless of how they are entered
  ########################################################
  truthTableSort <- df[order(df$caseLevelTruth),]
  
  caseIDCol <- as.integer(truthTable$CaseID)
  # TBA need a note on use of indx, why it is not used for readerID, etc.
  lesionIDCol <- as.integer(truthTable$LesionID)
  weightsCol <- as.numeric(truthTable$Weight)
  # readerIDCol <- truthTable$ReaderID # temp for testing non-character input
  # modalityIDCol <- truthTable$ModalityID# do:
  # bug non-character input error for HUGE dataset
  readerIDCol <- as.character(truthTable$ReaderID) # bug fix to avoid non-character input error below
  modalityIDCol <- as.character(truthTable$ModalityID) # do:
  # 
  L <- length(truthTable$CaseID) # length in the Excel sheet
  for (i in 1:5) if ((length(truthTable[[i]])) != L) stop("Cols of unequal length in Truth Excel worksheet")  
  
  normalCases <- sort(unique(caseIDCol[lesionIDCol == 0]))
  abnormalCases <- sort(unique(caseIDCol[lesionIDCol > 0]))
  K1 <- length(normalCases)
  K2 <- length(abnormalCases)
  K <- (K1 + K2)
  
  if (design == "SPLIT-PLOT-A") {
    # for this design the length is twice what it needs to be
    caseIDCol <- as.integer(truthTable$CaseID)[1:(L/2)]
    # lesionIDCol <- as.integer(truthTable$LesionID)[1:(L/2)]
    weightsCol <- truthTable$Weight[1:(L/2)]
    # preserve the strings; DO NOT convert to integers
    J <-  length(strsplit(readerIDCol[1], split = ",")[[1]])
    for (el in 1:length(readerIDCol)) {
      if (length(strsplit(readerIDCol[el], split = ",")[[1]]) != J) stop("Error in ReaderID column; number of readers nested in each modality must be the same.\n")
    }
    rdrArr <- array(dim = c(L,J))
    for (el in 1:L) {
      val <- strsplit(readerIDCol[el], split = ",|\\s")[[1]]
      val <- val[val != ""]
      for (i in 1:length(val)) {
        rdrArr[el,i] <- val[i]
      }
    }
    # preserve the strings; DO NOT convert to integers
    I <- length(strsplit(modalityIDCol[1], split = ",")[[1]])
    trtArr <- array(dim = c(L,I))
    for (l in 1:L) {
      if (grep("^\\(.\\)", modalityIDCol[l]) == 1) { # match found to something like (1), i.e., one nested factor
        val <- grep("^\\(.\\)", modalityIDCol[l], value = T)
        val <- strsplit(val, split = "\\(|\\)")[[1]]
        val <- val[val != ""]
        for (i in 1:length(val)) {
          trtArr[l] <- val[i]
        }
      } else stop("Was expecting nested notation, using () brackets ...")
    }
  } else if (design == "SPLIT-PLOT-C") {
    # preserve the strings; DO NOT convert to integers
    J <- length(strsplit(readerIDCol[1], split = ",")[[1]])
    rdrArr <- array(dim = c(L,J))
    for (l in 1:L) {
      if (grep("^\\(.\\)", readerIDCol[l]) == 1) { # match found to something like (1), i.e., one nested factor
        val <- grep("^\\(.\\)", readerIDCol[l], value = T)
        val <- strsplit(val, split = "\\(|\\)")[[1]]
        val <- val[val != ""]
        for (i in 1:length(val)) {
          rdrArr[l] <- val[i]
        }
      } else stop("Was expecting nested notation, using () brackets ...")
      # preserve the strings; DO NOT convert to integers
      I <- length(strsplit(modalityIDCol[1], split = ",")[[1]])
      trtArr <- array(dim = c(L,I))
      for (l in 1:L) {
        val <- strsplit(modalityIDCol[l], split = ",|\\s")[[1]]
        val <- val[val != ""]
        for (i in 1:length(val)) {
          trtArr[l,i] <- val[i]
        }
      }
    }
  } else stop("incorrect study design: must be SPLIT-PLOT-A or SPLIT-PLOT-C")
  
  if (design == "SPLIT-PLOT-A") {
    rdrArr1D <- t(unique(rdrArr)) # rdrArr is 2-dimensional; rdrArr1D is a one-dimensional array of all the readers in the study
    rdrArr1D <- rdrArr1D[!is.na(rdrArr1D)] # this modification is needed for HYK dataset with 3 readers in one group and 4 in the other
  } else {
    if (any(is.na(rdrArr))) stop("Illegal value in ReaderID column in Truth sheet")
    rdrArr1D <- as.vector(unique(rdrArr)) # rdrArr is 2-dimensional; rdrArr1D is a one-dimensional array of all the readers in the study
  }
  if (any(is.na(trtArr))) stop("Illegal value in ModalityID column in Truth sheet")
  trtArr1D <- as.vector(unique(trtArr))
  
  I <- length(trtArr1D)
  J <- length(rdrArr1D)
  
  truthTableStr <- array(dim = c(I, J, K, max(lesionIDCol)+1)) 
  for (l in 1:L) {
    k <- which(unique(truthTableSort$CaseID) == truthTable$CaseID[l])
    el <- lesionIDCol[l] + 1
    if (design == "SPLIT-PLOT-A") {
      i <- which(unique(trtArr) == trtArr[l])
      for (j1 in 1:length(rdrArr[l,])) {
        j <- which(rdrArr1D == rdrArr[l,j1])
        truthTableStr[i, j, k, el] <- 1
      }
    }
    else if (design == "SPLIT-PLOT-C") {
      i <- which(unique(trtArr) == trtArr[l,])
      j <- which(rdrArr1D == rdrArr[l])
      truthTableStr[i, j, k, el] <- 1
    } else if (design == "FCTRL") {
      i <- which(unique(trtArr) == trtArr[l,])
      j <- which(rdrArr1D == rdrArr[l,])
      truthTableStr[i, j, k, el] <- 1
    } else stop("incorrect study design: must be SPLIT-PLOT-A or SPLIT-PLOT-C")
  }
  
  perCase <- as.vector(table(caseIDCol[caseIDCol %in% abnormalCases]))
  weights <- array(dim = c(K2, max(perCase)))
  IDs <- array(dim = c(K2, max(perCase)))
  
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  for (k2 in 1:K2) {
    k <- which(caseIDCol == abnormalCases[k2])
    IDs[k2, ] <- c(sort(lesionIDCol[k]), 
                   rep(UNINITIALIZED, max(perCase) - length(k)))
    if (all(weightsCol[k] == 0)) {
      weights[k2, 1:length(k)] <- 1/perCase[k2]
    } else {
      weights[k2, ] <- as.numeric(c(weightsCol[k][order(lesionIDCol[k])], 
                                    rep(UNINITIALIZED, max(perCase) - length(k))))
      sumWeight <- sum(weights[k2, weights[k2, ] != UNINITIALIZED])
      if (sumWeight != 1){
        if (sumWeight <= 1.01 && sumWeight >= 0.99){
          weights[k2, ] <- weights[k2, ] / sumWeight
        }else{
          errorMsg <- paste0("The sum of the weights of Case ", k2, " is not 1.")
          stop(errorMsg)
        }
      }
    }
  }
  
  return (list(
    rdrArr1D = rdrArr1D,
    trtArr1D = trtArr1D,
    truthTableSort = truthTableSort,
    truthTableStr = truthTableStr,
    type = type,
    design = design,
    caseID = caseIDCol,
    perCase = perCase,
    lesionIDCol = lesionIDCol,
    IDs = IDs,
    weights = weights,
    normalCases = normalCases,
    abnormalCases = abnormalCases
  ))
  
}

is.wholenumber <- function(x) round(x) == x



