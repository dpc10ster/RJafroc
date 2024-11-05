# Implement SP_A formulae in Hillis 2014 for balanced split plot A design
# identical number of readers in each treatment
OR_SP_A_BAL <- function(dataset, FOM, alpha, analysisOption)
{

  I <- dim(dataset$ratings$NL)[1]

  # J is the number of readers in each treatment; as clarified in Hillis
  # 2023 paper;
  J <- dim(dataset$ratings$NL)[2]/I

  # no need to have different reader names in different treatments
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID[1:J] # 11/5/25 fixed error DPC

  ##############################################################################
  # get figures of merit etc for each modality i and reader j
  theta_ij <- as.matrix(FOM_SP(dataset, FOM))
  cat("Figure of merit matrix\n")
  print(theta_ij)
  cat("\n")

  # average FOM over readers in each treatment
  theta_i_dot <- array(dim = I)
  for (i in 1:I) {
    theta_i_dot[i] <- mean(theta_ij[i,])
  }

  # average FOM over readers and treatments
  theta_dot_dot <- mean(theta_ij)

  ##############################################################################
  # get mean squares
  # msT denotes MS(T), in Hillis 2014 p 332
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (theta_i_dot[i] - theta_dot_dot)^2
  }
  msT <- msT * J/(I-1)
  cat("msT =", msT, "\n")

  # msR_i denotes MS(R) for modality i, in Hillis 2014 p 341
  msR_i <- rep(0, I)
  for (i in 1:I) {
    for (j in 1:J) {
      msR_i[i] <- msR_i[i] + (theta_ij[i,j] - mean(theta_ij[i,]))^2
    }
    msR_i[i] <- msR_i[i] / (J-1)
  }

  # msTR denotes MS(TR), in Hillis 2014 p 331
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (theta_ij[i,j] - mean(theta_ij[i,]) - mean(theta_ij[,j]) + mean(theta_ij))^2
    }
  }
  msTR <- msTR/(I-1)/(J-1)

  # In Hillis 2014, R(T) denotes reader nested within treatment
  # msR__T__ denotes MS[R(T)]
  # double underscore denotes left/right parenthesis
  # Hillis 2014, definition of MS[R(T)], last line on page 344

  # note added after reading Hillis 2023
  # This matches ???
  msR__T__ <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msR__T__ <- msR__T__ + (theta_ij[i, j] - theta_i_dot[i])^2
    }
  }
  msR__T__ <- msR__T__ / I / (J - 1)
  cat("MS(R(T)) =", msR__T__, "\n")

  ##############################################################################
  # get pseudo values
  ret1 <- PseudovaluesSP_A(dataset, FOM)


  ##############################################################################
  # get variance covariance matrix, averaged over all treatments
  ret <- FOMijk2ORVarCompSP(ret1$jkFomValues, varInflFactor = TRUE)
  Var <- ret$Var
  Cov2 <- ret$Cov2
  Cov3 <- ret$Cov3
  # setting Cov3 <- 0 makes no difference to values
  # even though Cov3 = -3.469447e-18
  cat("Cov2 =", Cov2, ", Cov3 =", Cov3, "\n")

  ##############################################################################
  # Cov2 for individual treatments
  Cov2_i <- rep(0, I)
  for (i in 1:I) {
    ret_i <- FOMjk2ORVarComp(ret1$jkFomValues[i,,], varInflFactor = TRUE)
    Cov2_i[i] <- ret_i$Cov2
  }

  ##############################################################################
  # Overall F-test
  # F_OR statistic, last equation on page 344
  den <- msR__T__ + J*max(Cov2-Cov3,0)
  F_OR <- msT/den
  cat("F_OR =", F_OR, "\n")

  # implement Eqn. 27
  df2 <- (msR__T__ +  J * max(Cov2-Cov3,0))^2 /((msR__T__^2)/(J-1)/I)
  cat("ddf_H = df2 = ", df2, "\n")

  ##############################################################################
  # p-value
  pValue <- 1 - pf(F_OR, I - 1, df2)
  cat("pValue = ", pValue, "\n")

  ##############################################################################
  # confidence intervals for difference FOM between first two treatments
  # compare first two treatments only

  stop("need to fix")
  trtMeanDiffs <- theta_i_dot[1] - theta_i_dot[2]

  # as on page 346, first para
  # el_1 = 1; el_2 = - 1
  ## Note to myself ...
  ## Hillis does not state a constraint on the l_i, namely they
  ## should sum to zero; otherwise one could use l_1 = 1_2 = 1/2 and compute the CI
  ## on the average FOM using the very same method, which does not match his result
  sq_root_V_hat <- sqrt((I / J) * (msR__T__ + J * max(Cov2-Cov3,0)))

  # following matches Hillis 2023 Eqn. 6 for balanced design
  CI <- sort(c(trtMeanDiffs - qt(alpha/2, df2) * sq_root_V_hat,
               trtMeanDiffs + qt(alpha/2, df2) * sq_root_V_hat))
  cat("CI_lower = ", CI[1], ", CI_upper = ", CI[2], "\n")

  # last equation in box on page 346, using alternate method ...
  df2_i <- array(dim = I)
  for (i in 1:I) {
    df2_i[i] <- (msR_i[i] + J * max(Cov2_i[i], 0))^2/((msR_i[i])^2/(J-1))
  }

  ##############################################################################
  # confidence interval for reader averaged FOM for each treatment
  # as on page 346, first para
  # alternative method, using only data from each modality

  ciAvgRdrEachTrt <- array(dim = c(I,2))

  # these are the formulae for V_hat_i and df2_i in the text area in the middle of page 346
  # skipping the formulae that take both modalities into account
  sqrt_V_hat_i <- array(dim = I)
  for (i in 1:I) {
    sqrt_V_hat_i[i] <-  sqrt((1 / J) * (msR_i[i] + J * max(Cov2_i[i],0)))
    ciAvgRdrEachTrt[i,] <- sort(c(theta_i_dot[i] - qt(alpha/2, df2) * sqrt_V_hat_i[i],
                                  theta_i_dot[i] + qt(alpha/2, df2) * sqrt_V_hat_i[i]))
  }

  stop("add code here")
  # return(list(
  #   FOMs = FOMs,
  #   ANOVA = ANOVA,
  #   RRRC = RRRC
  # ))

}


# unbalanced dataset
OR_SP_A_UNB <- function(dataset, FOM, alpha, analysisOption)
{

  I <- dim(dataset$ratings$NL)[1]

  # J is the number of readers in each treatment; as clarified in Hillis
  # 2023 paper;
  J <- dim(dataset$ratings$NL)[2]/I

  # no need to have different reader names in different treatments
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID[1:J/I]

  ##############################################################################
  # get figures of merit etc for each modality i and reader j
  theta_ij <- as.matrix(FOM_SP(dataset, FOM))
  cat("Figure of merit matrix\n")
  print(theta_ij)
  cat("\n")

  # average FOM over readers in each treatment
  theta_i_dot <- array(dim = I)
  for (i in 1:I) {
    theta_i_dot[i] <- mean(theta_ij[i,])
  }

  # average FOM over readers and treatments
  theta_dot_dot <- mean(theta_ij)

  ##############################################################################
  # get mean squares
  # msT denotes MS(T), in Hillis 2014 p 331
  msT <- 0
  for (i in 1:I) {
    msT <- msT + (theta_i_dot[i] - theta_dot_dot)^2
  }
  msT <- msT * J/(I-1)
  cat("msT =", msT, "\n")

  # msR_i denotes MS(R) for modality i, in Hillis 2014 p 341
  msR_i <- rep(0, I)
  for (i in 1:I) {
    for (j in 1:J) {
      msR_i[i] <- msR_i[i] + (theta_ij[i,j] - mean(theta_ij[i,]))^2
    }
    msR_i[i] <- msR_i[i] / (J-1)
  }

  # msTR denotes MS(TR), in Hillis 2014 p 331
  msTR <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msTR <- msTR + (theta_ij[i,j] - mean(theta_ij[i,]) - mean(theta_ij[,j]) + mean(theta_ij))^2
    }
  }
  msTR <- msTR/(I-1)/(J-1)

  # R(T) denotes reader nested within treatment
  # msR__T__ denotes MS[R(T)]
  # double underscore denotes left/right parenthesis
  # Hillis 2014, definition of MS[R(T)], last line on page 344

  # note added after reading Hillis 2023
  # This matches ???
  msR__T__ <- 0
  for (i in 1:I) {
    for (j in 1:J) {
      msR__T__ <- msR__T__ + (theta_ij[i, j] - theta_i_dot[i])^2
    }
  }
  msR__T__ <- msR__T__ / I / (J - 1)
  cat("MS(R(T)) =", msR__T__, "\n")

  ##############################################################################
  # get pseudo values
  ret1 <- PseudovaluesSP_A(dataset, FOM)


  ##############################################################################
  # get variance covariance matrix, averaged over all treatments
  ret <- FOMijk2ORVarComp(ret1$jkFomValues, varInflFactor = TRUE)
  Var <- ret$Var
  Cov2 <- ret$Cov2
  Cov3 <- ret$Cov3
  # setting Cov3 <- 0 makes no difference to values
  # even though Cov3 = -3.469447e-18
  cat("Cov2 =", Cov2, ", Cov3 =", Cov3, "\n")

  ##############################################################################
  # Cov2 for individual treatments
  Cov2_i <- rep(0, I)
  for (i in 1:I) {
    ret_i <- FOMjk2ORVarComp(ret1$jkFomValues[i,,], varInflFactor = TRUE)
    Cov2_i[i] <- ret_i$Cov2
  }

  ##############################################################################
  # Overall F-test
  # F_OR statistic, last equation on page 344
  den <- msR__T__ + J * max(Cov2-Cov3,0)
  F_OR <- msT/den
  cat("F_OR =", F_OR, "\n")

  # implement Eqn. 27
  df2 <- (msR__T__ +  J * max(Cov2-Cov3,0))^2 /((msR__T__^2)/(J-1)/I)
  cat("ddf_H = df2 = ", df2, "\n")

  ##############################################################################
  # p-value
  pValue <- 1 - pf(F_OR, I - 1, df2)
  cat("pValue = ", pValue, "\n")

  ##############################################################################
  # confidence intervals for difference FOM between first two treatments
  # compare first two treatments only

  trtMeanDiffs <- theta_i_dot[1] - theta_i_dot[2]

  # as on page 346, first para
  # el_1 = 1; el_2 = - 1
  ## Note to myself ...
  ## Hillis does not state a constraint on the l_i, namely they
  ## should sum to zero; otherwise one could use l_1 = 1_2 = 1/2 and compute the CI
  ## on the average FOM using the very same method, which does not match his result
  sq_root_V_hat <- sqrt((I / J) * (msR__T__ + J * max(Cov2-Cov3,0)))

  # following matches Hillis 2023 Eqn. 6 for balanced design
  CI <- sort(c(trtMeanDiffs - qt(alpha/2, df2) * sq_root_V_hat,
               trtMeanDiffs + qt(alpha/2, df2) * sq_root_V_hat))
  cat("CI_lower = ", CI[1], ", CI_upper = ", CI[2], "\n")

  # last equation in box on page 346, using alternate method ...
  df2_i <- array(dim = I)
  for (i in 1:I) {
    df2_i[i] <- (msR_i[i] + J * max(Cov2_i[i], 0))^2/((msR_i[i])^2/(J-1))
  }

  ##############################################################################
  # confidence interval for reader averaged FOM for each treatment
  # as on page 346, first para
  # alternative method, using only data from each modality

  ciAvgRdrEachTrt <- array(dim = c(I,2))

  # these are the formulae for V_hat_i and df2_i in the text area in the middle of page 346
  # skipping the formulae that take both modalities into account
  sqrt_V_hat_i <- array(dim = I)
  for (i in 1:I) {
    sqrt_V_hat_i[i] <-  sqrt((1 / J) * (msR_i[i] + J * max(Cov2_i[i],0)))
    ciAvgRdrEachTrt[i,] <- sort(c(theta_i_dot[i] - qt(alpha/2, df2) * sqrt_V_hat_i[i],
                                  theta_i_dot[i] + qt(alpha/2, df2) * sqrt_V_hat_i[i]))
  }

  stop("add code here")
  # return(list(
  #   FOMs = FOMs,
  #   ANOVA = ANOVA,
  #   RRRC = RRRC
  # ))

}
