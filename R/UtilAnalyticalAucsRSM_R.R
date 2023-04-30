UtilAnalyticalAucsRSM_R <- function (mu, lambda, nu, zeta1 = -Inf, lesDistr, relWeights = 0){
  
  maxLL <- length(lesDistr)
  lesWghtDistr <- UtilLesWghtsLD(UtilLesDistr(lesDistr), relWeights)
  
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")

  if (missing(lesDistr)){
    lesDistr <- 1
  } 
  
  aucwAFROC <- aucAFROC <- aucROC <- rep(NA, length(mu))
  
  maxFPF <- xROC(zeta1, lambda)
  maxTPF <- yROC(zeta1, mu, lambda, nu, lesDistr)
  x <- integrate(y_ROC_FPF, 0, maxFPF, mu = mu, lambda = lambda, nu = nu, lesDistr = lesDistr)$value
  aucROC <- x + (1 + maxTPF) * (1 - maxFPF) / 2
  
  maxLLF <- RSM_LLF(zeta1, mu, nu)
  # y_AFROC_FPF does not call any Cpp functions
  x <- integrate(y_AFROC_FPF, 0, maxFPF, mu = mu, lambda = lambda, nu = nu)$value
  aucAFROC <- x + (1 + maxLLF) * (1 - maxFPF) / 2

 
  # dpc 01/05/22 these comments were added while converting code to formula for chapter 21-optim-op-point
  # needed formula for wAFROC ordinate, I know this is crazy, Einstein would never have done it this way :(
  # finished see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
  # checked from Console that following two give same results with following code
  # UtilAnalyticalAucsRSM(mu = 2, lambda = 1, nu = 0.9, zeta1 = -3, 
  # lesDistr = c(0.1, 0.4, 0.4, 0.1), relWeights =  c(0.2, 0.3, 0.1, 0.5))
  # $aucROC
  # [1] 0.9698827
  # $aucAFROC
  # [1] 0.8566404
  # $aucwAFROC
  # [1] 0.8448053
  # following is R implementation
  # see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
  # see test_RSM-formulae.R
  # contextStr <- "testing weights code with max 4 lesions per case: Cpp vs R"
  # contextStr <- "testing weights code with max 4 lesions per case, random values: Cpp vs R"
  # contextStr <- "testing weights code with max 10 lesions per case, random values: Cpp vs R"
  maxwLLF <- RSM_wLLF_R(zeta1, mu, nu, lesDistr, relWeights)
  x <- integrate(y_wAFROC_FPF_R, 0, maxFPF, mu = mu, lambda = lambda, nu = nu, lesDistr, relWeights)$value
  aucwAFROC <- x + (1 + maxwLLF) * (1 - maxFPF) / 2
  
  return(list(
    aucROC = aucROC,
    aucAFROC = aucAFROC,
    aucwAFROC = aucwAFROC
  ))
}

