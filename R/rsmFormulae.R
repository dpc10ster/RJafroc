# for future work
myPlot <- function(dataPoints, dashedPoints, x, y, 
                   legendPosition, legendDirection, legendJustification) {
  ret <- with(dataPoints, {
    ggplot(data = dataPoints) + 
      geom_line(aes(x = x, y = y , color = Treatment)) + 
      geom_line(data = dashedPoints, aes(x = x, y = y, color = Treatment), linetype = 2) +       
      theme(legend.position = legendPosition, legend.direction = legendDirection, 
            legend.justification = legendJustification) 
  })
  return(ret)
}



xFROC <- function(zeta, lambdaP){
  # returns NLF, the abscissa of FROC curve
  NLF <- lambdaP * (1 - pnorm(zeta))
  return(NLF)
}



yFROC <- function(zeta, mu, nuP){
  # returns LLF, the ordinate of FROC, AFROC curve
  LLF <- nuP * (1 - pnorm(zeta - mu))
  return(LLF)
}



intFROC <- function(NLF, mu, lambdaP, nuP){
  zeta <- qnorm(1 - NLF / lambdaP)
  LLF <- yFROC(zeta, mu, nuP)
  return(LLF)
}



# y_AFROC_FPF is AFROC as a function of FPF + RSM parameters
y_AFROC_FPF <- function(FPF, mu, lambdaP, nuP){
  # returns LLF, the ordinate of AFROC curve; takes FPF as the variable. 
  # AUC is calculated by integrating this function wrt FPF
  tmp <- 1 / lambdaP * log(1 - FPF) + 1
  tmp[tmp < 0] <- pnorm(-20)
  zeta <- qnorm(tmp)
  LLF <- yFROC(zeta, mu, nuP)
  return(LLF)
}



# ywAFROC_R is ordinate as a function of zeta + RSM parameters
# returns wLLF, the ordinate of wAFROC curve
# this has working C++ version with name ywAFROC
# this is only here for me to understand the C++ code
ywAFROC_R <- function(zeta, mu, nuP, lesDistr, lesWghtDistr){
  # zeta <- 0
  # fl is the fraction of cases with # lesions as in first column of lesDistr
  # the second column contains the fraction
  fl <- lesDistr[, 2] / sum(lesDistr[, 2]) # redundant normalization does not hurt
  wLLF <- 0
  for (nLesion in 1:nrow(lesDistr)){
    # outer looop sums over different numbers of lesions per case
    nLesPerCase <- lesDistr[nLesion, 1] 
    # nLesPerCase is the first element in the nLesion row of lesDistr, 
    # which is the number of lesions for this lesion distributions condition
    wLLFTmp <- 0
    for (nSuccess in 1:nLesPerCase){
      # inner loop sums over different numbers of nSuccess events 
      # appropriately weighted by the probability of that many successes
      # nSuccess is the number of successes
      # nLesPerCase is the trial size
      # the following works, but only for equal weights ?? 11/29/20
      # wLLFTmp <- wLLFTmp + sum(lesWghtDistr[nLesion, 2:(nSuccess+1)]) * dbinom(nSuccess, nLesPerCase, nuP) 
      # the next line should work for general case
      wLLFTmp <- wLLFTmp + lesWghtDistr[nLesion, nSuccess+1] * nSuccess * dbinom(nSuccess, nLesPerCase, nuP)
    }
    wLLF <- wLLF +  fl[nLesion] * wLLFTmp
  }
  return(wLLF * (1 - pnorm(zeta - mu)))
}


# y_wAFROC_FPF is wAFROC as a function of FPF + RSM parameters
y_wAFROC_FPF <- function(FPF, mu, lambdaP, nuP, lesDistr, lesWghtDistr){
  # returns wLLF, the ordinate of AFROC curve; takes FPF as the variable. 
  # AUC is calculated by integrating this function wrt FPF
  tmp <- 1 / lambdaP * log(1 - FPF) + 1
  tmp[tmp < 0] <- pnorm(-20)
  zeta <- qnorm(tmp)
  wLLF <- sapply(zeta, ywAFROC, mu = mu, nuP = nuP, lesDistr, lesWghtDistr)
  return(wLLF)
}

# 
# xROC <- function (zeta, lambdaP){
#   return (1 - exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2))))
# }
# 
# 
# xROCVect <- function(zeta, lambdaP) {
#     FPF = 1 - exp( (-lambdaP / 2) + 0.5 * lambdaP * erfcpp(zeta / sqrt(2.0)))
#   return (FPF);
# }
# 
# 
# R-only implementation of erf function
# erf_R <- function(x){
#  return (2 * pnorm(sqrt(2) * x) - 1)
# }
# 