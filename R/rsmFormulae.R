#' RSM predicted ROC-rating pdf for diseased cases
#' @param z The value at which to evaluate the pdf.
#' @param mu The mu parameter of the RSM.
#' @param lambdaP The lambdaP parameter of the RSM. 
#' @param nuP The nuP parameter of the RSM.
#' @param lesDistr The lesion distribution 1D vector.
#' 
#' @return pdf
#' 
#' @examples 
#' lesDistr <- c(0.5, 0.5)
#' RSM_pdfD(1,1,1,0.5, lesDistr)
#' lesDistr <- c(0.2, 0.3, 0.5)
#' RSM_pdfD(1,1,1,0.5, lesDistr)
#' 
#' @export

# this was the original form, simplified somewhat but otherwise
# identical to the 2017 version
RSM_pdfD <- function(z, mu, lambdaP, nuP, lesDistr){
  pdf <- 0
  for (L in 1:length(lesDistr)){
    a <- 1-nuP/2+nuP/2*erfVect((z-mu)/sqrt(2))
    b <- exp((-lambdaP/2)+lambdaP/2*erfVect(z/sqrt(2)))
    pdf <- pdf +
      # This is identical to the pdfD2 form below obtained using Maple, only the factoring is different
      lesDistr[L]*((a^(L-1)*b)*(L*nuP/sqrt(2*pi)*exp(-(z-mu)^2/2)+a*lambdaP/sqrt(2*pi)*exp(-z^2/2)))
  }
  # the following two checks are included to test out another simplification of the Maple generated
  # formulas, as well as one using calculus to get at the final expression (derivative of 1-TPF wrt
  # z)
  # the max is needed as this function works with an input vector for z
  if (abs(max(pdf - pdfD2(z, mu, lambdaP, nuP, lesDistr))) > 1e-16) stop("Two forms disagree A")
  if (abs(max(pdf - pdfD3(z, mu, lambdaP, nuP, lesDistr))) > 1e-16) stop("Two forms disagree A")
  
  return (pdf)
}


#' RSM predicted ROC-rating pdf for non-diseased cases
#' @param z The value at which to evaluate the pdf.
#' @param lambdaP The lambdaP parameter of the RSM. 
#' 
#' @return pdf
#' 
#' @examples 
#' RSM_pdfN(1,1)
#' 
#' @export
#' 
#' 
RSM_pdfN <- function(z, lambdaP){
  # verified using RSM_pdfN(1, 1) and (xROC(1, 1) - xROC(1 + 1e-8, 1)) / 1e-8
  # following expression is identical to book equation 17.21
  return(lambdaP * exp(-z^2/2) * exp(-lambdaP/2 * (1 - erfVect(z/sqrt(2)))) / sqrt(2 * pi))
}



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



# added 12/22/20
# alternate form using Maple, Dec 2020
pdfD2 <- function(z, mu, lambdaP, nuP, lesDistr){
  pdf <- 0
  for (L in 1:length(lesDistr)){
    a <- 1-nuP/2+nuP/2*erfVect((z-mu)/sqrt(2))
    b <- exp((-lambdaP/2)+lambdaP/2*erfVect(z/sqrt(2)))
    pdf <- pdf + 
      lesDistr[L]*((a^(L-1))*L*nuP*exp(-(z-mu)^2/2)+a^L*lambdaP*exp(-z^2/2))*b/sqrt(2*pi)
  }
  return (pdf)
}



# A is first term on rhs of book 17.22
A <- function(mu,nu,z,L) 
{
  return((1-nu/2+nu/2*erfVect((z-mu)/sqrt(2)))^L)
}

# B is second term on rhs of book 17.22
B <- function(lambda,z) 
{
  return(exp(-lambda/2+lambda/2*erfVect(z/sqrt(2))))
}


#dA is deriv. of A wrt z, Maple generated
dA <- function(mu,nu,z,L)
{
  return((1-nu/2+nu/2*erfVect((z-mu)/sqrt(2)))^(L-1)*L*nu*exp(-(z-mu)^2/2)/sqrt(2*pi))
}


#dB is deriv. of B wrt z, Maple generated
dB <- function(lambda,z)
{
  return(lambda*exp(-z^2/2)*exp(-lambda/2+lambda/2*erfVect(z/sqrt(2)))/sqrt(2*pi))
}


# added 12/22/20
# using Maple generated derivatives wrt z
pdfD3 <- function(z, mu, lambdaP, nuP, lesDistr){
  pdf <- 0
  for (L in 1:length(lesDistr)){
    # AB is the two terms in book 17.22
    # dA is deriv. of A wrt z
    # dB is deriv. of B wrt z
    pdf <- pdf +
      lesDistr[L]*(dA(mu,nuP,z,L)*B(lambdaP,z)+A(mu,nuP,z,L)*dB(lambdaP,z))
  }
  return (pdf)
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