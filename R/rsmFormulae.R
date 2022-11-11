#' RSM predicted ROC-rating pdf for diseased cases
#' @param z The z-vector at which to evaluate the pdf.
#' @param mu The scalar RSM mu parameter.
#' @param lambda The scalar RSM lambda parameter. 
#' @param nu The scalar RSM nu parameter.
#' @param lesDistr The lesion distribution 1D vector.
#' 
#' @return pdf, density function for diseased cases
#' 
#' @examples 
#' RSM_pdfD(c(1,2),1,1,0.9, c(0.5, 0.5))
#' RSM_pdfD(c(1,2),1,1,0.5, c(0.2, 0.3, 0.5))
#' 
#' @export

# this was the original form, simplified somewhat but otherwise
# identical to the 2017 version
RSM_pdfD <- function(z, mu, lambda, nu, lesDistr){
  
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  pdf <- 0
  for (L in 1:length(lesDistr)){
    a <- 1-nu/2+nu/2*erfVect((z-mu)/sqrt(2))
    b <- exp((-lambda/2)+lambda/2*erfVect(z/sqrt(2)))
    pdf <- pdf +
      # This is identical to the pdfD2 form below obtained using Maple, only the factoring is different
      lesDistr[L]*((a^(L-1)*b)*(L*nu/sqrt(2*pi)*exp(-(z-mu)^2/2)+a*lambda/sqrt(2*pi)*exp(-z^2/2)))
  }
  # the following two checks are included to test out another simplification of the Maple generated
  # formulas, as well as one using calculus to get at the final expression (derivative of 1-TPF wrt
  # z)
  # the max(pdf ...) is needed as this function works with an input vector for z; but this causes occasional failures
  # replaced max(pdf ...) with mean(pdf ...); median(pdf ...) might be even more resistant to outliers
  # 4/2/21 changed stop criterion from 1e-16 to 1e-15 
  if (abs(mean(pdf - pdfD2(z, mu, lambda, nu, lesDistr))) > 1e-15) stop("Two forms disagree A")
  if (abs(mean(pdf - pdfD3(z, mu, lambda, nu, lesDistr))) > 1e-15) stop("Two forms disagree B")
  
  return (pdf)
}


#' RSM predicted ROC-rating pdf for non-diseased cases
#' @param z The z-vector at which to evaluate the pdf.
#' @param lambda The scalar RSM  lambda parameter. 
#' 
#' @return pdf, density function for non-diseased cases
#' 
#' @examples 
#' RSM_pdfN(c(1,2),1)
#' 
#' @export
#' 
#' 
RSM_pdfN <- function(z, lambda){

  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  # verified using RSM_pdfN(1, 1) and (xROC(1, 1) - xROC(1 + 1e-8, 1)) / 1e-8
  # following expression is identical to book equation 17.21
  return(lambda * exp(-z^2/2) * exp(-lambda/2 * (1 - erfVect(z/sqrt(2)))) / sqrt(2 * pi))
}


################################################################################
################################################################################

#' RSM predicted FROC abscissa
#' @param z The z-vector at which to evaluate the FROC abscissa.
#' @param lambda The scalar RSM lambda parameter. 
#' 
#' @return NLF, the abscissa of the FROC curve
#' 
#' @examples 
#' RSM_NLF(c(1,2),1)
#' 
#' @export
#' 
#' 

RSM_NLF <- function(z, lambda){
  # returns NLF, the abscissa of FROC curve
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  NLF <- lambda * (1 - pnorm(z))
  return(NLF)
}


################################################################################
################################################################################


#' RSM predicted FROC ordinate
#' @param z The z-vector value at which to evaluate the FROC ordinate.
#' @param mu The scalar RSM mu parameter. 
#' @param nu The scalar RSM nu prime parameter. 
#' 
#' @return LLF, the ordinate of the FROC curve
#' 
#' @examples 
#' RSM_LLF(c(1,2),1,0.5)
#' 
#' @export
#' 
#' 

RSM_LLF <- function(z, mu, nu){
  # bug fix 12/26/21
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  # returns LLF, the ordinate of FROC, AFROC curve
  LLF <- nu * (1 - pnorm(z - mu))
  return(LLF)
}


################################################################################
################################################################################


integrandFROC <- function(NLF, mu, lambda, nu){
  
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  zeta <- qnorm(1 - NLF / lambda)
  LLF <- RSM_LLF(zeta, mu, nu) 
  return(LLF)
}


################################################################################
################################################################################


# y_AFROC_FPF is AFROC as a function of FPF + RSM parameters
# this function does not call any Cpp code
y_AFROC_FPF <- function(FPF, mu, lambda, nu){
  # returns LLF, the ordinate of AFROC curve; takes FPF as the variable. 
  # AUC is calculated by integrating this function wrt FPF
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  tmp <- 1 / lambda * log(1 - FPF) + 1
  tmp[tmp < 0] <- pnorm(-20)
  zeta <- qnorm(tmp)
  LLF <- RSM_LLF(zeta, mu, nu)
  return(LLF)
}



################################################################################
################################################################################


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

#' RSM predicted wAFROC ordinate
#' @param zeta The zeta-vector at which to evaluate the FROC ordinate.
#' @param mu The scalar RSM mu parameter. 
#' @param nu The scalar RSM nu prime parameter. 
#' @param lesDistr Lesion distribution 1D vector.
#' @param relWeights Relative lesion weights 1D vector.
#' 
#' @return wLLF, the ordinate of the wAFROC curve
#' 
#' @examples 
#' RSM_wLLF(c(1,2),1,0.5, lesDistr = c(0.5, 0.4, 0.1), relWeights = c(0.7, 0.2, 0.1)) 
#'
#' 
#' @export
#' 
#' 

# RSM_wLLF is ordinate as a function of zeta + RSM parameters
# returns wLLF, the ordinate of wAFROC curve
# this has working C++ version with name ywAFROC
# this is only here for me to understand the C++ code
RSM_wLLF <- function(zeta, mu, nu, lesDistr, relWeights){
  lesWghtDistr <- UtilLesionWeightsMatrixLesDistr(lesDistr, relWeights)
  # zeta <- 0
  # fl is the fraction of cases with # lesions as in first column of lesDistr
  # the second column contains the fraction
  # bug fix 12/26/21
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  maxLes <- length(lesDistr)
  
  wLLF <- 0
  for (nLesion in 1:maxLes){
    # outer looop sums over different numbers of lesions per case
    wLLFTmp <- 0
    for (nSuccess in 1:nLesion){
      # inner loop sums over different numbers of nSuccess events 
      # appropriately weighted by the probability of that many successes
      # nSuccess is the number of successes
      # nLesion is the trial size
      # the following works, but only for equal weights ?? 11/29/20
      # wLLFTmp <- wLLFTmp + sum(lesWghtDistr[nLesion, 2:(nSuccess+1)]) * dbinom(nSuccess, nLesion, nu) 
      # the next line should work for general case
      wLLFTmp <- wLLFTmp + lesWghtDistr[nLesion, nSuccess+1] * nSuccess * dbinom(nSuccess, nLesion, nu)
      # see RJafrocFrocBook, search for rsm-pred-wafroc-curve 1/7/22
    }
    wLLF <- wLLF +  lesDistr[nLesion] * wLLFTmp
  }
  return(wLLF * (1 - pnorm(zeta - mu)))
}


################################################################################
################################################################################


#' RSM predicted ROC-abscissa as function of z
#' @param z The z-vector at which to evaluate the ROC-abscissa.
#' @param lambda The scalar RSM lambda parameter. 
#' 
#' @return FPF, the abscissa of the ROC
#' 
#' @examples 
#' RSM_FPF(c(-Inf,0.1,0.2,0.3),1)
#' 
#' @export

RSM_FPF <- function(z, lambda) {
  
  if (lambda < 0) stop("Incorrect value for lambda\n")
  
  return(xROCVect(z, lambda))

}

#' RSM predicted ROC-ordinate as function of z
#' 
#' @param z The z-vector at which to evaluate the pdf.
#' @param mu The scalar RSM  mu  parameter.
#' @param lambda The scalar RSM  lambda parameter. 
#' @param nu The scalar nu parameter.
#' @param lesDistr The lesion distribution 1D vector.
#' 
#' @return TPF, the ordinate of the ROC
#' 
#' @examples 
#' lesDistr <- c(0.1,0.3,0.6)
#' RSM_TPF(c(-Inf,0.1,0.2,0.3), 1, 1, 0.9, lesDistr)
#' 
#' @export


RSM_TPF <- function(z, mu, lambda, nu, lesDistr) {
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  return(yROCVect(z, mu, lambda, nu, lesDistr))
}


################################################################################
################################################################################


# y_wAFROC_FPF is wAFROC as a function of FPF + RSM parameters
y_wAFROC_FPF <- function(FPF, mu, lambda, nu, lesDistr, relWeights){
  lesWghtDistr <- UtilLesionWeightsMatrixLesDistr(lesDistr, relWeights)
  # returns wLLF, the ordinate of AFROC curve; takes FPF as the variable. 
  # AUC is calculated by integrating this function wrt FPF
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  tmp <- 1 / lambda * log(1 - FPF) + 1
  tmp[tmp < 0] <- pnorm(-20)
  zeta <- qnorm(tmp)
  # Cpp code
  wLLF <- sapply(zeta, ywAFROC, mu = mu, nu = nu, lesDistr, lesWghtDistr)
  return(wLLF)
}


# y_wAFROC_FPF_R is wAFROC as a function of FPF + RSM parameters
y_wAFROC_FPF_R <- function(FPF, mu, lambda, nu, lesDistr, relWeights){
  # returns wLLF, the ordinate of AFROC curve; takes FPF as the variable. 
  # AUC is calculated by integrating this function wrt FPF
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  tmp <- 1 / lambda * log(1 - FPF) + 1
  tmp[tmp < 0] <- pnorm(-20)
  zeta <- qnorm(tmp)
  # R code
  wLLF <- sapply(zeta, RSM_wLLF, mu = mu, nu = nu, lesDistr, relWeights)
  return(wLLF)
}



# error function
RSM_erf <- function (x) {
  
  return(erfVect(x)) # this implements the commented formula below
  # return(2 * pnorm(sqrt(2) * x) - 1)
  
}




# 
# xROC <- function (zeta, lambda){
#   return (1 - exp( (-lambda / 2) + 0.5 * lambda * erfcpp(zeta / sqrt(2))))
# }
# 
# 
# xROCVect <- function(zeta, lambda) {
#     FPF = 1 - exp( (-lambda / 2) + 0.5 * lambda * erfcpp(zeta / sqrt(2.0)))
#   return (FPF);
# }
# 
# 
# R-only implementation of erf function
# erf_R <- function(x){
#  return (2 * pnorm(sqrt(2) * x) - 1)
# }
# 




# added 12/22/20
# alternate form using Maple, Dec 2020
pdfD2 <- function(z, mu, lambda, nu, lesDistr){
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  pdf <- 0
  for (L in 1:length(lesDistr)){
    a <- 1-nu/2+nu/2*erfVect((z-mu)/sqrt(2))
    b <- exp((-lambda/2)+lambda/2*erfVect(z/sqrt(2)))
    pdf <- pdf + 
      lesDistr[L]*((a^(L-1))*L*nu*exp(-(z-mu)^2/2)+a^L*lambda*exp(-z^2/2))*b/sqrt(2*pi)
  }
  return (pdf)
}



# A is first term on rhs of book 17.22
A <- function(mu,nu,z,L) 
{
  
  # bug fix 12/26/21
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  return((1-nu/2+nu/2*erfVect((z-mu)/sqrt(2)))^L)
}

# B is second term on rhs of book 17.22
B <- function(lambda,z) 
{
  
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  
  return(exp(-lambda/2+lambda/2*erfVect(z/sqrt(2))))
}


#dA is deriv. of A wrt z, Maple generated
dA <- function(mu,nu,z,L)
{
  # bug fix 12/26/21
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  return((1-nu/2+nu/2*erfVect((z-mu)/sqrt(2)))^(L-1)*L*nu*exp(-(z-mu)^2/2)/sqrt(2*pi))
}


#dB is deriv. of B wrt z, Maple generated
dB <- function(lambda,z)
{
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  
  return(lambda*exp(-z^2/2)*exp(-lambda/2+lambda/2*erfVect(z/sqrt(2)))/sqrt(2*pi))
}


# added 12/22/20
# using Maple generated derivatives wrt z
pdfD3 <- function(z, mu, lambda, nu, lesDistr){
  
  # bug fix 12/26/21
  if (lambda < 0) stop("Incorrect value for lambda\n")
  if (nu < 0) stop("Incorrect value for nu\n")
  if (nu > 1) stop("Incorrect value for nu\n")
  
  pdf <- 0
  for (L in 1:length(lesDistr)){
    # AB is the two terms in book 17.22
    # dA is deriv. of A wrt z
    # dB is deriv. of B wrt z
    pdf <- pdf +
      lesDistr[L]*(dA(mu,nu,z,L)*B(lambda,z)+A(mu,nu,z,L)*dB(lambda,z))
  }
  return (pdf)
}


################################################################################


# yROCVect_R <- function (zeta, mu, lambda, nu, lesDistr){
# 
#   TPF <- array(dim = length(zeta))
#   for (il  in 1:length(zeta)){
#     for (i in 1:length(lesDistr)){
#       TPF[il] = TPF[il] + lesDistr[i] * 
#         (1 - pow(1 - nu/2 + nu/2  * erfcpp( (zeta[il] - mu) / sqrt(2.0) ) , (i+1)) * exp( (-lambda / 2) + 0.5 * lambda * erfcpp(zeta[il] / sqrt(2.0))))
#     }
#   }
#   
#   return (TPF)
# }


# y_ROC_FPF_R <- function (FPF, mu, lambda, nu, lesDistr){
#   for (il in 1:length(FPF)){
#     temp = (1 / lambda) * log(1 - FPF[il]) + 1;
#     if (temp <= 0){
#       zeta[il] = -20;
#     }else{
#       zeta[il] = R::qnorm(temp, 0, 1, 1, 0);
#     }
#   }
# 
#   return (yROCVect(zeta, mu, lambda, nu, lesDistr))
# }

