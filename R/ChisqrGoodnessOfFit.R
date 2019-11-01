#' Compute the chisquare goodness of fit statistic for ROC fitting model
#'
#' @description Compute the chisquare goodness of fit statistic for specified ROC data fitting model
#'
#'
#' @param fpCounts The FP counts table
#' @param tpCounts The TP counts table
#' @param parameters The parameters of the model including cutoffs, see details
#' @param model The fitting model: "BINORMAL", "CBM" or "RSM
#' @param lesDistr The lesion distribution matrix; not needed for "BINORMAL" 
#'    or "CBM" models. Array [1:maxLL,1:2]. The probability mass function of the 
#'    lesion distribution for diseased cases. The first column contains the 
#'    actual numbers of lesions per case. 
#'    The second column contains the fraction of diseased cases with the number 
#'    of lesions specified in the first column. 
#'    The second column must sum to unity. 
#'
#'
#' @return The return value is a list with the following elements:
#' @return \item{chisq}{The chi-square statistic}
#' @return \item{pVal}{The p-value of the fit}
#' @return \item{df}{The degrees of freedom}
#' 
#'
#' @details
#' For model = "BINORMAL" the parameters are c(a,b,zetas).
#' For model = "CBM" the parameters are c(mu,alpha,zetas).
#' For model = "RSM" the parameters are c(mu,lambdaP,nuP,zetas).
#'
#'
#' @importFrom stats pchisq

# general code replaces three functions; dpc 10/27/18
ChisqrGoodnessOfFit <- function(fpCounts, tpCounts, parameters, model,lesDistr) {
  if (model == "BINORMAL") {
    a <- parameters[1]
    b <- parameters[2]
    zetas <- parameters[3:length(parameters)]
    fpf1 <- pnorm(-zetas/b)
    tpf1 <-  pnorm(a-zetas)
    minDfVal <- 3
  } else if (model == "CBM") {
    mu <- parameters[1]
    alpha <- parameters[2]
    zetas <- parameters[3:length(parameters)]
    fpf1 <- pnorm(-zetas)
    tpf1 <- (1 - alpha) * pnorm(-zetas) + alpha * pnorm(mu-zetas)
    minDfVal <- 3
  } else if (model == "RSM") {
    mu <- parameters[1]
    lambdaP <- parameters[2]
    nuP <- parameters[3]
    zetas <- parameters[4:length(parameters)]
    fpf1 <- xROCVect(zetas, lambdaP)
    tpf1 <- yROCVect(zetas, mu, lambdaP, nuP, lesDistr)
    minDfVal <- 4
  } else stop("Incorrect model parameter in ChisqrGoodnessOfFit")
  
  fpExpProb <- c(1, fpf1) - c(fpf1, 0)
  tpExpProb <- c(1, tpf1) - c(tpf1, 0)
  
  retComb1 <- CombBins(rbind(fpCounts, tpCounts), rbind(fpExpProb, tpExpProb))
  retComb1 <- CombBins(retComb1$obs[c(2, 1), , drop = FALSE], retComb1$prob[c(2, 1), , drop = FALSE])
  obs1 <- retComb1$obs[c(2, 1), , drop = FALSE]; exp1 <- retComb1$prob[c(2, 1), , drop = FALSE] * rowSums(obs1)
  fpObsExp1 <- rbind(obs1[1, ], exp1[1, ])
  tpObsExp1 <- rbind(obs1[2, ], exp1[2, ])
  
  retComb2 <- CombBins(rbind(tpCounts, fpCounts), rbind(tpExpProb, fpExpProb))
  retComb2 <- CombBins(retComb2$obs[c(2, 1), , drop = FALSE], retComb2$prob[c(2, 1), , drop = FALSE])
  obs2 <- retComb2$obs; exp2 <- retComb2$prob * rowSums(obs2)
  fpObsExp2 <- rbind(obs2[1, ], exp2[1, ])
  tpObsExp2 <- rbind(obs2[2, ], exp2[2, ])
  
  if (ncol(fpObsExp1) >= ncol(fpObsExp2)){
    fpGoodness <- fpObsExp1
    tpGoodness <- tpObsExp1
    nBinsComb <- ncol(fpObsExp1)
  }else{
    fpGoodness <- fpObsExp2
    tpGoodness <- tpObsExp2
    nBinsComb <- ncol(fpObsExp2)
  }
  
  if (nBinsComb > minDfVal){
    chisq <- sum((fpGoodness[1, ] - fpGoodness[2, ])^2/fpGoodness[2, ]) + 
      sum((tpGoodness[1, ] - tpGoodness[2, ])^2/tpGoodness[2, ])
    df <- nBinsComb - minDfVal
    pVal <- pchisq(chisq, df, lower.tail = FALSE)
  }else{
    chisq <- NA
    pVal <- NA
    df <- NA
  }
  
  return(list(
    chisq = chisq,
    pVal = pVal,
    df = df
  ))
}


# this code was inspired by XZ
CombBins <- function(binned, prob){
  if (ncol(binned) > 1){
    less5Indx <- which(binned[1, ] < 5)
    if (length(less5Indx) > 0){
      less5 <- binned[, less5Indx, drop = FALSE]
      less5Prob <- prob[, less5Indx, drop = FALSE]
      binned <- binned[, -less5Indx, drop = FALSE]
      prob <- prob[, -less5Indx, drop = FALSE]
      while (sum(less5[1, ]) >= 5){
        if (sum(less5[1, ]) == 5){
          binned <- cbind(binned, rowSums(less5))
          prob <- cbind(prob, rowSums(less5Prob))
          less5 <- numeric(0)
          less5Prob <- numeric(0)
          break
        }else{
          maxIndx <- which.max(less5[1, ])
          tempBin <- less5[, maxIndx, drop = FALSE]
          tempProb <- less5Prob[, maxIndx, drop = FALSE]
          less5 <- less5[, -maxIndx, drop = FALSE]
          less5Prob <- less5Prob[, -maxIndx, drop = FALSE]
          while (tempBin[1, ] < 5){
            minIndx <- which.min(less5[1, ])
            tempBin <- tempBin + less5[, minIndx, drop = FALSE]
            tempProb <- tempProb + less5Prob[, minIndx, drop = FALSE]
            less5 <- less5[, -minIndx, drop = FALSE]
            less5Prob <- less5Prob[, -minIndx, drop = FALSE]
          }
          binned <- cbind(binned, tempBin)
          prob <- cbind(prob, tempProb)
        }
      }
      if (length(less5) > 0){
        minIndx <- which.min(binned[1, ])
        binned[ , minIndx] <- binned[ , minIndx] + rowSums(less5)
        prob[ , minIndx] <- prob[ , minIndx] + rowSums(less5Prob)
      }
    }
  }
  return(list(
    obs = binned,
    prob = prob
  ))
}

