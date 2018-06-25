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