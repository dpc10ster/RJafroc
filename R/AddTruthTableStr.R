# moved from several locations to new function 9/16/2023
AddTruthTableStr <- function(dataset, type, perCase) {

  I <- dim(dataset$ratings$NL)[1]
  J <- dim(dataset$ratings$NL)[2]
  K <- dim(dataset$ratings$NL)[3]
  K2 <- dim(dataset$ratings$LL)[3]
  K1 <- K - K2
  maxLL <- dim(dataset$ratings$LL)[4]

  if (type == "FROC") {
    # added truthTableStr 5/18/2023
    truthTableStr <- array(dim = c(I, J, K, maxLL+1))
    truthTableStr[,,1:K1,1] <- 1
    for (k2 in 1:K2) {
      truthTableStr[,,k2+K1,(1:perCase[k2])+1] <- 1
    }
  } else if (type == "ROC") {
    # added truthTableStr 5/18/2023
    truthTableStr <- array(dim = c(I, J, K, 2)) 
    truthTableStr[1:I, 1:J, 1:K1, 1] <- 1
    truthTableStr[1:I, 1:J, (K1+1):K, 2] <- 1
  } else if (type == "LROC") {
    # added truthTableStr 7/26/2024
    truthTableStr <- array(dim = c(I, J, K, 2)) 
    truthTableStr[1:I, 1:J, 1:K1, 1] <- 1
    truthTableStr[1:I, 1:J, (K1+1):K, 2] <- 1
  } else stop("data type must be ROC or FROC or LROC")
  
  return(truthTableStr)
}
