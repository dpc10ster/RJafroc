isDataDegenerate <-  function (fpf, tpf) {
  
  ret <- rep(FALSE, length(fpf))
  for (i in 1:length(fpf)){
    if ((fpf[i] == 0) || (tpf[i] == 0) || (fpf[i] == 1) || (tpf[i] == 1)) ret[i] <- TRUE
  }
  if (all(ret)) return (TRUE) else return (FALSE)
}




UtilBinCountsOpPts <- function(dataset, trt = 1, rdr = 1)
{
  ret <- UtilExtractDataStructure(dataset)
  NL <- ret$NL;LL <- ret$LL;J <- ret$J;K1 <- ret$K1;K2 <- ret$K2
  
  fp <- NL[trt,rdr,1:K1,,drop = TRUE] 
  tp <- LL[trt,rdr,,,drop = TRUE]
  
  bins <- sort(unique(c(fp,tp)))
  nBins <- length(bins)
  
  fpCounts <- array(0, dim = nBins)
  tpCounts <- array(0, dim = nBins)
  
  for (b in 1:nBins){
    fpCounts[b] <- sum(fp == bins[b])
    tpCounts[b] <- sum(tp == bins[b])
  }
  
  fpf <- cumsum(rev(fpCounts)) / K1
  tpf <- cumsum(rev(tpCounts)) / K2
  fpf <- fpf[-length(fpf)]
  tpf <- tpf[-length(tpf)]
  
  return(list(
    fpCounts = fpCounts,
    tpCounts = tpCounts,
    fpf = fpf,
    tpf = tpf
  ))
}



UtilExtractDataStructure <- function(dataset){
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  nlDim <- dim(dataset$NL)
  llDim <- dim(dataset$LL)
  I <- nlDim[1]
  J <- nlDim[2]
  K <- nlDim[3]
  K2 <- llDim[3]
  K1 <- K - K2
  NL <- dataset$NL
  LL <- dataset$LL
  return(list(
    I = I,
    J = J,
    K1 = K1,
    K2 = K2,
    NL = NL,
    LL= LL
  ))
}
