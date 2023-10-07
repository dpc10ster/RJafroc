OrVarCov <- function(jkFomValues, modalityID, readerID, covEstMethod) 
{
  
  if (covEstMethod == "jackknife") {
    
    I <- dim(jkFomValues)[1]
    J <- dim(jkFomValues)[2]
    
    x <- FOM2VarCov(jkFomValues, varInflFactor = TRUE, flag = "IJ")
    vc <- array(dim = 4)
    vc[1] <- x$Var
    vc[2] <- x$Cov1
    vc[3] <- x$Cov2
    vc[4] <- x$Cov3
    names(vc) <- c("Var", "Cov1", "Cov2", "Cov3")
    
    vcEachTrt <- array(dim = c(I,2))
    for (i in 1:I) {
      x <- FOM2VarCov(jkFomValues[i,,], varInflFactor = TRUE, flag = "J")
      vcEachTrt[i,1] <- x$Var 
      vcEachTrt[i,2] <- x$Cov2    
    }
    rownames(vcEachTrt) <- modalityID
    colnames(vcEachTrt) <- c("Var", "Cov2")
    
    vcEachRdr <- array(dim = c(J,2))
    for (j in 1:J) {
      x <- FOM2VarCov(jkFomValues[,j,], varInflFactor = TRUE, flag = "I")
      vcEachRdr[j,1] <- x$Var
      vcEachRdr[j,2] <- x$Cov1
    }
    rownames(vcEachRdr) <- readerID
    colnames(vcEachRdr) <- c("Var", "Cov1")
    
    return (list(
      vc = vc,
      vcEachTrt = vcEachTrt,
      vcEachRdr = vcEachRdr
    ))
    
  } 
  
  else if (covEstMethod == "bootstrap") {
    
    stop("code needs fixing: OrVarCov bootstrap")
    ret <- varCompBS (dataset, FOM, FPFValue, nBoots, seed)
    
  } 
  
  else if (covEstMethod == "DeLong") {
    
    stop("code needs fixing: OrVarCov DeLong")
    ret <- varComponentsDeLong (dataset, FOM)
    
  } 
  
}  

