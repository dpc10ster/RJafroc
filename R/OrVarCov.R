SampledFom2ORCov <- function(jkFomValues, 
                        modalityID, 
                        readerID, 
                        covEstMethod, 
                        FPFValue, 
                        nBoots, 
                        seed) 
{
  
  if (covEstMethod == "jackknife") {
    
    if (length(dim(jkFomValues)) == 3) {
      # factorial one-treatment dataset
      
      I <- dim(jkFomValues)[1]
      J <- dim(jkFomValues)[2]
      
      x <- FOM2VarCov(jkFomValues, varInflFactor = TRUE, flag = "IJ")
      # XModality Dataset
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
    } else if (length(dim(jkFomValues)) == 4) {
      # cross modality dataset
      
      I1 <- dim(jkFomValues)[1]
      I2 <- dim(jkFomValues)[2]
      J <- dim(jkFomValues)[3]
      I <- c(I2, I1)
      
      fomAvgArray <- list()
      vc <- list()
      vcEachTrt <- list()
      vcEachRdr <- list()
      
      for (avgIndx in 1:2) {
        
        # average over first modality and all readers
        fomAvgArray[[avgIndx]] <- apply(jkFomValues, (1:4)[-avgIndx], mean) 
        
        x <- FOM2VarCov(
          fomAvgArray[[avgIndx]], 
          varInflFactor = TRUE, 
          flag = "IJ")
        vc[[avgIndx]] <- array(dim = 4)
        vc[[avgIndx]][1] <- x$Var
        vc[[avgIndx]][2] <- x$Cov1
        vc[[avgIndx]][3] <- x$Cov2
        vc[[avgIndx]][4] <- x$Cov3
        names(vc[[avgIndx]]) <- c("Var", "Cov1", "Cov2", "Cov3")
        
        vcEachTrt[[avgIndx]] <- array(dim = c(I[avgIndx],2))
        for (i in 1:I[avgIndx]) {
          x <- FOM2VarCov(
            fomAvgArray[[avgIndx]][i,,], 
            varInflFactor = TRUE, 
            flag = "J")
          vcEachTrt[[avgIndx]][i,1] <- x$Var 
          vcEachTrt[[avgIndx]][i,2] <- x$Cov2    
        }
        rownames(vcEachTrt[[avgIndx]]) <- modalityID[[avgIndx]]
        colnames(vcEachTrt[[avgIndx]]) <- c("Var", "Cov2")
        
        vcEachRdr[[avgIndx]] <- array(dim = c(J,2))
        for (j in 1:J) {
          x <- FOM2VarCov(
            fomAvgArray[[avgIndx]][,j,], 
            varInflFactor = TRUE, 
            flag = "I")
          vcEachRdr[[avgIndx]][j,1] <- x$Var
          vcEachRdr[[avgIndx]][j,2] <- x$Cov1
        }
        rownames(vcEachRdr[[avgIndx]]) <- readerID
        colnames(vcEachRdr[[avgIndx]]) <- c("Var", "Cov1")
      }
    } else stop("incorrect length(dim(jkFomValues)).\n")
    
    return (list(
      vc = vc,
      vcEachTrt = vcEachTrt,
      vcEachRdr = vcEachRdr
    ))
    
  } 
  
  else if (covEstMethod == "bootstrap") {
    
    stop("code needs fixing: SampledFom2ORCov bootstrap")
    # ret <- varCompBS (dataset, FOM, FPFValue, nBoots, seed)
    
  } 
  
  else if (covEstMethod == "DeLong") {
    
    stop("code needs fixing: SampledFom2ORCov DeLong")
    # ret <- varCompDeLong (dataset)
    
  } 
  
}  

