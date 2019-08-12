isValidDataset <- function(dataset) {
  if (typeof(dataset) != "list") return (FALSE)
  if (!(length(dataset) %in% c(8,9))) return (FALSE)
  
  if (length(dataset) == 8) {
    if (!is.array(dataset$NL))  return (FALSE)
    I <- length(dataset$NL[,1,1,1])
    J <- length(dataset$NL[1,,1,1])
    K <- length(dataset$NL[1,1,,1])
    
    if (!is.vector(dataset$lesionVector))  return (FALSE)
    if (!is.array(dataset$lesionWeight))  return (FALSE)
    if (!is.array(dataset$lesionID))  return (FALSE)
    
    if (!is.array(dataset$LL))  return (FALSE)
    
    K2 <- length(dataset$LL[1,1,,1])
    
    if (length(dataset$lesionVector) != K2) return (FALSE)
    if (length(dataset$lesionID[,1]) != K2) return (FALSE)
    if (length(dataset$lesionWeight[,1]) != K2) return (FALSE)
    if (length(dataset$modalityID) != I) return (FALSE)
    if (length(dataset$readerID) != J) return (FALSE)
    
    K1 <- K - K2
    if (!(dataset$dataType) %in% c("ROC", "FROC", "LROC"))  return (FALSE)
    return(TRUE)
  } else if (length(dataset) == 9) {
    if (!is.array(dataset$NL))  return (FALSE)
    I <- length(dataset$NL[,1,1,1])
    J <- length(dataset$NL[1,,1,1])
    K <- length(dataset$NL[1,1,,1])
    
    if (!is.vector(dataset$lesionVector))  return (FALSE)
    if (!is.array(dataset$lesionWeight))  return (FALSE)
    if (!is.array(dataset$lesionID))  return (FALSE)
    
    if (!is.array(dataset$LL))  return (FALSE)
    
    K2 <- length(dataset$LL[1,1,,1])
    
    if (length(dataset$lesionVector) != K2) return (FALSE)
    if (length(dataset$lesionID[,1]) != K2) return (FALSE)
    if (length(dataset$lesionWeight[,1]) != K2) return (FALSE)
    if (length(dataset$modalityID) != I) return (FALSE)
    if (length(dataset$readerID) != J) return (FALSE)
    
    K1 <- K - K2
    if (!(dataset$dataType) %in% c("ROC", "FROC", "LROC"))  return (FALSE)
    
    return(TRUE)
  } else {
    stop("Incorrect length of dataset list")
  }
}
