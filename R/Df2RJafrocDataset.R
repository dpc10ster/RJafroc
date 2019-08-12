#' Convert ratings arrays to an RJafroc dataset
#' 
#' @description Converts ratings arrays, ROC or FROC, \emph{not LROC}, to an \pkg{RJafroc} dataset, thereby allowing the 
#' user to leverage the file I/O, plotting and analyses capabilities of \pkg{RJafroc}. 
#' 
#' 
#' @param NL Non-lesion localizations array (or FP array for ROC data). 
#' @param LL Lesion localizations  array (or TP array for ROC data). 
#' @param ... Other elements of \pkg{RJafroc} dataset that may, depending on the context, need to be specified. 
#' \code{lesionVector} \strong{must} be specified if an FROC dataset is to be returned. It is a \code{K2}-length array specifying
#' the numbers of lesions in each diseased case in the dataset.
#' 
#' @return A dataset with the structure described in \code{\link{RJafroc-package}}.
#' 
#' @details The function "senses" the data type (ROC or FROC) from the the absence or presence of \code{lesionVector}. 
#' ROC data can be \code{NL[1:K1]} and \code{LL[1:K2]} or \code{NL[1:I,1:J,1:K1]} and \code{LL[1:I,1:J,1:K2]}. 
#' FROC data can be \code{NL[1:K1,1:maxNL]} and \code{LL[1:K2, 1:maxLL]} or \code{NL[1:I,1:J,1:K1,1:maxNL]} and 
#' \code{LL[1:I,1:J,1:K2,1:maxLL]}. 
#' Here \code{maxNL/maxLL} = maximum numbers of NLs/LLs, per case, over entire dataset.  
#' Equal weights are assigned to every lesion (FROC data). 
#' Consecutive characters/integers starting from "1" are assigned to \code{lesionID}, \code{modalityID} and \code{readerID}.
#' 
#' @examples
#' set.seed(1)
#' NL <- rnorm(5)
#' LL <- rnorm(7)*1.5 + 2
#' dataset <- Df2RJafrocDataset(NL, LL) # an ROC dataset
#'
#' I <- 2;J <- 3;set.seed(1)
#' K1 <- 25;K2 <- 35
#' z1 <- array(dim = c(I, J, K1))
#' z2 <- array(dim = c(I, J, K2))
#' mu <- 2;sigma <- 1.5
#' for (i in 1:I) {
#'  for (j in 1:J) {
#'    z1[i,j,1:K1] <- rnorm(K1)
#'    z2[i,j,] <- rnorm(K2) * sigma + mu
#'  }
#' }
#' dataset <- Df2RJafrocDataset(z1, z2) ## note lesionVector consists of 1s; i.e., an ROC dataset
#'
#' set.seed(1)
#' mu <- 1;lambda <- 1;nu <- 1; zeta1 <- 0
#' K1 <- 5;K2 <- 7
#' Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
#' frocDataRaw <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, 
#' lesionVector = Lk2)
#' NL <- drop(frocDataRaw$NL)
#' LL <- drop(frocDataRaw$LL)
#' dataset <- Df2RJafrocDataset(NL, LL, lesionVector = Lk2) 
#' ## note lesionVector is not all 1s, signalling an FROC dataset
#'
#' ## Simulate FROC dataset, convert to dataset object, display ROC, FROC and AFROC curves
#' I <- 2;J <- 3;set.seed(1)
#' K1 <- 25;K2 <- 35
#' mu <- 1;nuP <- 0.8;lambdaP <- 1;zeta1 <- 0
#' lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
#' nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
#' Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
#' z1 <- array(-Inf,dim = c(I,J,K1+K2,40))
#' z2 <- array(-Inf,dim = c(I,J,K2,40))
#' dimNL <- array(dim=c(I,J,2)) 
#' ## the last value (2) accommodates case and location indices
#' dimLL <- array(dim=c(I,J,2))
#' for (i in 1:I) {
#'   for (j in 1:J) {
#'     frocDataRaw <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, 
#'     J = 1, K1, K2, lesionVector = Lk2)
#'     dimNL[i,j,] <- dim(drop(frocDataRaw$NL))
#'     dimLL[i,j,] <- dim(drop(frocDataRaw$LL))
#'     z1[i,j,,1:dimNL[i,j,2]] <- drop(frocDataRaw$NL) # drop the excess location indices
#'     z2[i,j,,1:dimLL[i,j,2]] <- drop(frocDataRaw$LL)
#'   }
#' }
#' z1 <- z1[,,,1:max(dimNL[,,2])]
#' z2 <- z2[,,,1:max(dimLL[,,2])]
#'
#' dataset <- Df2RJafrocDataset(z1, z2, lesionVector = Lk2)
#'
#' retPlot <- PlotEmpiricalOperatingCharacteristics(dataset, 
#' trts = seq(1,I), rdrs = seq(1,J), opChType = "ROC")
#' print(retPlot$Plot)
#'
#' retPlot <- PlotEmpiricalOperatingCharacteristics(dataset, 
#' trts = seq(1,I), rdrs = seq(1,J), opChType = "FROC")
#' print(retPlot$Plot)
#'
#' retPlot <- PlotEmpiricalOperatingCharacteristics(dataset, 
#' trts = seq(1,I), rdrs = seq(1,J), opChType = "AFROC")
#' print(retPlot$Plot)
#'
#' 
#' 
#' @export
#' 
Df2RJafrocDataset <- function(NL, LL, ...)  {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  inputList <- list(...)
  if (length(inputList) == 0) dataType <- "ROC" 
  else if (names(inputList) == "lesionVector") dataType <- "FROC" else stop("unknown data type")
  
  if (is.vector(NL)) {
    I <- 1
    J <- 1
    K1 <- length(NL)
    K2 <- length(LL)
    NL1 <- array(UNINITIALIZED,dim=c(I,J,K1+K2,1))
    NL1[,,1:K1,1] <- NL
    NL <- NL1
    dim(LL) <- c(1,1,K2,1)
    if (dataType == "ROC") lesionVector <- rep(1, K2) else 
      lesionVector <- FrocDataDescriptor(inputList)$lesionVector
    if (dataType == "ROC") lesionID <- as.matrix(lesionVector, c(K2, 1)) else 
      lesionID <- FrocDataDescriptor(inputList)$lesionID
    if (dataType == "ROC") lesionWeight <- lesionID else 
      lesionWeight <- FrocDataDescriptor(inputList)$lesionWeight
    modalityID <- "1"
    readerID <- "1"
    dataset <- list(NL = NL, 
                    LL = LL, 
                    lesionVector = lesionVector, 
                    lesionID = lesionID, 
                    lesionWeight = lesionWeight, 
                    dataType = dataType, 
                    modalityID = modalityID, 
                    readerID = readerID
    )
    return(dataset)
  }
  
  if (is.array(NL)) {
    nLdim <- dim(NL)
    lLdim <- dim(LL)
    if (length(nLdim) != length(lLdim)) stop("array dimensions must have same lengths")
  }
  
  if  (length(nLdim) == 2) {
    if (dataType == "ROC") {
      I <- 1
      J <- nLdim[1]
      K1 <- nLdim[2]
      K2 <- lLdim[2]
      NL1 <- array(dim=c(I,J,K1+K2,1))
      NL1[1:I,1:J,1:K1,1] <- NL[1:J,1:K1];NL1[1:I,1:J,(K1+1):(K1+K2),1] <- UNINITIALIZED
      NL <- NL1
      dim(LL) <- c(I,J,K2,1)
    } else {
      I <- 1
      J <- 1
      K1 <- nLdim[1]
      K2 <- lLdim[1]
      K1 <- K1 - K2
      dim(NL) <- c(I,J,K1+K2,nLdim[2])
      dim(LL) <- c(I,J,K2,lLdim[2])
    }
    if (dataType == "ROC") lesionVector <- rep(1, K2) else 
      lesionVector <- FrocDataDescriptor(inputList)$lesionVector
    if (dataType == "ROC") lesionID <- as.matrix(lesionVector, c(K2, 1)) else 
      lesionID <- FrocDataDescriptor(inputList)$lesionID
    if (dataType == "ROC") lesionWeight <- lesionID else 
      lesionWeight <- FrocDataDescriptor(inputList)$lesionWeight
    for (k in 1:K2){
      lesionID[k, 1:lesionVector[k]] <- seq(1:lesionVector[k])
      lesionWeight[k, 1:lesionVector[k]] <- rep(1/lesionVector[k], lesionVector[k])
    }
    modalityID <- as.character(seq(1:I))
    readerID <- as.character(seq(1:J))
    dataset <- list(NL = NL, 
                    LL = LL, 
                    lesionVector = lesionVector, 
                    lesionID = lesionID, 
                    lesionWeight = lesionWeight, 
                    dataType = dataType, 
                    modalityID = modalityID, 
                    readerID = readerID
    )
    return(dataset)
  } 
  
  if  (length(nLdim) == 3) {
    if (dataType == "ROC") {
      I <- nLdim[1]
      J <- nLdim[2]
      K1 <- nLdim[3]
      K2 <- lLdim[3]
      NL1 <- array(dim=c(I,J,K1+K2,1))
      NL1[1:I,1:J,1:K1,1] <- NL[1:I,1:J,1:K1];NL1[1:I,1:J,(K1+1):(K1+K2),1] <- UNINITIALIZED
      NL <- NL1
      dim(LL) <- c(I,J,K2,1)
    } else {
      I <- 1
      J <- nLdim[1]
      K1 <- nLdim[2]
      K2 <- lLdim[2]
      dim(NL) <- c(I,J,K1+K2,nLdim[3])
      dim(LL) <- c(I,J,K2,lLdim[3])
    }
    if (dataType == "ROC") {
      lesionVector <- rep(1, K2)
      lesionID <- as.matrix(lesionVector, c(K2, 1))
      lesionWeight <- lesionID
    } else {
      temp1 <- FrocDataDescriptor(inputList)
      lesionVector <- temp1$lesionVector
      lesionID <- temp1$lesionID
      lesionWeight <- temp1$lesionWeight
    }
    modalityID <- as.character(seq(1:I))
    readerID <- as.character(seq(1:J))
    dataset <- list(NL = NL, 
                    LL = LL, 
                    lesionVector = lesionVector, 
                    lesionID = lesionID, 
                    lesionWeight = lesionWeight, 
                    dataType = dataType, 
                    modalityID = modalityID, 
                    readerID = readerID
    )
    return(dataset)
  } 
  
  if (length(nLdim) == 4) {
    I <- nLdim[1]
    J <- nLdim[2]
    K1 <- nLdim[3]
    K2 <- lLdim[3]
    if (dataType == "ROC") {
      lesionVector <- rep(1, K2)
      lesionID <- as.matrix(lesionVector, c(K2, 1))
      lesionWeight <- lesionID
    } else {
      temp1 <- FrocDataDescriptor(inputList)
      lesionVector <- temp1$lesionVector
      lesionID <- temp1$lesionID
      lesionWeight <- temp1$lesionWeight
    }
    modalityID <- as.character(seq(1:I))
    readerID <- as.character(seq(1:J))
    dataset <- list(NL = NL, 
                    LL = LL, 
                    lesionVector = lesionVector, 
                    lesionID = lesionID, 
                    lesionWeight = lesionWeight, 
                    dataType = dataType, 
                    modalityID = modalityID, 
                    readerID = readerID)    
    return(dataset)
  }
  stop("could not figure out data type")
}


#################################################################################################
FrocDataDescriptor <- function(inputList) {
  K2 <- length(inputList$lesionVector)
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  lesionVector <- inputList$lesionVector
  lesionID <- array(UNINITIALIZED, dim = c(K2, max(lesionVector)))
  lesionWeight <- lesionID
  for (k in 1:K2){
    lesionID[k, 1:lesionVector[k]] <- seq(1:lesionVector[k])
    lesionWeight[k, 1:lesionVector[k]] <- rep(1/lesionVector[k], lesionVector[k])
  }
  return(list (
    lesionVector = lesionVector,
    lesionID = lesionID,
    lesionWeight = lesionWeight))
}
