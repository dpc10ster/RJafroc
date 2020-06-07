#' Convert ratings arrays to an RJafroc dataset
#' 
#' @description Converts ratings arrays, ROC or FROC, \emph{but not LROC}, to an \pkg{RJafroc} dataset, thereby allowing the 
#' user to leverage the file I/O, plotting and analyses capabilities of \pkg{RJafroc}. 
#' 
#' 
#' @param NL Non-lesion localizations array (or FP array for ROC data). 
#' @param LL Lesion localizations array (or TP array for ROC data). 
#' @param InputIsCountsTable If \code{TRUE}, the \code{NL} and \code{LL} 
#'    arrays are rating-counts tables, with common lengths equal to the 
#'    number of ratings \code{R}, if \code{FALSE}, the default, these are arrays 
#'    of lengths \code{K1}, the number of non-diseased cases, and \code{K2}, 
#'    the number of diseased cases, respectively.
#' @param ... Other elements of \pkg{RJafroc} dataset that may, depending on the 
#'    context, need to be specified. \code{perCase} \strong{must} be specified 
#'       if an FROC dataset is to be returned. It is a \code{K2}-length array 
#'       specifying the numbers of lesions in each diseased case in the dataset.
#' 
#' @return A dataset with the structure described in \code{\link{RJafroc-package}}.
#' 
#' @details The function "senses" the data type (ROC or FROC) from the the absence 
#'    or presence of \code{perCase}.
#' \itemize{
#' \item{ROC data can be \code{NL[1:K1]} and \code{LL[1:K2]} or \code{NL[1:I,1:J,1:K1]} 
#'    and \code{LL[1:I,1:J,1:K2]}.}
#' \item{FROC data can be \code{NL[1:K1,1:maxNL]} and \code{LL[1:K2, 1:maxLL]} or 
#'    \code{NL[1:I,1:J,1:K1,1:maxNL]} and  \code{LL[1:I,1:J,1:K2,1:maxLL]}.}
#' } 
#'  
#'  
#' Here \code{maxNL/maxLL} = maximum numbers of NLs/LLs, per case, over entire dataset.  
#' Equal weights are assigned to every lesion (FROC data). 
#' Consecutive characters/integers starting with "1" are assigned to \code{IDs}, \code{modalityID} and \code{readerID}.
#' 
#' @examples
#' set.seed(1)
#' NL <- rnorm(5)
#' LL <- rnorm(7)*1.5 + 2
#' dataset <- Df2RJafrocDataset(NL, LL) # an ROC dataset
#'
#' ## Input as counts tables
#' K1t <- c(30, 19, 8, 2, 1)
#' K2t <- c(5,  6, 5, 12, 22)
#' dataset <- Df2RJafrocDataset(K1t, K2t, InputIsCountsTable = TRUE)
#' ## perCase consists of 1s; i.e., an ROC dataset
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
#' dataset <- Df2RJafrocDataset(z1, z2) 
#'
#' ## note perCase is not all 1s, signalling an FROC dataset
#' set.seed(1)
#' mu <- 1;lambda <- 1;nu <- 1; zeta1 <- 0
#' K1 <- 5;K2 <- 7
#' Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
#' frocDataRaw <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, J = 1, K1, K2, 
#' perCase = Lk2)
#' NL <- drop(frocDataRaw$ratings$NL)
#' LL <- drop(frocDataRaw$ratings$LL)
#' dataset <- Df2RJafrocDataset(NL, LL, perCase = Lk2) 
#' 
#'
#' ## Simulate FROC dataset, convert to dataset object
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
#'     J = 1, K1, K2, perCase = Lk2)
#'     dimNL[i,j,] <- dim(drop(frocDataRaw$ratings$NL))
#'     dimLL[i,j,] <- dim(drop(frocDataRaw$ratings$LL))
#'     z1[i,j,,1:dimNL[i,j,2]] <- drop(frocDataRaw$ratings$NL) # drop the excess location indices
#'     z2[i,j,,1:dimLL[i,j,2]] <- drop(frocDataRaw$ratings$LL)
#'   }
#' }
#' z1 <- z1[,,,1:max(dimNL[,,2])]
#' z2 <- z2[,,,1:max(dimLL[,,2])]
#'
#' dataset <- Df2RJafrocDataset(z1, z2, perCase = Lk2)
#'
#' 
#' @export
#' 
Df2RJafrocDataset <- function(NL, LL, InputIsCountsTable = FALSE, ...)  {
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  inputList <- list(...)
  if (length(inputList) == 0) {
    type <- "ROC" 
    if (InputIsCountsTable == TRUE) {
      ret <- RatingsArraysFromRatingsTables(NL,LL)
      NL <- ret$NL
      LL <- ret$LL
    }
  }
  else if (names(inputList) == "perCase") type <- "FROC" else stop("unknown data type")
  
  if (is.vector(NL)) {
    I <- 1
    J <- 1
    K1 <- length(NL)
    K2 <- length(LL)
    NL1 <- array(UNINITIALIZED,dim=c(I,J,K1+K2,1))
    NL1[,,1:K1,1] <- NL
    NL <- NL1
    dim(LL) <- c(1,1,K2,1)
    if (type == "ROC") perCase <- rep(1, K2) else 
      perCase <- FrocDataDescriptor(inputList)$perCase
    if (type == "ROC") IDs <- as.matrix(perCase, c(K2, 1)) else 
      IDs <- FrocDataDescriptor(inputList)$IDs
    if (type == "ROC") weights <- IDs else 
      weights <- FrocDataDescriptor(inputList)$weights
    modalityID <- "1"
    readerID <- "1"
    binned <- isBinned(NL, LL)
    fileName <- NA
    name <- NA
    design <- "FCTRL"
    truthTableStr <- NA
    return(convert2dataset(NL, LL, LL_IL = NA, 
                           perCase, IDs, weights,
                           binned, fileName, type, name, truthTableStr, design,
                           modalityID, readerID))
  }
  
  if (is.array(NL)) {
    nLdim <- dim(NL)
    lLdim <- dim(LL)
    if (length(nLdim) != length(lLdim)) stop("array dimensions must have same lengths")
  }
  
  if  (length(nLdim) == 2) {
    if (type == "ROC") {
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
    if (type == "ROC") perCase <- rep(1, K2) else 
      perCase <- FrocDataDescriptor(inputList)$perCase
    if (type == "ROC") IDs <- as.matrix(perCase, c(K2, 1)) else 
      IDs <- FrocDataDescriptor(inputList)$IDs
    if (type == "ROC") weights <- IDs else 
      weights <- FrocDataDescriptor(inputList)$weights
    for (k in 1:K2){
      IDs[k, 1:perCase[k]] <- seq(1:perCase[k])
      weights[k, 1:perCase[k]] <- rep(1/perCase[k], perCase[k])
    }
    modalityID <- as.character(seq(1:I))
    readerID <- as.character(seq(1:J))
    binned <- isBinned(NL, LL)
    fileName <- NA
    name <- NA
    design <- "FCTRL"
    truthTableStr <- NA
    return(convert2dataset(NL, LL, LL_IL = NA, 
                           perCase, IDs, weights,
                           binned, fileName, type, name, truthTableStr, design,
                           modalityID, readerID))
    
  } 
  
  if  (length(nLdim) == 3) {
    if (type == "ROC") {
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
    if (type == "ROC") {
      perCase <- rep(1, K2)
      IDs <- as.matrix(perCase, c(K2, 1))
      weights <- IDs
    } else {
      temp1 <- FrocDataDescriptor(inputList)
      perCase <- temp1$perCase
      IDs <- temp1$IDs
      weights <- temp1$weights
    }
    modalityID <- as.character(seq(1:I))
    readerID <- as.character(seq(1:J))
    binned <- isBinned(NL, LL)
    fileName <- NA
    name <- NA
    design <- "FCTRL"
    truthTableStr <- NA
    return(convert2dataset(NL, LL, LL_IL = NA, 
                           perCase, IDs, weights,
                           binned, fileName, type, name, truthTableStr, design,
                           modalityID, readerID))
  } 
  
  if (length(nLdim) == 4) {
    I <- nLdim[1]
    J <- nLdim[2]
    K1 <- nLdim[3]
    K2 <- lLdim[3]
    if (type == "ROC") {
      perCase <- rep(1, K2)
      IDs <- as.matrix(perCase, c(K2, 1))
      weights <- IDs
    } else {
      temp1 <- FrocDataDescriptor(inputList)
      perCase <- temp1$perCase
      IDs <- temp1$IDs
      weights <- temp1$weights
    }
    modalityID <- as.character(seq(1:I))
    readerID <- as.character(seq(1:J))
    binned <- isBinned(NL, LL)
    fileName <- NA
    name <- NA
    design <- "FCTRL"
    truthTableStr <- NA
    return(convert2dataset(NL, LL, LL_IL = NA, 
                           perCase, IDs, weights,
                           binned, fileName, type, name, truthTableStr, design,
                           modalityID, readerID))
  }
  stop("could not figure out data type")
  
}

#################################################################################################
FrocDataDescriptor <- function(inputList) {
  K2 <- length(inputList$perCase)
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  perCase <- inputList$perCase
  IDs <- array(UNINITIALIZED, dim = c(K2, max(perCase)))
  weights <- IDs
  for (k in 1:K2){
    IDs[k, 1:perCase[k]] <- seq(1:perCase[k])
    weights[k, 1:perCase[k]] <- rep(1/perCase[k], perCase[k])
  }
  return(list (
    perCase = perCase,
    IDs = IDs,
    weights = weights))
}


# K1 and K2 are as in book chapter 5
RatingsArraysFromRatingsTables <- function( K1, K2 ) {
  
  R <- length(K1)
  if (length(K2) != R) stop("Length of two ratings arrays are unequal")
  tab <- data.frame(value=seq(1:R), freq=K1)
  NL <- rep(tab$value, tab$freq)
  tab <- data.frame(value=seq(1:R), freq=K2)
  LL <- rep(tab$value, tab$freq)
  
  return( list(
    NL = NL,
    LL = LL
  ) )
  
}


isBinned <- function(NL, LL, minUniqeRatings = 6){
  I <- dim(NL)[1]
  J <- dim(NL)[2]
  binned <- array(dim = c(I,J))
  for (i in 1:I) {
    for (j in 1:J) {
      nl <- NL[i,j,,]
      ll <- LL[i,j,,]
      if (length(unique(c(ll[is.finite(ll)], nl[is.finite(nl)]))) <= minUniqeRatings) 
        binned[i,j] <- TRUE else binned[i,j] <- FALSE
    }
  }
  return (binned)
}

convert2dataset <- function(NL, LL, LL_IL, 
                            perCase, IDs, weights,
                            binned, fileName, type, name, truthTableStr, design,
                            modalityID, readerID) {
  ratings <- list(NL = NL,
                  LL = LL,
                  LL_IL = LL_IL)
  
  lesions <- list(perCase = perCase,
                  IDs = IDs,
                  weights = weights)
  
  descriptions <- list(binned = binned,
                       fileName = fileName,
                       type = type,
                       name = name,
                       truthTableStr = truthTableStr,
                       design = design,
                       modalityID = modalityID,
                       readerID = readerID)
  
  dataset <- list(ratings = ratings, 
                  lesions = lesions, 
                  descriptions = descriptions)
  
  return(dataset)
  
}
