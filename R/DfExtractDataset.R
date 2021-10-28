#' Extract a subset of treatments and readers from a dataset
#' 
#' @description Extract a dataset consisting of a subset of treatments/readers from a larger dataset 
#' 
#' 
#' @param dataset The original dataset from which the subset is to be extracted
#' @param trts A vector contains the indices of the treatments to be extracted. 
#'    \strong{If this parameter is not supplied, all treatments are extracted.}
#' @param rdrs A vector contains the indices of the readers to be extracted. 
#'    \strong{If this parameter is not supplied, all readers are extracted.}
#' 
#' @return A new dataset containing only the specified treatments and readers that were
#' extracted from the original dataset
#' 
#' @details \strong{Note} that \code{trts} and \code{rdrs} are the vectors of \strong{indices} 
#'    not \strong{IDs}. For example, if the ID of the first reader is "0", the 
#'    corresponding value in \code{trts} should be \strong{1}  not \strong{0}.
#' 
#' @examples 
#' ## Extract the data corresponding to the second reader in the 
#' ## first treatment from an included ROC dataset
#' ds1 <- DfExtractDataset(dataset05, trts = 1, rdrs = 2)
#' 
#' ## Extract the data of the first and third reader in all 
#' ## treatment from the included ROC dataset
#' ds2 <- DfExtractDataset(dataset05, rdrs = c(1, 3))
#' 
#' @export

DfExtractDataset <- function(dataset, trts, rdrs) {
  
  if (dataset$descriptions$design == "FCTRL-X-MOD") {
    ds <- DfExtractXDataset(dataset, trts, rdrs)
    return(ds)
  } else {
    I <- length(dataset$descriptions$modalityID)
    if (!missing(trts)){
      if (all(trts <= I)){
        I <- length(trts)
      }else{
        stop("Modality index/indices cannot exceed the total number of treatments in the original dataset.")
      }
    }else{
      trts <- 1:I
    }
  }
  
  J <- length(dataset$ratings$NL[1,,1,1])
  if (!missing(rdrs)){
    if (all(rdrs <= J)){
      J <- length(rdrs)
    }else{
      stop("Reader index/indices cannot exceed the total number of readers in the original dataset.")
    }
  }else{
    rdrs <- 1:J
  }
  
  K <- dim(dataset$ratings$NL)[3]
  K2 <- dim(dataset$ratings$LL)[3]
  
  NL <- dataset$ratings$NL[trts, rdrs, , , drop = FALSE]
  maxNL <- length(NL[1,1,1,]) # determine this from the extracted values
  dim(NL) <- c(I, J, K, maxNL)
  
  LL <- dataset$ratings$LL[trts, rdrs, , , drop = FALSE]
  maxLL <- length(LL[1,1,1,]) # determine this from the extracted values
  dim(LL) <- c(I, J, K2, maxLL)
  
  if (is.numeric(dataset$ratings$LL_IL)) {
    LL_IL <- dataset$ratings$LL_IL[trts, rdrs, , 1]
    dim(LL_IL) <- c(I, J, K2, 1)
  } else LL_IL <- NA
  
  modalityID <- dataset$descriptions$modalityID[trts]
  readerID <- dataset$descriptions$readerID[rdrs]
  
  # start code fix issue T1-RRRC for ROC data #73 
  # if (is.numeric(dataset$descriptions$truthTableStr)) {
  #   truthTableStr <- dataset$descriptions$truthTableStr[trts,rdrs,,,drop=FALSE]
  # } else truthTableStr <- NA
  if (!all(is.na(dataset$descriptions$truthTableStr))) {
    truthTableStr <- dataset$descriptions$truthTableStr[trts,rdrs,,,drop=FALSE]
  } else truthTableStr <- NA
  # end code fix issue T1-RRRC for ROC data #73 
  
  fileName <- paste0("DfExtractDataset(", dataset$descriptions$fileName,")")
  name <- dataset$descriptions$name
  design <- dataset$descriptions$design
  type <- dataset$descriptions$type
  perCase <- dataset$lesions$perCase
  IDs <- dataset$lesions$IDs
  weights <- dataset$lesions$weights
  
  return(convert2dataset(NL, LL, LL_IL, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
}


DfExtractXDataset <- function(dataset, trts, rdrs) {
  
  if (dataset$descriptions$design != "FCTRL-X-MOD") {
    stop("This function requires a crossed modality dataset")
  } else {
    I1 <- length(dataset$descriptions$modalityID1)
    I2 <- length(dataset$descriptions$modalityID2)
    trts1 <- 1:I1
    trts2 <- 1:I2
  }
  
  J <- length(dataset$ratings$NL[1,1,,1,1])
  if (!missing(rdrs)){
    if (all(rdrs <= J)){
      J <- length(rdrs)
    }else{
      stop("Reader index/indices cannot exceed the total number of readers in the original dataset.")
    }
  }else{
    rdrs <- 1:J
  }
  
  K <- dim(dataset$ratings$NL)[4]
  K2 <- dim(dataset$ratings$LL)[4]
  
  NL <- dataset$ratings$NL[trts1, trts2, rdrs, , , drop = FALSE]
  maxNL <- length(NL[1,1,1,1,]) # determine this from the extracted values
  dim(NL) <- c(I1, I2, J, K, maxNL)
  
  LL <- dataset$ratings$LL[trts1, trts2, rdrs, , , drop = FALSE]
  maxLL <- length(LL[1,1,1,1,]) # determine this from the extracted values
  dim(LL) <- c(I1, I2, J, K2, maxLL)
  
  if (is.numeric(dataset$ratings$LL_IL)) {
    LL_IL <- dataset$ratings$LL_IL[trts1, trts2, rdrs, , 1]
    dim(LL_IL) <- c(I1, I2, J, K2, 1)
  } else LL_IL <- NA
  
  modalityID1 <- dataset$descriptions$modalityID1
  modalityID2 <- dataset$descriptions$modalityID2
  readerID <- dataset$descriptions$readerID[rdrs]
  
  if (is.numeric(dataset$descriptions$truthTableStr)) {
    truthTableStr <- dataset$descriptions$truthTableStr[trts1, trts2, rdrs,,,drop=FALSE]
  } else truthTableStr <- NA
  
  fileName <- paste0("DfExtractXDataset(", dataset$descriptions$fileName, ")")
  name <- dataset$descriptions$name
  design <- dataset$descriptions$design
  type <- dataset$descriptions$type
  perCase <- dataset$lesions$perCase
  IDs <- dataset$lesions$IDs
  weights <- dataset$lesions$weights
  
  return(convert2Xdataset(NL, LL, LL_IL, 
                          perCase, IDs, weights,
                          fileName, type, name, truthTableStr, design,
                          modalityID1, modalityID2, readerID))
}

