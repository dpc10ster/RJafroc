#' Extract a subset of treatments and readers from a dataset
#' 
#' @description Extract a dataset consisting of a subset of treatments/readers from a larger dataset 
#' 
#' 
#' @param dataset The original dataset from which the subset is to be extracted;
#'    can be ROC, FROC or LROC
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
#' dataset1_2 <- DfExtractDataset(dataset05, trts = 1, rdrs = 2)
#' 
#' ## Extract the data of the first and third reader in all 
#' ## treatment from the included ROC dataset
#' datasetA_123 <- DfExtractDataset(dataset05, rdrs = c(1, 3))
#' 
#' @export

DfExtractDataset <- function(dataset, trts, rdrs){
  I <- length(dataset$modalityID)
  if (!missing(trts)){
    if (all(trts <= I)){
      I <- length(trts)
    }else{
      stop("Modality index/indices cannot exceed the total number of treatments in the original dataset.")
    }
  }else{
    trts <- 1:I
  }
  
  J <- length(dataset$NL[1,,1,1])
  if (!missing(rdrs)){
    if (all(rdrs <= J)){
      J <- length(rdrs)
    }else{
      stop("Reader index/indices cannot exceed the total number of readers in the original dataset.")
    }
  }else{
    rdrs <- 1:J
  }
  
  if (dataset$dataType != "LROC") {  
    K <- dim(dataset$NL)[3]
    K2 <- dim(dataset$LL)[3]
    maxNL <- dim(dataset$NL)[4]
    maxLL <- dim(dataset$LL)[4]
    NL <- dataset$NL[trts, rdrs, , ]
    dim(NL) <- c(I, J, K, maxNL)
    dataset$NL <- NL
    LL <- dataset$LL[trts, rdrs, , ]
    dim(LL) <- c(I, J, K2, maxLL)
    dataset$LL <- LL
    dataset$modalityID <- dataset$modalityID[trts]
    dataset$readerID <- dataset$readerID[rdrs]
    return(dataset)
  } else {
    K <- dim(dataset$NL)[3]
    K2 <- dim(dataset$LLCl)[3]
    maxNL <- 1
    maxLL <- 1
    NL <- dataset$NL[trts, rdrs, , ]
    dim(NL) <- c(I, J, K, maxNL)
    dataset$NL <- NL
    LLCl <- dataset$LLCl[trts, rdrs, , ]
    dim(LLCl) <- c(I, J, K2, maxLL)
    LLIl <- dataset$LLIl[trts, rdrs, , ]
    dim(LLIl) <- c(I, J, K2, maxLL)
    dataset$LLCl <- LLCl
    dataset$LLIl <- LLIl
    dataset$modalityID <- dataset$modalityID[trts]
    dataset$readerID <- dataset$readerID[rdrs]
    return(dataset)
  }
}