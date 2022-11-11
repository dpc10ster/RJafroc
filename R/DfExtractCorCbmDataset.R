#' Extract two arms of a pairing from an MRMC ROC dataset
#'
#' @description Extract a paired dataset from a larger dataset. The pairing could be
#' two readers in the same
#' treatment, or different readers in different treatments, or the same reader
#' in different treatments. If necessary
#' The data is binned to 5 bins in each condition.
#'
#'
#' @param dataset The original dataset from which the pairing is to be extracted
#' @param trts A vector, maximum length 2, contains the indices of the treatment
#' or treatments to be extracted
#' @param rdrs A vector, maximum length 2, contains the indices of the reader or
#' readers to be extracted
#'
#' @return A 1-treatment 2-reader dataset
#'
#' @details The desired pairing is contained in the vectors \code{trts} and \code{rdrs}.
#' If either has length one, the other must
#' have length two and the pairing is implicit. If both are length two, then the pairing
#' is that implied by the first treatement
#' and the second reader, which is one arm, and the other arm is that implied by the second
#' treatment paired with the first
#' reader. Using this method any allowed pairing can be extracted and analyzed by \code{\link{FitCorCbm}}.
#' The utility of this software is
#' in designing a ratings simulator that is statistically matched to a real dataset.
#'
#' @examples
#' 
#' \donttest{
#' ## Extract the paired data corresponding to the second and third readers in the first treatment
#' ## from the included ROC dataset
#' dataset11_23 <- DfExtractCorCbmDataset(dataset05, trts = 1, rdrs = c(2,3))
#'
#' ## Extract the paired data corresponding to the third reader in the first and second treatments
#' dataset12_33 <- DfExtractCorCbmDataset(dataset05, trts = c(1,2), rdrs = 3)
#'
#' ## Extract the data corresponding to the first reader in the first
#' ## treatment paired with the data
#' ## from the third reader in the second treatment
#' ## (the bin indices are at different positions in the two arrays)
#' dataset12_13 <- DfExtractCorCbmDataset(dataset05,
#' trts = c(1,2), rdrs = c(1,3))
#' }
#' @export

DfExtractCorCbmDataset <- function(dataset, trts = 1, rdrs = 1){

  dataset <- DfBinDataset(dataset, desiredNumBins = 5, opChType = "ROC")
  if (dataset$descriptions$type != "ROC") {
    stop("This program requires an ROC dataset")
  }
  K <- length(dataset$ratings$NL[1,1,,1])
  K2 <- length(dataset$ratings$LL[1,1,,1])

  lt <- length(trts);lr <- length(rdrs)
  if ((lt == 1) && (lr == 2)){
    ds <- DfExtractDataset(dataset, trts, rdrs)
    ds$descriptions$modalityID <- "1"
    ds$descriptions$readerID <- c("1", "2")
    ds$descriptions$fileName <- paste0("DfExtractCorCbmDataset(", ds$descriptions$fileName,")")
    return(ds)
  } else if ((lt == 2) && (lr == 1)) {
    ds <- DfExtractDataset(dataset, trts, rdrs)
    NL <- ds$ratings$NL;dim(NL) <- c(1,2,K,1)
    LL <- ds$ratings$LL;dim(LL) <- c(1,2,K2,1)
    ds$ratings$NL <- NL;ds$ratings$LL <- LL
    ds$descriptions$modalityID <- "1"
    ds$descriptions$readerID <- c("1", "2")
    ds$descriptions$fileName <- paste0("DfExtractCorCbmDataset(", ds$descriptions$fileName,")")
    return(ds)
  } else if ((lt == 2) && (lr == 2)) {
    for (i in 1:lt){
      for (j in 1:lr) {
        if (j != i){
          if (j > i) {
            dsX <- DfExtractDataset(dataset, trts = trts[i], rdrs = rdrs[j])
          } else {
            dsY <- DfExtractDataset(dataset, trts = trts[i], rdrs = rdrs[j])
          }
        }
      }
    }
    NL <- rbind(dsX$ratings$NL,dsY$ratings$NL);dim(NL) <- c(1,2,K,1)
    LL <- rbind(dsX$ratings$LL,dsY$ratings$LL);dim(LL) <- c(1,2,K2,1)
    ds <- dsX
    ds$ratings$NL <- NL;ds$ratings$LL <- LL
    ds$descriptions$modalityID <- "1"
    ds$descriptions$readerID <- c("1", "2")
    ds$descriptions$fileName <- paste0("DfExtractCorCbmDataset(", ds$descriptions$fileName,")")
    return(ds)
  } else stop("Illegal combination of treatments and readers")
  
}


