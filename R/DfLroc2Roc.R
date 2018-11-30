#' Convert an LROC dataset to a ROC dataset
#' 
#'  
#' @description Converts an LROC dataset to a ROC dataset
#'  
#' 
#' @param dataset The \strong{LROC} dataset to be converted.
#' 
#' @details The conversion is effected by taking the maximum rating on each diseased case, 
#' which could be a TPCl or a TPIl, whichever has the higher rating.
#' 
#' 
#' @return An ROC dataset  
#'
#' @examples
#' dataset <- DfReadLrocDataFile()
#' str(dataset)
#' rocDataSet <- DfLroc2Roc(dataset)
#' str(rocDataSet)
#' 
#' 
#' @export

DfLroc2Roc <- function (dataset){
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
 
  NL <- dataset$NL[1,,,1]
  LLCl <- dataset$LLCl[1,,,1]
  LLIl <- dataset$LLIl[1,,,1]
  LL <- pmax(LLCl,LLIl)
  K1 <- length(NL[1,])
  K2 <- length(LLCl[1,])
  K1 <- K1 - K2  # sic
  J <- length(NL[,1])
  dim(LL) <- c(1, J, K2, 1)

  return (list(
    NL = dataset$NL,
    LL = LL,
    lesionNum = dataset$lesionNum,
    lesionID = dataset$lesionID,
    lesionWeight = dataset$lesionWeight,
    dataType = "ROC",
    modalityID = dataset$modalityID,
    readerID = dataset$readerID
  ))
}

