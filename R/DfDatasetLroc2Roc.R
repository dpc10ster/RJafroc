#' Convert an LROC dataset to a ROC dataset
#' 
#'  
#' @description Converts an LROC dataset to an ROC dataset
#'  
#' 
#' @param dataset The \strong{LROC} dataset to be converted.
#' 
#' @details The conversion is effected by taking the maximum rating on each diseased case, 
#'    which could be a TPCl (true positive correct localization) or a TPIl 
#'    (true positive incorrect localization) rating, whichever has the higher rating.
#' 
#' 
#' @return An ROC dataset  
#'
#' @examples
#' str(datasetCadLroc)
#' rocDataSet <- DfDatasetLroc2Roc(datasetCadLroc)
#' str(rocDataSet)
#' 
#' 
#' @export

DfDatasetLroc2Roc <- function (dataset){

  if (dataset$dataType != "LROC") stop("This function requires an LROC dataset.")
  
  LLCl <- dataset$LLCl[1,,,1]
  LLIl <- dataset$LLIl[1,,,1]
  I <- length(dataset$LLCl[,1,1,1])
  J <- length(dataset$LLCl[1,,1,1])
  K2 <- length(dataset$LLCl[1,1,,1])
  
  LL <- pmax(LLCl,LLIl)
  dim(LL) <- c(I, J, K2, 1)
  
  return (list(
    NL = dataset$NL,
    LL = LL,
    lesionVector = dataset$lesionVector,
    lesionID = dataset$lesionID,
    lesionWeight = dataset$lesionWeight,
    dataType = "ROC",
    modalityID = dataset$modalityID,
    readerID = dataset$readerID
  ))
}

