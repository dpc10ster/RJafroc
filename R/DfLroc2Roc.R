#' Convert an LROC dataset to a ROC dataset
#' 
#'  
#' @description Converts an LROC dataset to an ROC dataset
#'  
#' 
#' @param dataset The \strong{LROC} dataset to be converted.
#' 
#' @details For the diseased cases one takes the maximum rating on each diseased case, 
#'    which could be a LLCl ("true positive" correct localization) or a LLIl 
#'    ("true positive" incorrect localization) rating, whichever has the higher rating.
#'    For non-diseased cases the NL arrays are identical.
#' 
#' 
#' @return An ROC dataset  
#'
#' @examples
#' str(datasetCadLroc)
#' rocDataSet <- DfLroc2Roc(datasetCadLroc)
#' str(rocDataSet)
#' 
#' 
#' @export

DfLroc2Roc <- function (dataset) #  !!!in tests!!!
{
  
  if (dataset$dataType != "LROC") stop("This function requires an LROC dataset.")
  
  LLCl <- dataset$LLCl[,,,1]
  LLIl <- dataset$LLIl[,,,1]
  I <- length(dataset$LLCl[,1,1,1])
  J <- length(dataset$LLCl[1,,1,1])
  K2 <- length(dataset$LLCl[1,1,,1])
  
  dim(LLCl) <- c(I,J,K2)
  dim(LLIl) <- c(I,J,K2)
  
  LL <- dataset$LLCl
  dim(LL) <- c(I,J,K2,1)
  
  for (i in 1:I) {
    for (j in 1:J) {
      # For the diseased cases one takes the maximum rating on each diseased case, 
      #    which could be a LLCl ("true positive" correct localization) or a LLIl 
      #    ("true positive" incorrect localization) rating, whichever has the higher rating.
      LL[i,j,,1] <- pmax(LLCl[i,j,],LLIl[i,j,])
    }
  }
  
  return (list(
    NL = dataset$NL, # For non-diseased cases the NL arrays are identical.
    LL = LL,
    lesionVector = dataset$lesionVector,
    lesionID = dataset$lesionID,
    lesionWeight = dataset$lesionWeight,
    dataType = "ROC",
    modalityID = dataset$modalityID,
    readerID = dataset$readerID
  ))
}

