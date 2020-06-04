#' Convert an LROC dataset to a ROC dataset
#' 
#'  
#' @description Converts an LROC dataset to an ROC dataset
#'  
#' 
#' @param dataset The \strong{LROC} dataset to be converted.
#' 
#' @details For the diseased cases one takes the maximum rating on each diseased case, 
#'    which could be a LL ("true positive" correct localization) or a LL_IL 
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
  stop("need fix here: DfLroc2Roc")
  # TBA SimplifyDatasets
  
  if (dataset$dataType != "LROC") stop("This function requires an LROC dataset.")
  
  LL <- dataset$LL[,,,1]
  LL_IL <- dataset$LL_IL[,,,1]
  I <- length(dataset$LL[,1,1,1])
  J <- length(dataset$LL[1,,1,1])
  K2 <- length(dataset$LL[1,1,,1])
  
  dim(LL) <- c(I,J,K2)
  dim(LL_IL) <- c(I,J,K2)
  
  LL <- dataset$LL
  dim(LL) <- c(I,J,K2,1)
  
  for (i in 1:I) {
    for (j in 1:J) {
      # For the diseased cases one takes the maximum rating on each diseased case, 
      #    which could be a LL ("true positive" correct localization) or a LL_IL 
      #    ("true positive" incorrect localization) rating, whichever has the higher rating.
      LL[i,j,,1] <- pmax(LL[i,j,,1],LL_IL[i,j,])
    }
  }
  
  # TBA SimplifyDatasets
  return (list(
    NL = dataset$ratings$NL, # For non-diseased cases the NL arrays are identical.
    LL = LL,
    lesionVector = dataset$lesionVector,
    lesionID = dataset$lesionID,
    lesionWeight = dataset$lesionWeight,
    dataType = "ROC",
    modalityID = dataset$descriptions$modalityID,
    readerID = dataset$descriptions$readerID,
    datasetName = "ignore"
  ))
}

