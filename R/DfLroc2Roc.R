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
#' rocDataSet <- DfLroc2Roc(datasetCadLroc)
#' 
#' 
#' @export

DfLroc2Roc <- function (dataset) #  TBA !!!in tests!!!
{
  if (dataset$descriptions$type != "LROC") stop("This function requires an LROC dataset.")
  
  LL <- dataset$ratings$LL[,,,1]
  LL_IL <- dataset$ratings$LL_IL[,,,1]
  I <- length(dataset$ratings$LL[,1,1,1])
  J <- length(dataset$ratings$LL[1,,1,1])
  K2 <- length(dataset$ratings$LL[1,1,,1])
  
  dim(LL) <- c(I,J,K2,1)
  dim(LL_IL) <- c(I,J,K2)
  
  for (i in 1:I) {
    for (j in 1:J) {
      # For the diseased cases one takes the maximum rating on each diseased case, 
      #    which could be a LL ("true positive" correct localization) or a LL_IL 
      #    ("true positive" incorrect localization) rating, whichever has the higher rating.
      LL[i,j,,1] <- pmax(LL[i,j,,1],LL_IL[i,j,])
    }
  }
  
  NL <- dataset$ratings$NL
  fileName <- paste0("DfLroc2Roc(", dataset$descriptions$fileName, ")")
  name <- NA
  design <- "FCTRL"
  truthTableStr <- NA
  type <- "ROC"
  perCase <- rep(1,K2)
  IDs <- dataset$lesions$IDs
  weights <- dataset$lesions$weights
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
}

