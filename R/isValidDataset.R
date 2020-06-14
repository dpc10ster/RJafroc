#' Check the validity of a dataset
#' 
#' @description Checks the validity of the dataset. 
#' 
#' 
#' @param dataset The dataseet object to be checked. 
#' 
#' @return \code{TRUE} if dataset is valid, \code{FALSE} otherwise.
#' 
#' @export
#' 
isValidDataset <- function(dataset) {
  
  if (typeof(dataset) != "list") return (FALSE)
  if (!(length(dataset) == 3)) return (FALSE)
  if (!(all(names(dataset) == c("ratings", "lesions", "descriptions")))) return (FALSE)
  
  if (!(all(names(dataset$ratings) == c("NL", "LL", "LL_IL")))) return (FALSE)
  if (!(all(names(dataset$lesions) == c("perCase", "IDs", "weights")))) return (FALSE)
  if (dataset$descriptions$design != "FCTRL-X-MOD") {
    if (!(all(names(dataset$descriptions) == c("fileName", "type",
                                         "name", "truthTableStr", "design",
                                         "modalityID", "readerID")))) return (FALSE)
  } else {
    if (!(all(names(dataset$descriptions) == c("fileName", "type",
                                         "name", "truthTableStr", "design",
                                         "modalityID1", "modalityID2", "readerID")))) return (FALSE)
  }
  
  if (!(dataset$descriptions$type) %in% c("ROC", "FROC", "LROC", "ROI"))  return (FALSE)
  
  return (TRUE)
  
}
