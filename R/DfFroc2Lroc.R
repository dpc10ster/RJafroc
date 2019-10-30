#' Simulates an "AUC-equivalent" LROC dataset from an FROC dataset
#' 
#' @description  Simulates a multiple-treatment multiple-reader "AUC-equivalent" 
#'    LROC dataset from a supplied FROC dataset.
#' 
#' @param dataset The FROC dataset to be converted to LROC.
#'  
#' @return The equivalent LROC dataset
#' 
#' @details The FROC paradigm can have 0 or more marks per case. However, 
#'    LROC is restricted to \bold{exactly one mark per case}. For the NL array 
#'    of the LROC data, for non-disesed cases, the \bold{highest} rating of the 
#'    FROC marks, or -Inf if there are no marks, is copied to case index 
#'    k1 = 1 to k1 = K1 of the LROC dataset. For each diseased case, if the 
#'    max LL rating exceeds the max NL rating, then the max LL rating is copied 
#'    to the LLCl array, otherwise the max NL rating is copied to the LLIl array. 
#'    The max NL rating on each diseased case is then set to -Inf (since the LROC
#'    paradigm only allows one mark. The equivalent FROC dataset has the same 
#'    HrAuc as the original LROC dataset. See example. The main use of this 
#'    function is to test the Significance testing functions using MRMC LROC 
#'    datasets, which I currently don't have.
#' 
#' @examples 
#' 
#' lrocDataset <- DfFroc2Lroc(dataset05)
#' frocHrAuc <- UtilFigureOfMerit(dataset05, FOM = "HrAuc")   
#' lrocWilcoxonAuc <- UtilFigureOfMerit(lrocDataset, FOM = "Wilcoxon")
#' 
#' @export

DfFroc2Lroc <- function(dataset) #  !!!in tests!!!  test-LrocDfConversionFunctions.R
{
  if (dataset$dataType != "FROC") 
    stop("This function requires an FROC dataset")
  
  I <- length(dataset$NL[,1,1,1])
  J <- length(dataset$NL[1,,1,1])
  K <- length(dataset$NL[1,1,,1])
  K2 <- length(dataset$LL[1,1,,1])
  K1 <- K - K2
  #  For the NL array of the LROC data, for non-disesed cases, the highest rating of the 
  #  FROC marks, or a minimum rating value (less than lowest finite rating, see below) 
  #  if there are no marks, is copied to case index k1 = 1 to k1 = K1 of the LROC dataset.   

  NL <- apply(dataset$NL, c(1, 2, 3), max)# keep max NL rating; 
  dim(NL) <- c(dim(NL), 1) # add the fourth "unnecessary" dimension
  
  LL <- dataset$LL
  lowestRating <- min(min(NL[is.finite(NL)]), min(LL[is.finite(LL)]))
  NL[,,1:K1,1][!is.finite(NL[,,1:K1,1])] <- lowestRating - 10 # less than lowest finite rating
  # increased from one to ten to make it stand out
  
  # the entries for diseased cases are set to -Inf below ...
  # after assigning to LLIl array (if it excees the max LL rating)
  
  LLCl <- array(lowestRating - 10, dim = c(I,J,K2,1)) # ensure a finite LLCl rating on each case
  LLIl <- array(lowestRating - 10, dim = c(I,J,K2,1)) # ensure a finite LLIl rating on each case
  
  #  For each diseased case, if the 
  #  max LL rating exceeds the max NL rating, then the max LL rating is copied 
  #  to the LLCl array, otherwise the max NL rating is copied to the LLIl array. 
  #  Then the max NL rating on the diseased case is set to -Inf (since the LROC
  #  paradigm only allows one mark).
  LL <- dataset$LL
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K2) {
        maxLL <- max(LL[i,j,k,]) # max LL
        maxNL <- NL[i,j,k+K1,1] # max NL on diseased case
        if (maxLL > maxNL) # assign to appropriate array
        {
          LLCl[i,j,k,1] <- maxLL # assign to LLCl array
        } 
        else  {
          LLIl[i,j,k,1] <- maxNL # assign to LLIl array
        } 
        NL[i,j,k+K1,1] <- -Inf # LROC NL array for diseased case is -Inf
        # commenting above line does not affect FOM results
      }
    }
  }
  
  lesVector <- array(1, dim = c(K2))
  lesID <- array(1, dim = c(K2,1))
  lesWghts <- array(1, dim = c(K2,1))
  lesWghts[,1] <- 1
  
  datasetFroc <- list(
    NL = NL,
    LLCl = LLCl,
    LLIl = LLIl,
    lesionVector = lesVector,
    lesionID = lesID,
    lesionWeight = lesWghts,
    dataType = "LROC",
    modalityID = dataset$modalityID,
    readerID = dataset$readerID
  )
  
  return (datasetFroc)
  
}



