#' Simulates an "AUC-equivalent" equivalent LROC dataset from a FROC dataset
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
#'    of the LROC data, for non-disesed casese the \bold{highest} rating of the 
#'    FROC marks, or -Inf if there are no marks, is copied to case index 
#'    k1 = 1 to k1 = K1 of the LROC dataset. The LL array is created by copying the
#'    LL array of the FROC dataset to the LLCl array of the LROC dataset, from 
#'    diseased case index k2 = 1 to k2 = K2. For diseased cases, the 
#'    \bold{highest} rated NL 
#'    array of the FROC dataset is copied to the NL array of the FROC dataset, 
#'    starting at case index k1 = K1+1 to k1 = K1+K2. Any zero ratings are replace by -Infs. The 
#'    equivalent FROC dataset has the same HrAuc as the original LROC dataset. 
#'    See example. The main use of this function is to test the Significance 
#'    testing functions using MRMC LROC datasets, which I currently don't have. The ROC AUCs
#'    under the two datasets are guaranteed to be identical.
#' 
#' @examples 
#' 
#' lrocDataset <- SimulateLrocFromFrocDataset(dataset05)
#' frocHrAuc <- UtilFigureOfMerit(dataset05, FOM = "HrAuc")   
#' lrocWilcoxonAuc <- UtilFigureOfMerit(lrocDataset, FOM = "Wilcoxon")
#' 
#' @export

SimulateLrocFromFrocDataset <- function(dataset) #  !!!in tests!!!
{
  if (dataset$dataType != "FROC") 
    stop("This function requires an FROC dataset")
  
  I <- length(dataset$NL[,1,1,1])
  J <- length(dataset$NL[1,,1,1])
  K <- length(dataset$NL[1,1,,1])
  K2 <- length(dataset$LL[1,1,,1])
  K1 <- K - K2
  NL <- apply(dataset$NL, c(1, 2, 3), max)# keep max NL rating
  dim(NL) <- c(dim(NL), 1) # add the fourth "unnecessary" dimension
  
  LLCl <- array(-Inf, dim = c(I,J,K2,1))
  LLIl <- array(-Inf, dim = c(I,J,K2,1))
  
  LL <- dataset$LL
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K2) {
        maxLL <- max(LL[i,j,k,])
        maxNL <- NL[i,j,k+K1,1]
        if (maxLL > maxNL) LLCl[i,j,k,1] <- maxLL else  LLIl[i,j,k,1] <- maxNL 
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



