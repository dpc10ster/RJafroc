#' Simulates an "AUC-equivalent" FROC dataset from an LROC dataset
#' 
#' @description  Simulates a multiple-modality multiple-reader 
#'    "AUC-equivalent" FROC dataset from a supplied LROC dataset,
#'    e.g., \link{datasetCadLroc}.
#' 
#' @param dataset The LROC dataset to be converted to FROC.
#'  
#' @return The AUC-equivalent FROC dataset
#' 
#' @details The LROC paradigm always yields a single mark per case. Therefore 
#'    the equivalent FROC will also have only one mark per case. The NL arrays 
#'    of the two datasets are identical. The LL array is created by copying the
#'    LLCl array of the LROC dataset to the LL array of the FROC dataset, from 
#'    diseased case index k2 = 1 to k2 = K2. Additionally, the LLIl array of the 
#'    LROC dataset is copied to the NL array of the FROC dataset, starting at case 
#'    index k1 = K1+1 to k1 = K1+K2. Any zero ratings are replace by -Infs. The 
#'    equivalent FROC dataset has the same HrAuc as the original LROC dataset. 
#'    See example. The main use of this function is to test the CAD significance
#'    testing functions using CAD FROC datasets, which I currently don't have.
#' 
#' @examples 
#' 
#' frocDataset <- DfLroc2Froc(datasetCadLroc)
#' lrocAuc <- UtilFigureOfMerit(datasetCadLroc, FOM = "Wilcoxon")
#' frocHrAuc <- UtilFigureOfMerit(frocDataset, FOM = "HrAuc")   
#' 
#' @export

DfLroc2Froc <- function(dataset)  #  !!!in tests!!!
{
  if (dataset$descriptions$type != "LROC") 
    stop("This function requires an LROC dataset")
  
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  
  I <- length(NL[,1,1,1])
  J <- length(NL[1,,1,1])
  K <- length(NL[1,1,,1])
  K2 <- length(LL[1,1,,1])
  K1 <- K - K2
  
  NL <- array(-Inf, dim = c(I,J,K,1))
  LL <- array(-Inf, dim = c(I,J,K2,1))
  
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K1) {
        NL[i,j,k,1] <- dataset$ratings$NL[i,j,k,1]
      }
    }
  }
  
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K2) {
        NL[i,j,k+K1,1] <- dataset$ratings$LL_IL[i,j,k,1]
      }
    }
  }
  
  for (i in 1:I) {
    for (j in 1:J) {
      for (k in 1:K2) {
        LL[i,j,k,1] <- dataset$ratings$LL[i,j,k,1]
      }
    }
  }
  
  NL[NL == 0] <- -Inf
  LL[LL == 0] <- -Inf
  
  weights <- dataset$lesions$weights
  weights[,1] <- 1
  
  fileName <- paste0("DfLroc2Froc(", dataset$descriptions$fileName, ")")
  name <- dataset$descriptions$name
  design <- dataset$descriptions$design
  truthTableStr <- dataset$descriptions$truthTableStr
  IDs <- dataset$lesions$IDs
  # weights <- dataset$lesions$weights
  type <- "FROC"
  perCase <- dataset$lesions$perCase
  modalityID <- dataset$descriptions$modalityID
  readerID <- dataset$descriptions$readerID
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
  
}



