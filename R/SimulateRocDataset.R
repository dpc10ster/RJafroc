#' Simulates a binormal model ROC dataset
#' 
#' @description  Simulates an uncorrelated binormal model ROC factorial dataset
#' 
#' @param I     The number of modalities, default is 1
#' @param J     The number of readers, default is 1
#' @param K1     The number of non-diseased cases
#' @param K2     The number of diseased cases
#' @param a      The \eqn{a} parameter of the binormal model
#' @param b      The \eqn{b} parameter of the binormal model
#' @param seed  The initial seed, default is NULL, which results in a random seed
#'  
#' @return An ROC dataset
#' 
#' @details See book Chapter 6 for details
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @examples 
#' K1 <- 5;K2 <- 7;a <- 1.5;b <- 0.5
#' rocDataRaw <- SimulateRocDataset(K1 = K1, K2 = K2, a = a, b = b)
#'   
#' @importFrom stats rnorm
#' 
#' @export
SimulateRocDataset <- function(I = 1, J = 1, K1, K2, a, b, seed = NULL){
  
  if (!is.null(seed)) set.seed(seed)
  
  NL <- array(dim = c(I, J, K1+K2, 1))
  LL <- array(dim = c(I, J, K2, 1))
  
  mu <- a/b
  sigma <- 1/b
  K <- K1 + K2
  
  for (i in 1:I) {
    for (j in 1:J) {
      NL[i,j,1:K1,1] <- rnorm(K1)
      LL[i,j,,1] <- rnorm(K2) * sigma + mu
    }
  }
  
  fileName <- "NA"
  name <- NA
  design <- "FCTRL"
  truthTableStr <- NA
  type <- "ROC"
  perCase <- rep(1,K2)
  IDs <- perCase; dim(perCase) <- c(K2,1)
  weights <- IDs
  modalityID <- as.character(1:I)
  readerID <- as.character(1:J)
  dataset <- convert2dataset(NL, LL, LL_IL = NA, 
                              perCase, IDs, weights,
                              fileName, type, name, truthTableStr, design,
                              modalityID, readerID) 
  return(dataset)
}