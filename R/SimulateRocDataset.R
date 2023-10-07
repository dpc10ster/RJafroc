#' Simulates a binormal model ROC dataset
#' 
#' @description  Simulates an uncorrelated binormal model ROC factorial dataset
#' 
#' @param I     Number of modalities, default 1
#' @param J     The number of readers, default 1
#' @param K1    Number of non-diseased cases
#' @param K2    Number of diseased cases
#' @param a     \eqn{a} parameter of binormal model
#' @param deltaA Inter-modality increment in the \eqn{a} parameter, 
#'   default zero
#' @param b      \eqn{b} parameter of the binormal model
#' @param seed  Initial seed, default is NULL, for random seed
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
SimulateRocDataset <- function(I = 1, J = 1, K1, K2, a, deltaA = 0, b, seed = NULL){
  # added deltaA 5/18/2023  
  if (!is.null(seed)) set.seed(seed)

  NL <- array(dim = c(I, J, K1+K2, 1))
  LL <- array(dim = c(I, J, K2, 1))
  
  mu <- a/b
  sigma <- 1/b
  K <- K1 + K2
  if (I > 1) {
    deltaMu <- array(0, dim = I)
    deltaMu[2] <- (a+deltaA)/b
  } else deltaMu <- (a+deltaA)/b # added 5/18/2023  
  
  for (i in 1:I) {
    for (j in 1:J) {
      NL[i,j,1:K1,1] <- rnorm(K1)
      # LL[i,j,,1] <- rnorm(K2) * sigma + mu # commented 5/18/2023  
      LL[i,j,,1] <- rnorm(K2) * sigma + mu + deltaMu[i] # added 5/18/2023  
    }
  }
  
  fileName <- "NA"
  name <- NA
  design <- "FCTRL"
  type <- "ROC"
  perCase <- rep(1,K2)
  truthTableStr <- AddTruthTableStr(dataset, type, perCase) # added 9/16/2023
  IDs <- perCase; dim(IDs) <- c(K2,1) # fix 5/18/2023
  weights <- IDs
  modalityID <- as.character(1:I)
  readerID <- as.character(1:J)
  dataset <- convert2dataset(NL, LL, LL_IL = NA, 
                             perCase, IDs, weights,
                             fileName, type, name, truthTableStr, design,
                             modalityID, readerID) 
  return(dataset)
}