#' Simulates a binormal model ROC dataset
#' 
#' @description  Simulates a binormal model ROC dataset for a single treatment and reader
#' 
#' @param K1     The number of non-diseased cases
#' @param K2     The number of diseased cases
#' @param a      The \eqn{a} parameter of the binormal model
#' @param b      The \eqn{b} parameter of the binormal model
#' @param seed  The initial seed, default is NULL
#'  
#' @return An ROC dataset
#' 
#' @details See book Chapter 6 for details
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @examples 
#' K1 <- 5;K2 <- 7;
#' a <- 1.5;b <- 0.5
#' 
#' rocDataRaw <- SimulateRocDataset(K1 = K1, K2 = K2,
#'    a = a, b = b)
#'   
#' ## plot the data
#' ret <- PlotEmpiricalOperatingCharacteristics(rocDataRaw, trts= 1, rdrs = 1, opChType = "ROC")
#' print(ret$Plot)
#' 
#' @importFrom stats rnorm
#' 
#' @export
SimulateRocDataset <- function(K1, K2, a, b, seed = NULL){
  if (!is.null(seed)) set.seed(seed)
  mu <- a/b
  sigma <- 1/b
  K <- K1 + K2
  z1 <- rnorm(K1)
  z2 <- rnorm(K2) * sigma + mu
  dataset <- Df2RJafrocDataset(z1, z2)
  return(dataset)
}