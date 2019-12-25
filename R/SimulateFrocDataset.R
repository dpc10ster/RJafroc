#' Simulates an MRMC uncorrelated FROC dataset using the RSM
#' 
#' @description  Simulates an uncorrelated MRMC FROC dataset for specified numbers of
#'    readers and treatments 
#' 
#' @param mu     The intrinsic mu parameter of the RSM
#' @param lambda The intrinsic lambda parameter of the RSM (not the physical parameter)
#' @param nu     The intrinsic nu parameter of the RSM (not the physical parameter)
#' @param zeta1  The lowest reporting threshold
#' @param I      The number of treatments
#' @param J      The number of readers
#' @param K1     The number of non-diseased cases
#' @param K2     The number of diseased cases
#' @param lesionVector    A K2 length array containing the numbers of lesions per diseased case
#' 
#' @return The return value is an FROC dataset.
#' 
#' @details See book chapters on the Radiological Search Model (RSM) for details. 
#'    In this code correlations between ratings on the same case are assumed to be zero.
#' 
#' @examples
#' set.seed(1) 
#' K1 <- 5;K2 <- 7;
#' maxLL <- 2;lesionVector <- floor(runif(K2, 1, maxLL + 1))
#' mu <- 1;lambda <- 1;nu <- 1 ;zeta1 <- -1
#' I <- 2; J <- 5
#' 
#' frocDataRaw <- SimulateFrocDataset(
#'   mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
#'   I = I, J = J, K1 = K1, K2 = K2, lesionVector = lesionVector )
#'   
#' ## plot the data
#' ret <- PlotEmpiricalOperatingCharacteristics(frocDataRaw, opChType = "FROC")
#' print(ret$Plot)
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @importFrom stats rpois rnorm rbinom
#' 
#' @export

SimulateFrocDataset <- function(mu, lambda, nu, zeta1, I, J, K1, K2, lesionVector){
  lambdaP <- lambda/mu
  nuP <- 1-exp(-nu*mu)
  nNL <- rpois(I * J * (K1 + K2), lambdaP)
  dim(nNL) <- c(I,J,K1+K2)
  maxNL <- max(nNL)
  NL <- array(-Inf, dim = c(I, J, K1 + K2, maxNL))
  for (i in 1:I) {
    for (j in 1:J) {  
      for (k in 1:(K1 + K2)) {
        nl <- rnorm(nNL[i,j,k])
        nl <- nl[order(nl, decreasing = TRUE)]
        nl[nl < zeta1] <- -Inf
        NL[i,j,k, ] <- c(nl, rep(-Inf, maxNL - nNL[i,j,k]))
      }
    }
  }
  
  maxLL <- max(lesionVector)
  LL <- array(-Inf, dim = c(I,J,K2, maxLL))
  
  for (i in 1:I) {
    for (j in 1:J) {  
      for (k in 1:K2){
        nLL <- rbinom(1, lesionVector[k], nuP)
        ll <- rnorm(nLL, mu)
        ll <- ll[order(ll, decreasing = TRUE)]
        ll[ll < zeta1] <- -Inf
        LL[i,j,k, ] <- c(ll, rep(-Inf, maxLL - nLL))
      }
      
      lesID <- array(dim = c(K2, maxLL))
      lesWght <- array(dim = c(K2, maxLL))
      for (k in 1:K2){
        lesID[k, ] <- c(1:lesionVector[k], rep(-Inf, maxLL - lesionVector[k]))
        lesWght[k, ] <- c(rep(1 / lesionVector[k], lesionVector[k]), rep(-Inf, maxLL - lesionVector[k]))
      }
    }
  }  
  modalityID <- as.character(seq(1:I))
  readerID <- as.character(seq(1:J))
  dataset <- list(
    NL = NL, 
    LL = LL,
    lesionVector = lesionVector,
    lesionID = lesID,
    lesionWeight = lesWght,
    dataType = "FROC",
    modalityID = modalityID,
    readerID = readerID
  )
  return(dataset)
}