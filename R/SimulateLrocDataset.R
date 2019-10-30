#' Simulates an uncorrelated FLROC FrocDataset using the RSM
#' 
#' @description  Simulates an uncorrelated LROC dataset for specified numbers of
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
#' @return The return value is an LROC dataset.
#' 
#' @details See book chapters on the Radiological Search Model (RSM) for details.
#'    The spproach is to first simulate an FROC dataset and then convert it to an Lroc
#'    dataset. The correlations between FROC ratings on the same case are assumed to be zero.
#' 
#' @examples
#'   set.seed(1)
#'   K1 <- 5
#'   K2 <- 5
#'   mu <- 2
#'   lambda <- 1
#'   lesionVector <- rep(1, 5)
#'   nu <- 0.8
#'   zeta1 <- -3
#'   frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, lesionVector)
#'   lrocData <- DfFroc2Lroc(frocData)
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @importFrom stats rpois rnorm rbinom
#' 
#' @export

SimulateLrocDataset <- function(mu, lambda, nu, zeta1, I, J, K1, K2, lesionVector){
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
  FrocDataset <- list(
    NL = NL, 
    LL = LL,
    lesionVector = lesionVector,
    lesionID = lesID,
    lesionWeight = lesWght,
    dataType = "FROC",
    modalityID = modalityID,
    readerID = readerID
  )
  
  lrocDataSet <- DfFroc2Lroc(FrocDataset) # convert to LROC dataset
   
  return(lrocDataSet)
}