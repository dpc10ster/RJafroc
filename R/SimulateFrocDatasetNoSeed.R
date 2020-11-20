#' Simulates an MRMC uncorrelated FROC dataset using the RSM - no seed version
#' 
#' @description  Simulates an uncorrelated MRMC FROC dataset for specified numbers of
#'    readers and treatments 
#' 
#' @param mu     The mu parameter of the RSM
#' @param lambda The intrinsic lambda parameter of the RSM (not the physical parameter)
#' @param nu     The intrinsic nu parameter of the RSM (not the physical parameter)
#' @param zeta1  The lowest reporting threshold
#' @param I      The number of treatments
#' @param J      The number of readers
#' @param K1     The number of non-diseased cases
#' @param K2     The number of diseased cases
#' @param perCase    A K2 length array containing the numbers of lesions per diseased case
#' 
#' @return The return value is an FROC dataset.
#' 
#' @details See book chapters on the Radiological Search Model (RSM) for details. 
#'    In this code correlations between ratings on the same case are assumed to be zero.
#' 
#' 
#' 
#' @importFrom stats rpois rnorm rbinom
#' 
#' @export

SimulateFrocDatasetNoSeed <- function(mu, lambda, nu, zeta1, I, J, K1, K2, perCase){
  
  if (length(perCase) != K2) stop("SimulateFrocDataset: error in specification of number of lesions perCase vector.")
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
  
  maxLL <- max(perCase)
  LL <- array(-Inf, dim = c(I,J,K2, maxLL))
  
  for (i in 1:I) {
    for (j in 1:J) {  
      for (k in 1:K2){
        nLL <- rbinom(1, perCase[k], nuP)
        ll <- rnorm(nLL, mu)
        ll <- ll[order(ll, decreasing = TRUE)]
        ll[ll < zeta1] <- -Inf
        LL[i,j,k, ] <- c(ll, rep(-Inf, maxLL - nLL))
      }
      
      IDs <- array(dim = c(K2, maxLL))
      weights <- array(dim = c(K2, maxLL))
      for (k in 1:K2){
        IDs[k, ] <- c(1:perCase[k], rep(-Inf, maxLL - perCase[k]))
        weights[k, ] <- c(rep(1 / perCase[k], perCase[k]), rep(-Inf, maxLL - perCase[k]))
      }
    }
  }  
  modalityID <- as.character(seq(1:I))
  readerID <- as.character(seq(1:J))
  fileName <- "NA"
  name <- NA
  design <- "FCTRL"
  truthTableStr <- NA
  type <- "FROC"
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
}
