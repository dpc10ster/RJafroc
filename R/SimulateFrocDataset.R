#' Simulates an MRMC uncorrelated FROC dataset using the RSM
#' 
#' @description  Simulates an uncorrelated MRMC FROC dataset for specified numbers of
#'    readers and treatments 
#' 
#' @param mu     mu parameter of the RSM
#' @param lambda RSM lambda parameter
#' @param nu     RSM nu parameter
#' @param zeta1  Lowest reporting threshold
#' @param I      Number of treatments, default is 1
#' @param J      Number of readers
#' @param K1     Number of non-diseased cases
#' @param K2     Number of diseased cases
#' @param perCase A K2 length array containing the numbers of lesions per diseased case
#' @param seed  Initial seed for RNG, default \code{NULL} for random seed.
#' @param deltaMu Inter-modality increment in mu, default zero 
#' 
#' @return An FROC dataset.
#' 
#' @details See book chapters on the Radiological Search Model (RSM) for details. 
#'    Correlations between ratings on the same case are assumed to be zero.
#' 
#' @examples
#' set.seed(1) 
#' K1 <- 5;K2 <- 7;
#' maxLL <- 2;perCase <- floor(runif(K2, 1, maxLL + 1))
#' mu <- 1;lambda <- 1;nu <- 0.99 ;zeta1 <- -1
#' I <- 2; J <- 5
#' 
#' frocDataRaw <- SimulateFrocDataset(
#'   mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1,
#'   I = I, J = J, K1 = K1, K2 = K2, perCase = perCase )
#'   
#' ## plot the data
#' ret <- PlotEmpOpChr(frocDataRaw, opChType = "FROC")
#' ## print(ret$Plot)
#' 
#' @references 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @importFrom stats rpois rnorm rbinom
#' 
#' @export

SimulateFrocDataset <- function(mu, lambda, nu, zeta1, I, J, K1, K2, perCase, seed = NULL, deltaMu = 0){
  
  if (length(perCase) != K2) stop("SimulateFrocDataset: error in specification of number of lesions `perCase` vector.")
  
  K <- K1 + K2
  if (I > 1) {
    temp <- array(0, dim = I)
    temp[2:I] <- deltaMu
    deltaMu <- temp
  }  
  
  if (!is.null(seed)) set.seed(seed)
  
  # find maxNL
  nNL <- array(dim = c(I,J,K1+K2))
  for (i in 1:I) {
    # get intrinsic parameters
    par_i <- Util2Intrinsic(mu, lambda, nu) # intrinsic
    # find physical parameters for increased muNH
    par_p <- Util2Physical(mu + deltaMu[i], par_i$lambda_i, par_i$nu_i)  # physical
    for (j in 1:J) {  
      for (k in 1:(K1 + K2)) {
        nNL[i,j,k] <- rpois(1, par_p$lambda)
      }
    }
  }
  maxNL <- max(nNL)
  
  NL <- array(-Inf, dim = c(I, J, K1 + K2, maxNL))
  for (i in 1:I) {
    for (j in 1:J) {  
      for (k in 1:(K1 + K2)) {
        if (nNL[i,j,k] > 0) {
          rNL <- rnorm(nNL[i,j,k])
          rNL <- rNL[order(rNL, decreasing = TRUE)]
          rNL[rNL < zeta1] <- -Inf
          NL[i,j,k, ] <- c(rNL, rep(-Inf, maxNL - nNL[i,j,k]))
        }
      }
    }
  }
  
  maxLL <- max(perCase)
  LL <- array(-Inf, dim = c(I,J,K2, maxLL))
  for (i in 1:I) {
    # get intrinsic parameters
    par_i <- Util2Intrinsic(mu, lambda, nu) # intrinsic
    # find physical parameters for increased muNH
    par_p <- Util2Physical(mu + deltaMu[i], par_i$lambda_i, par_i$nu_i)  # physical
    for (j in 1:J) {  
      for (k in 1:K2){
        nLL <- rbinom(1, perCase[k], par_p$nu)
        if (nLL > 0) {
          rLL <- rnorm(nLL, mu + deltaMu[i])
          rLL <- rLL[order(rLL, decreasing = TRUE)]
          rLL[rLL < zeta1] <- -Inf
          LL[i,j,k, ] <- c(rLL, rep(-Inf, maxLL - nLL))
        }
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
  type <- "FROC"
  
  if (type == "FROC") {
    # added truthTableStr 5/18/2023
    truthTableStr <- array(dim = c(I, J, K, maxLL+1))
    truthTableStr[,,1:K1,1] <- 1
    for (k2 in 1:K2) {
      truthTableStr[,,k2+K1,(1:perCase[k2])+1] <- 1
    }
  } else if (type == "ROC") {
    # added truthTableStr 5/18/2023
    truthTableStr <- array(dim = c(I, J, K, 2)) 
    truthTableStr[1:I, 1:J, 1:K1, 1] <- 1
    truthTableStr[1:I, 1:J, (K1+1):K, 2] <- 1
  } else stop("data type must be ROC or FROC")
  
  return(convert2dataset(NL, LL, LL_IL = NA, 
                         perCase, IDs, weights,
                         fileName, type, name, truthTableStr, design,
                         modalityID, readerID))
}


