#' Create paired dataset for testing \code{\link{FitCorCbmRoc}}
#' 
#' @description The paired dataset is generated using bivariate sampling; 
#' details are in referenced publication 
#' 
#'    
#' @param seed The seed variable, default is 123; set to NULL for truly random seed 
#' 
#' @param K1 The number of non-diseased cases, default is 50 
#' @param K2 The number of diseased cases, default is 50 
#' @param desiredNumBins The desired number of bins; default is 5 
#' @param muX The CBM \eqn{\mu} parameter in condition X 
#' @param muY The CBM \eqn{\mu} parameter in condition Y 
#' @param alphaX The CBM \eqn{\alpha} parameter in condition X 
#' @param alphaY The CBM \samp{alpha} parameter in condition Y 
#' @param rhoNor The correlation of non-diseased case z-samples
#' @param rhoAbn2 The correlation of diseased case z-samples, when disease is
#'    visible in both conditions
#'    
#'     
#' @details The ROC data is bined to 5 bins in each condition. 
#'    
#'     
#' @return The return value is the desired dataset, suitable for testing \code{\link{FitCorCbmRoc}}.
#' 
#' 
#' 
#' @examples 
#' ## seed <- 1 
#' ## this gives unequal numbers of bins in X and Y conditions for 50/50 dataset
#' dataset <- DfCreateCorCbmDataset()
#' 
#' \dontrun{
#' ## this takes very long time!! used to show asymptotic convergence of ML estimates 
#' dataset <- DfCreateCorCbmDataset(K1 = 5000, K2 = 5000)
#' }
#' 
#' 
#' @references 
#' Zhai X, Chakraborty DP (2017) A bivariate contaminated binormal model for robust fitting of proper ROC
#' curves to a pair of correlated, possibly degenerate, ROC datasets. Medical Physics. 44(6):2207--2222.
#' 
#' @importFrom mvtnorm pmvnorm 
#' @export 
#' 
#' 
DfCreateCorCbmDataset <- function( seed = 123, K1 = 50, K2 = 50, desiredNumBins = 5, 
                                   muX = 1.5, muY = 3, alphaX = 0.4, alphaY = 0.7, 
                                   rhoNor = 0.3, rhoAbn2 = 0.8){
  
  # 50/50; 100/100; 1000/1000; 5000/5000
  
  set.seed(seed)
  
  AUCX <- 0.5 * (1 - alphaX) + alphaX * pnorm(muX/sqrt(2)) 
  AUCY <- 0.5 * (1 - alphaY) + alphaY * pnorm(muY/sqrt(2))
  
  rhoAbn1 <- rhoNor 
  rhoAbn12 <- mean(c(rhoAbn1, rhoAbn2)) 
  sigmaNor <- rbind(c(1, rhoNor), c(rhoNor, 1)) 
  sigmaAbn1 <- rbind(c(1, rhoAbn1), c(rhoAbn1, 1)) 
  sigmaAbn2 <- rbind(c(1, rhoAbn2), c(rhoAbn2, 1)) 
  sigmaAbn12 <- rbind(c(1, rhoAbn12), c(rhoAbn12, 1))
  
  p200 <- (1 - alphaX) * (1 - alphaY) 
  p2X0 <- alphaX * (1 - alphaY) 
  p20Y <- (1 - alphaX) * alphaY 
  p2XY <- alphaX * alphaY 
  K2Sample <- sample(c("00", "X0","0Y", "XY"), size = K2, replace = TRUE, 
                     prob = c(p200, p2X0, p20Y, p2XY))
  K200 <- sum(K2Sample == "00") 
  K2X0 <- sum(K2Sample == "X0") 
  K20Y <- sum(K2Sample == "0Y") 
  K2XY <- sum(K2Sample == "XY")
  
  zk1 <- t(rmvnorm(K1, sigma = sigmaNor))
  
  zk200 <- t(rmvnorm(K200, mean = c(0, 0), sigma = sigmaAbn1)) 
  zk2X0 <- t(rmvnorm(K2X0, mean = c(muX, 0), sigma = sigmaAbn12)) 
  zk20Y <- t(rmvnorm(K20Y, mean = c(0, muY), sigma = sigmaAbn12)) 
  zk2XY <- t(rmvnorm(K2XY, mean = c(muX, muY), sigma = sigmaAbn2)) 
  zk2 <- cbind(zk200,zk2X0, zk20Y, zk2XY)
  
  simuData <- Df2RJafrocDataset(zk1, zk2) 
  simuDataB <- DfBinDataset(simuData,desiredNumBins = desiredNumBins, opChType = "ROC")
  
  return(simuDataB) 
}
