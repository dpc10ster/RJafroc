#' #' Simulate an uncorrelated ROI dataset
#' #' 
#' #' @description  Simulate a raw data uncorrelated single-treatment single-reader ROI dataset 
#' #' 
#' #' @param I  Number of treatments
#' #' @param J  Number of readers
#' #' @param K1 Number of non-diseased cases
#' #' @param K2  Number of diseased cases
#' #' @param Q   Number of ROIs per case
#' #' @param mu     The RSM \eqn{\mu} parameter
#' #' @param lambda The RSM \eqn{\lambda} parameter
#' #' @param nu     The RSM \eqn{\nu} parameter
#' #' @param zeta1  The lowest reporting threshold
#' #' @param lesionVector A K2 length array containing the numbers of lesions per diseased case
#' #' 
#' #' @return The return value is a list with following elements:
#' #' @return \item{dataset}{The ROI dataset.}
#' #' 
#' #' @details See book chapters on the Radiological Search Model (RSM) for details. 
#' #' 
#' #' @examples
#' #' set.seed(1) 
#' 
#' SimulateRoiDataset <- function( I, J, K, Q, mu, tau, varC, varTC, varRC, varEps, varR, varTR, rhoC, rhoRC, rhoTC, rhoEps)
#' {
#' 
#'   stop("Fix me")
#'   muIT <- array(0, dim = c(I, 2))
#'   muIT[,2] <- mu + Deltamu
#' 
#'   qk2 <- round(runif (K[2] , min = 1 , max = Q))
#' 
#'   isDiseased <- array(0 ,dim = c(K[2] , Q))
#'   for (k in 1:K[2]){
#'     isDiseased[k, sample(c(1:Q) , qk2[k] , replace = FALSE)] <- 1
#'   }
#' 
#'   Rjt <- rnorm( 2*J, sd = sqrt(varR) )
#'   dim(Rjt) <- c(J,2)
#' 
#'   Cktt <- rnorm( 2*max(K), sd = sqrt(varC * rhoC) )
#'   dim(Cktt) <- c(max(K),2)
#' 
#'   CLkttlss <- rnorm( 2*max(K)*Q, sd = sqrt(varC * (1 - rhoC)))
#'   dim(CLkttlss) <- c(max(K), 2, Q)
#' 
#'   mRijt <- rnorm( I*2*J, sd = sqrt(varTR) )
#'   dim(mRijt) <- c(I, J, 2)
#' 
#'   mCiktt <- rnorm( I*2*max(K), sd = sqrt(varTC * rhoTC) )
#'   dim(mCiktt) <- c(I, max(K), 2)
#' 
#'   mCLikttlss <- rnorm( I*2*max(K)*Q, sd = sqrt(varTC * (1 - rhoTC)) )
#'   dim(mCLikttlss) <- c(I, max(K), 2, Q)
#' 
#'   RCjktt <- rnorm( J*2*max(K), sd = sqrt(varRC * rhoRC) )
#'   dim(RCjktt) <- c(J, max(K), 2)
#' 
#'   RCLjkttlss <- rnorm( J*2*max(K)*Q, sd = sqrt(varRC * (1 - rhoRC)) )
#'   dim(RCLjkttlss) <- c(J, max(K), 2, Q)
#' 
#'   eijktt <- rnorm( I*J*2*max(K), sd = sqrt(varEps * rhoEps) )
#'   dim(eijktt) <- c(I, J, max(K), 2)
#' 
#'   eLijkttlss <- rnorm( I*J*2*max(K)*Q, sd = sqrt(varEps * (1 - rhoEps)) )
#'   dim(eLijkttlss) <- c(I, J, max(K), 2, Q)
#' 
#'   Rijkttlss <- array(dim=c(I, J,  max(K), 2, Q))
#'   for (i in 1:I) {
#'     for (j in 1:J) {
#'       for (t in 1:2) {
#'         if(t == 1){
#'           for (k in 1:K[t]) {
#'             for (r in 1:Q){
#'               Rijkttlss[i,j,k,t,r] <- (muIT[i, t] + Rjt[j, t] + Cktt[k, t]  + CLkttlss[k, t, r]
#'                                          + mRijt[i, j, t] + mCiktt[i, k, t] + mCLikttlss[i, k, t, r]
#'                                          + RCjktt[j, k, t] + RCLjkttlss[j, k, t, r] + eijktt[i,j,k,t] + eLijkttlss[i, j, k, t, r])
#'             }
#'           }
#'         }else{
#'           for (k in 1:K[t]) {
#'             for (r in 1:Q){
#'               if (isDiseased[k, r] == 0) {
#'                 Rijkttlss[i,j,k,t,r] <- (muIT[i, 1] + Rjt[j, t] + Cktt[k, t]  + CLkttlss[k, t, r]
#'                                            + mRijt[i, j, t] + mCiktt[i, k, t] + mCLikttlss[i, k, t, r]
#'                                            + RCjktt[j, k, t] + RCLjkttlss[j, k, t, r] + eijktt[i,j,k,t] + eLijkttlss[i, j, k, t, r])
#'               } else {
#'                 Rijkttlss[i,j,k,t,r] <- (muIT[i, t] + Rjt[j, t] + Cktt[k, t]  + CLkttlss[k, t, r]
#'                                            + mRijt[i, j, t] + mCiktt[i, k, t] + mCLikttlss[i, k, t, r]
#'                                            + RCjktt[j, k, t] + RCLjkttlss[j, k, t, r] + eijktt[i,j,k,t] + eLijkttlss[i, j, k, t, r])
#'               }
#'             }
#'           }
#'         }
#'       }
#'     }
#'   }
#' 
#'   return( list (Rijkttlss = Rijkttlss,
#'                 isDiseased = isDiseased
#'   ))
#' }
