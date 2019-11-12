#' Convert an FROC dataset to an ROC dataset
#' 
#' @description Convert an FROC dataset to a highest rating inferred ROC dataset
#'  
#' 
#' @param dataset The FROC dataset to be converted, \code{\link{RJafroc-package}}.
#' 
#' @return An ROC dataset with \strong{finite ratings} in NL[,,1:K1,1] and LL[,,1:K2,1]. 
#' 
#' @details The first member of the ROC dataset is \code{NL}, whose 3rd dimension has
#' length \code{(K1 + K2)}, the total number of cases. Ratings of cases \code{(K1 + 1)} 
#' through \code{(K1 + K2)} are \code{-Inf}. \strong{This is because in an ROC dataset 
#' FPs are only possible on non-diseased cases.}The second member of the list is \code{LL}. 
#' Its 3rd dimension has length K2, the number of diseased cases.  \strong{This is 
#' because TPs are only possible on diseased cases}. For each case the  
#' inferred ROC rating is the highest of all FROC ratings on that case. If a case has 
#' no marks, a \strong{finite ROC rating, guaranteed to be smaller than the rating on 
#' any marked case}, is assigned to it. The dataset structure is shown below:
#' \itemize{
#' \item{\code{NL}}{ Ratings array [1:I, 1:J, 1:(K1+K2), 1], of false positives, FPs}
#' \item{\code{LL}}{ Ratings array [1:I, 1:J, 1:K2, 1], of true positives, TPs}
#' \item{\code{lesionVector}}{ array [1:K2], number of lesions per diseased case}
#' \item{\code{lesionID}}{ array [1:K2, 1], labels of lesions on diseased cases}
#' \item{\code{lesionWeight}}{ array [1:K2, 1], weights (or clinical importances) of lesions}
#' \item{\code{dataType}}{ "ROC", the data type}
#' \item{\code{modalityID}}{ [1:I] inherited modality labels}
#' \item{\code{readerID}}{ [1:J] inherited reader labels}
#' } 
#'
#' @examples
#' rocDataSet <- DfFroc2Roc(dataset05)
#' p <- PlotEmpiricalOperatingCharacteristics(rocDataSet, trts = 1, rdrs = 1, opChType = "ROC")
#' print(p$Plot)
#' str(rocDataSet)
#' 
#' ## in the following example, because of the smaller number of cases, 
#' ## it is easy to see the process at work:
#' set.seed(1);K1 <- 3;K2 <- 5
#' mu <- 1;nuP <- 0.5;lambdaP <- 2;zeta1 <- 0
#' lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
#' nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
#' Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
#' frocDataRaw <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, J = 1, 
#' K1, K2, lesionVector = Lk2)
#' hrData <- DfFroc2Roc(frocDataRaw)
#' print("frocDataRaw$NL[1,1,,] = ");print(frocDataRaw$NL[1,1,,])
#' print("hrData$NL[1,1,1:K1,] = ");print(hrData$NL[1,1,1:K1,])
#' print("frocDataRaw$LL[1,1,,] = ");print(frocDataRaw$LL[1,1,,])
#' print("hrData$LL[1,1,,] = ");print(hrData$LL[1,1,,]) 
#' ## following is the output
#' ## [1] "frocDataRaw$NL[1,1,,] = "
#' ## [,1]      [,2]      [,3] [,4]
#' ## [1,] 2.4046534 0.7635935      -Inf -Inf
#' ## [2,]      -Inf      -Inf      -Inf -Inf
#' ## [3,] 0.2522234      -Inf      -Inf -Inf
#' ## [4,] 0.4356833      -Inf      -Inf -Inf
#' ## [5,]      -Inf      -Inf      -Inf -Inf
#' ## [6,]      -Inf      -Inf      -Inf -Inf
#' ## [7,]      -Inf      -Inf      -Inf -Inf
#' ## [8,] 0.8041895 0.3773956 0.1333364 -Inf
#' ## > print("hrData$NL[1,1,1:K1,] = ");print(hrData$NL[1,1,1:K1,])
#' ## [1] "hrData$NL[1,1,1:K1,] = "
#' ## [1] 2.4046534      -Inf 0.2522234
#' ## > print("frocDataRaw$LL[1,1,,] = ");print(frocDataRaw$LL[1,1,,])
#' ## [1] "frocDataRaw$LL[1,1,,] = "
#' ## [,1] [,2]
#' ## [1,]      -Inf -Inf
#' ## [2,] 1.5036080 -Inf
#' ## [3,] 0.8442045 -Inf
#' ## [4,] 1.0467262 -Inf
#' ## [5,]      -Inf -Inf
#' ## > print("hrData$LL[1,1,,] = ");print(hrData$LL[1,1,,]) 
#' ## [1] "hrData$LL[1,1,,] = "
#' ## [1] 0.4356833 1.5036080 0.8442045 1.0467262 0.8041895
#' ## Note that rating of the first and the last diseased case came from NL marks
#' 
#' 
#' @export

DfFroc2Roc <- function (dataset){
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  NL1 <- dataset$NL;NL <- NL1
  LL1 <- dataset$LL;LL <- LL1
  
  I <- length(dataset$NL[,1,1,1])
  J <- length(dataset$NL[1,,1,1])
  K <- length(dataset$NL[1,1,,1])
  K2 <- length(dataset$LL[1,1,,1])
  K1 <- K - K2 
  modalityID <- dataset$modalityID
  readerID <- dataset$readerID
  lesionVector <- dataset$lesionVector
  
  # # unmarked FROC images can have -Infs; these belong in the lowest ROC bin;
  # # -Inf is not allowed as an ROC rating (will throw off binning alg)
  # # find the lowest conf.levels that are
  # # not -Infs and replace them with a lower value (by one) for each modality-reader dataset
  # NL <- dataset$NL[,,1:K1,]
  # LL <- dataset$LL[,,1:K2,]
  # LtMinRating <- min(c(NL[NL != UNINITIALIZED],LL[LL != UNINITIALIZED])) - 1
  # NL[NL == UNINITIALIZED] <- LtMinRating
  # LL[LL == UNINITIALIZED] <- LtMinRating
  # dataset$NL[,,1:K1,] <- NL
  # dataset$LL[,,1:K2,] <- LL
  
  # take maximum over the location index
  NL <- apply(NL, c(1, 2, 3), max)# this gets max NL ratings on all cases
  LL <- apply(LL, c(1, 2, 3), max)# this gets max LL ratings on diseased cases
  
  # add the fourth "unnecessary" dimension
  dim(NL) <- c(dim(NL), 1)
  
  LLTmp <- array(dim = c(I, J, K2, 2))     # last index is 2, not maxLL
  LLTmp[ , , , 1] <- NL[ , , (K1 + 1):K, ] # this contains the max NL on diseased cases
  LLTmp[ , , , 2] <- LL                    # this contains the max LL on diseased cases
  LL <- apply(LLTmp, c(1, 2, 3), max)      # this contains the max LL or max LL, whichever is higher, on diseased cases
  # add the fourth "unnecessary" dimension
  dim(LL) <- c(dim(LL), 1)
  
  lesionVector <- rep(1, times = K2)
  lesionID <- lesionVector
  dim(lesionID) <- c(K2, 1)
  lesionWeight <- lesionID 
  dataset$NL <- NL[,,,1, drop = FALSE]
  dataset$NL[,,(K1+1):K,1] <- UNINITIALIZED
  dataset$LL <- LL
  dataset$lesionVector <- lesionVector
  dataset$lesionID <- lesionID
  dataset$lesionWeight <- lesionWeight
  dataset$dataType <- "ROC"

  # unmarked FROC images may have -Inf; these belong in the lowest bin; 
  # find the lowest conf.level that is
  # not -Inf and replace them with a lower value
  NL <- dataset$NL[,,1:K1,1]
  LL <- dataset$LL[,,1:K2,1]
  LtMinRating <- min(c(NL[NL != UNINITIALIZED],LL[LL != UNINITIALIZED])) - 1 # lower by unity
  dim(NL) <- c(I,J,K1,1);dim(LL) <- c(I,J,K2,1)
  NL[,,1:K1,1][NL[,,1:K1,1] == UNINITIALIZED] <- LtMinRating
  LL[LL == UNINITIALIZED] <- LtMinRating
  dataset$NL[,,1:K1,1] <- NL
  dataset$LL[,,1:K2,1] <- LL
  
  return (dataset)
}

