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
#' \item{\code{perCase}}{ array [1:K2], number of lesions per diseased case}
#' \item{\code{IDs}}{ array [1:K2, 1], labels of lesions on diseased cases}
#' \item{\code{weights}}{ array [1:K2, 1], weights (or clinical importances) of lesions}
#' \item{\code{dataType}}{ "ROC", the data type}
#' \item{\code{modalityID}}{ [1:I] inherited modality labels}
#' \item{\code{readerID}}{ [1:J] inherited reader labels}
#' } 
#'
#' @examples
#' rocDataSet <- DfFroc2Roc(dataset05)
#' 
#' rocSpDataSet <- DfFroc2Roc(datasetFROCSp)
#' 
#' ## in the following example, because of the smaller number of cases, 
#' ## it is easy to see the process at work:
#' set.seed(1);K1 <- 3;K2 <- 5
#' mu <- 1;nuP <- 0.5;lambdaP <- 2;zeta1 <- 0
#' lambda <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$lambda
#' nu <- UtilPhysical2IntrinsicRSM(mu,lambdaP,nuP)$nu
#' Lmax <- 2;Lk2 <- floor(runif(K2, 1, Lmax + 1))
#' frocDataRaw$ratings <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 1, J = 1, 
#' K1, K2, perCase = Lk2)
#' hrData$ratings <- DfFroc2Roc(frocDataRaw$ratings)
#' ## print("frocDataRaw$ratings$NL[1,1,,] = ");## print(frocDataRaw$ratings$NL[1,1,,])
#' ## print("hrData$ratings$NL[1,1,1:K1,] = ");## print(hrData$ratings$NL[1,1,1:K1,])
#' ## print("frocDataRaw$ratings$LL[1,1,,] = ");## print(frocDataRaw$ratings$LL[1,1,,])
#' ## print("hrData$ratings$LL[1,1,,] = ");## print(hrData$ratings$LL[1,1,,]) 
#' ## following is the output
#' ## [1] "frocDataRaw$ratings$NL[1,1,,] = "
#' ## [,1]      [,2]      [,3] [,4]
#' ## [1,] 2.4046534 0.7635935      -Inf -Inf
#' ## [2,]      -Inf      -Inf      -Inf -Inf
#' ## [3,] 0.2522234      -Inf      -Inf -Inf
#' ## [4,] 0.4356833      -Inf      -Inf -Inf
#' ## [5,]      -Inf      -Inf      -Inf -Inf
#' ## [6,]      -Inf      -Inf      -Inf -Inf
#' ## [7,]      -Inf      -Inf      -Inf -Inf
#' ## [8,] 0.8041895 0.3773956 0.1333364 -Inf
#' ## > ## print("hrData$ratings$NL[1,1,1:K1,] = ");## print(hrData$ratings$NL[1,1,1:K1,])
#' ## [1] "hrData$ratings$NL[1,1,1:K1,] = "
#' ## [1] 2.4046534      -Inf 0.2522234
#' ## > ## print("frocDataRaw$ratings$LL[1,1,,] = ");## print(frocDataRaw$ratings$LL[1,1,,])
#' ## [1] "frocDataRaw$ratings$LL[1,1,,] = "
#' ## [,1] [,2]
#' ## [1,]      -Inf -Inf
#' ## [2,] 1.5036080 -Inf
#' ## [3,] 0.8442045 -Inf
#' ## [4,] 1.0467262 -Inf
#' ## [5,]      -Inf -Inf
#' ## > ## print("hrData$ratings$LL[1,1,,] = ");## print(hrData$ratings$LL[1,1,,]) 
#' ## [1] "hrData$ratings$LL[1,1,,] = "
#' ## [1] 0.4356833 1.5036080 0.8442045 1.0467262 0.8041895
#' ## Note that rating of the first and the last diseased case came from NL marks
#' 
#' 
#' @export

DfFroc2Roc <- function (dataset) {
  
  if (dataset$descriptions$type != "FROC") stop("This function requires an FROC dataset to be supplied")
  
  UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
  
  NL <- dataset$ratings$NL
  LL <- dataset$ratings$LL
  
  I <- length(dataset$ratings$NL[,1,1,1])
  J <- length(dataset$ratings$NL[1,,1,1])
  K <- length(dataset$ratings$NL[1,1,,1])
  K2 <- length(dataset$ratings$LL[1,1,,1])
  K1 <- K - K2 
  
  # apply max() over the location index
  NL <- apply(NL, c(1, 2, 3), max)# this gets max NL ratings over location index on all cases
  LL <- apply(LL, c(1, 2, 3), max)# this gets max LL ratings over location index on diseased cases
  
  # add the fourth "unnecessary" dimension
  dim(NL) <- c(dim(NL), 1)
  
  # get max over location index for diseased cases, counting both NLs and LLs
  LLTmp <- array(dim = c(I, J, K2, 2))      # last index is 2, not maxLL, 1 for NLs on diseased cases and 
  # 2 for LLs
  LLTmp[ , , , 1] <- NL[ , , (K1 + 1):K, 1] # this contains the max NL on diseased cases
  LLTmp[ , , , 2] <- LL                     # this contains the max LL on diseased cases
  LL <- apply(LLTmp, c(1, 2, 3), max)       # this contains the max(max LL or max NL on diseased case), 
  # whichever is higher
  # add the fourth "unnecessary" dimension
  dim(LL) <- c(dim(LL), 1)
  
  # remove -Infs ....
  # unmarked FROC images can have -Infs; these belong in the lowest ROC bin;
  # -Inf is not allowed as an ROC rating (will throw off binning alg)
  # find the lowest conf.level that is
  # not -Inf and set OneLtMinRating to one less than this value
  OneLtMinRating <- min(c(NL[NL != UNINITIALIZED],LL[LL != UNINITIALIZED])) - 1 # one less than lowest value
  NL[NL == UNINITIALIZED] <- OneLtMinRating # replace UNINITIALIZED values with OneLtMinRating
  LL[LL == UNINITIALIZED] <- OneLtMinRating # ditto
  
  # tailor the lesions list for an ROC dataset
  perCase <- rep(1, times = K2)
  IDs <- perCase
  dim(IDs) <- c(K2, 1)
  weights <- IDs
  
  # create dataset and return
  modalityID <- as.character(seq(1:I))
  readerID <- as.character(seq(1:J))
  fileName <- paste0("DfFroc2Roc (", dataset$descriptions$fileName, ")")
  name <- dataset$descriptions$name
  design <- dataset$descriptions$design
  
  # convert truthTableStr from FROC to ROC
  truthTableStr <- dataset$descriptions$truthTableStr
  if (!all(is.na(truthTableStr))) { # if FROC truthTableStr is available, convert it to ROC 
    t <- array(dim=c(I,J,K,2)) # default is all NAs
    U <- length(dataset$ratings$LL[1,1,1,]) + 1 # 4th dimension of truthTableStr
    for (k in 1:K) {
      if (k <= K1) {
        t[,,k,1][!is.na(truthTableStr[,,k,1])] <- 1
      }
      else {
        for (el in 2:U) {
          t[,,k,2][!is.na(truthTableStr[,,k,el])] <- 1 # if any is not NA, then set t[,,k,2] to one
        }
      }
    }
  } else t <- NA # FROC truthTableStr is not available
  
  type <- "ROC"
  dataset <- convert2dataset(NL, LL, LL_IL = NA, 
                             perCase, IDs, weights,
                             fileName, type, name, truthTableStr = t, design,
                             modalityID, readerID)
  
  return (dataset)
}

# In the following example, the highest rating comes from LLs on diseased cases for all 
# except case 51, where the highest rating comes from an NL rated 4 as indicated by the
# parenthesis around the rating
# > frocData$ratings$NL[1,1,46:55,]
# [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# [1,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [2,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [3,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [4,]    7    9 -Inf -Inf -Inf -Inf -Inf
# [5,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [6,]  (4) -Inf -Inf -Inf -Inf -Inf -Inf # this is case 51
# [7,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [8,]    4 -Inf -Inf -Inf -Inf -Inf -Inf
# [9,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [10,]    2 -Inf -Inf -Inf -Inf -Inf -Inf

# > frocData$ratings$LL[1,1,1:10,]
# [,1] [,2] [,3]
# [1,]    5 -Inf -Inf
# [2,]   10 -Inf -Inf
# [3,]    7 -Inf -Inf
# [4,]    6    9 -Inf
# [5,] -Inf    9 -Inf
# [6,] -Inf -Inf -Inf
# [7,]   10 -Inf -Inf
# [8,]   10 -Inf -Inf
# [9,] -Inf -Inf -Inf
# [10,]    2 -Inf -Inf

# > rocData$ratings$LL[1,1,1:10,]
# [1]  5 10  7  9  9  (4) 10 10  0  2 # note that highest rating (4) is from NL on dis. case

# Another example
# > frocData$ratings$NL[1,3,46:55,]
# [,1] [,2] [,3] [,4] [,5] [,6] [,7]
# [1,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [2,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [3,]  (7)    7 -Inf -Inf -Inf -Inf -Inf  # this contributes highest rating
# [4,]  (10) -Inf -Inf -Inf -Inf -Inf -Inf # this contributes highest rating
# [5,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [6,]  (9)    8    8 -Inf -Inf -Inf -Inf  # this contributes highest rating
# [7,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf
# [8,]    8 -Inf -Inf -Inf -Inf -Inf -Inf
# [9,]    9    8 -Inf -Inf -Inf -Inf -Inf
# [10,] -Inf -Inf -Inf -Inf -Inf -Inf -Inf

# > frocData$ratings$LL[1,3,1:10,]
# [,1] [,2] [,3]
# [1,]    8 -Inf -Inf
# [2,]    9 -Inf -Inf
# [3,] -Inf -Inf -Inf
# [4,]    9    9 -Inf
# [5,] -Inf    9 -Inf
# [6,] -Inf -Inf -Inf
# [7,]    9 -Inf -Inf
# [8,]   10 -Inf -Inf
# [9,]    9 -Inf -Inf
# [10,] -Inf -Inf -Inf
# > rocData$ratings$LL[1,3,1:10,]

# [1]  8  9  (7) (10)  9  (9)  9 10  9  0

