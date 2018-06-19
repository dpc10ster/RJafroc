#' Generate a power table
#' 
#' @description  Generate combinations of numbers of readers J and numbers of cases K
#'    for desired power and specified generalizations (i.e.,  RRRC or FRRC or RRFC)
#' 
#' @usage SsPowerTable(effectSize, alpha = 0.05, desiredPower = 0.8, 
#'    method = "DBMH", option = "ALL", ...)  
#' 
#' @param effectSize The postulated effect size
#' @param alpha The The size of the test, default is 0.05
#' @param desiredPower The desired statistical power, default is 0.8
#' @param method Analysis method, "DBMH" or "ORH", the default is "DBMH"
#' @param option Desired generalization; the default is "RRRC", for random-reader random-cases 
#' @param ...  Other necessary parameters, OR or DBM variance components, see details
#' 
#' 
#' @return A data frame containing following three columns.
#' @return \item{numReaders}{The number of readers in the pivotal study.}  
#' @return \item{numCases}{The number of cases in the pivotal study.}
#' @return \item{power}{The calculated statistical power corresponding to the indicated
#' numbers of readers and cases.}
#' 
#' @details Regarding other parameters (...), see details in \link{SsPowerGivenJK}
#' 
#' 
#'@note The procedure is valid for ROC studies only; for FROC studies see Online Appendix Chapter 19.
#'
#'
#' @examples
#' ## Example of sample size calculation with DBM method
#' retDbm <- StSignificanceTesting (dataset02, FOM = "Wilcoxon", method = "DBMH")
#' effectSize <- retDbm$ciDiffTrtRRRC$Estimate
#' varYTR <- retDbm$varComp$varComp[3]
#' varYTC <- retDbm$varComp$varComp[4]
#' varYEps <- retDbm$varComp$varComp[6]
#' powTab <- SsPowerTable(
#' effectSize = effectSize, 
#' method = "DBMH", 
#' varYTR = varYTR, 
#' varYTC = varYTC, 
#' varYEps = varYEps)
#' print(powTab)
#' 
#' ## Example of sample size calculation with OR method
#' retOR <- StSignificanceTesting (dataset02, FOM = "Wilcoxon", method = "ORH") 
#' effectSize <- retOR$ciDiffTrtRRRC$Estimate
#' varCompOR <- retOR$varComp
#' varTR <- varCompOR$varCov[2]
#' cov1 <- varCompOR$varCov[3]
#' cov2 <- varCompOR$varCov[4]
#' cov3 <- varCompOR$varCov[5]
#' varEps <- varCompOR$varCov[6]
#' KStar <- length(dataset02$NL[1,1,,1])
#' powTab <- SsPowerTable(
#' effectSize = effectSize, 
#' method = "ORH",
#' KStar = KStar,
#' varTR = varTR, 
#' cov1 = cov1, 
#' cov2 = cov2, 
#' cov3 = cov3, 
#' varEps = varEps) 
#' print(powTab)
#' 
#' @export

SsPowerTable <- function(effectSize, alpha = 0.05, desiredPower = 0.8, 
                         method = "DBMH", option = "ALL", ...) {
  allParameters <- c(as.list(environment()), list(...))
  if (option != "ALL"){
    nCases <- 2000
    j <- 2
    randomSampleSize <- NULL
    while (nCases >= 20) {
      j <- j + 1
      if (j > 100) break
      ret <- do.call("SsSampleSizeKGivenJ", c(allParameters, J = j))
      nCases <- ret[[1]]
      power <- ret[[2]]
      if (nCases > 2000) {
        randomSampleSize <- rbind(randomSampleSize, c(j, ">2000", NA))
      } else if (nCases < 20) {
        randomSampleSize <- rbind(randomSampleSize, c(j, "<20", NA))
      } else {
        randomSampleSize <- rbind(randomSampleSize, c(j, nCases, signif(power, 3)))
      }
    }
    randomSampleSize <- data.frame(numReaders = randomSampleSize[, 1], 
                                   numCases = randomSampleSize[, 2], power = randomSampleSize[, 3])
    return(randomSampleSize)
  }else{
    powerTable <- list()
    for (option in c("RRRC", "FRRC", "RRFC")){
      allParameters$option <- option
      randomSampleSize <- NULL
      nCases <- 2000
      j <- 2
      while (nCases >= 20) {
        j <- j + 1
        if (j > 100) break
        ret <- do.call("SsSampleSizeKGivenJ", c(allParameters, J = j))
        nCases <- ret[[1]]
        power <- ret[[2]]
        if (nCases > 2000) {
          randomSampleSize <- rbind(randomSampleSize, c(j, ">2000", NA))
        } else if (nCases < 20) {
          randomSampleSize <- rbind(randomSampleSize, c(j, "<20", NA))
        } else {
          randomSampleSize <- rbind(randomSampleSize, c(j, nCases, signif(power, 3)))
        }
      }
      randomSampleSize <- data.frame(numReaders = randomSampleSize[, 1], 
                                     numCases = randomSampleSize[, 2], power = randomSampleSize[, 3])
      powerTable <- c(powerTable, list(randomSampleSize))
    }
    names(powerTable) <- c("powerTableRRRC", "powerTableFRRC", "powerTableRRFC")
    return(powerTable)
  }
} 
