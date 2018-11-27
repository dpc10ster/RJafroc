#' Generate a power table
#' 
#' @description  Generate combinations of numbers of readers J and numbers of cases K
#'    for desired power and specified generalization(s)
#' 
#' @param dataset The \bold{pilot} ROC dataset to be used to extrapolate to the \bold{pivotal} study.
#' @param effectSize The effect size to be used in the \bold{pivotal} study, default value is \code{NULL}.
#' @param alpha The The size of the test, default is 0.05.
#' @param desiredPower The desired statistical power, default is 0.8.
#' @param method Analysis method, "DBMH" or "ORH", the default is "DBMH".
#' @param option Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" (the default).
#' 
#' 
#' @return A list containing up to 3 (depending on \code{options}) dataframes. 
#'     Each dataframe contains 3 arrays:
#' @return \item{numReaders}{The numbers of readers in the pivotal study.}  
#' @return \item{numCases}{The numbers of cases in the pivotal study.}
#' @return \item{power}{The estimated statistical powers.}
#' 
#' @details The default \code{effectSize}
#'     uses the observed effect size in the pilot study. A numeric value over-rides the default value.
#' 
#' 
#'@note The procedure is valid for ROC studies only; for FROC studies see Online Appendix Chapter 19.
#'
#'
#' @examples
#' ## Example of sample size calculation with DBM method
#' SsPowerTable(dataset02, method = "DBMH")
#' 
#' ## Example of sample size calculation with OR method
#' SsPowerTable(dataset02, method = "ORH")
#' 
#' @export

SsPowerTable <- function(dataset, effectSize = NULL, alpha = 0.05, desiredPower = 0.8, 
                         method = "DBMH", option = "ALL") {
  
  if (!(option %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect option.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  if (dataset$dataType != "ROC") stop("Dataset must be of type ROC")
  
  if (method == "DBMH") {
    ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "DBMH")
    if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
    varCompDBM <- ret$varComp
    varYTR <- varCompDBM$varComp[3]
    varYTC <- varCompDBM$varComp[4]
    varYEps <- varCompDBM$varComp[6]
    allParameters <- list(method = method, 
                          varYTR = varYTR, 
                          varYTC = varYTC, 
                          varYEps = varYEps, 
                          effectSize = effectSize)
  } else if (method == "ORH") {
    ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "ORH")
    if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
    varTR <- ret$varComp$varCov[2]
    cov1 <- ret$varComp$varCov[3]
    cov2 <- ret$varComp$varCov[4]
    cov3 <- ret$varComp$varCov[5]
    varEps <- ret$varComp$varCov[6]
    KStar <- length(dataset$NL[1,1,,1])
    allParameters <- list(method = method,
                          varTR = varTR,
                          cov1 = cov1, 
                          cov2 = cov2, 
                          cov3 = cov3,
                          varEps = varEps,
                          effectSize = effectSize,
                          KStar = KStar)
  } else stop("1:method must be DBMH or ORH")
  
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
  } else {
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
                                     numCases = randomSampleSize[, 2], 
                                     power = randomSampleSize[, 3], 
                                     stringsAsFactors = FALSE)
      powerTable <- c(powerTable, list(randomSampleSize))
    }
    names(powerTable) <- c("powerTableRRRC", "powerTableFRRC", "powerTableRRFC")
    return(powerTable)
  }
} 
