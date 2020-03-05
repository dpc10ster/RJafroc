#' Generate a power table
#' 
#' @description  Generate combinations of numbers of readers J and numbers of cases K
#'    for desired power and specified generalization(s)
#' 
#' @param dataset The \bold{pilot} ROC dataset to be used to extrapolate 
#'    to the \bold{pivotal} study.
#' @param FOM The figure of merit.
#' @param effectSize The effect size to be used in the \bold{pivotal} study, 
#'    default value is \code{NULL}. See Details.
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
#' @details The default \code{effectSize} uses the observed effect size in the 
#'    pilot study. A numeric value over-rides the default value.
#' 
#' 
#'@note The procedure is valid for ROC studies only; for FROC studies see Online Appendix Chapter 19.
#'
#'
#' @examples
#' \donttest{
#' ## Examples with CPU or elapsed time > 5s
#' ##              user    system elapsed
#' ## SsPowerTable 20.033  0.037  20.077    
#'
#' ## Example of sample size calculation with DBM method
#' SsPowerTable(dataset02, FOM = "Wilcoxon", method = "DBMH")
#' 
#' ## Example of sample size calculation with OR method
#' SsPowerTable(dataset02, FOM = "Wilcoxon", method = "ORH")
#' }
#'  
#' @export

SsPowerTable <- function(dataset, FOM, effectSize = NULL, alpha = 0.05, desiredPower = 0.8, 
                         method = "DBMH", option = "ALL") {
  
  if (!(option %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect option.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  if (dataset$dataType != "ROC") stop("Dataset must be of type ROC")
  
  if (method == "DBMH") {
    ret <- StSignificanceTesting(dataset, FOM, method = "DBMH")
    if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
    varYTR <- ret$varComp$varTR
    varYTC <- ret$varComp$varTC
    varYEps <- ret$varComp$varErr
  } else if (method == "ORH") {
    ret <- StSignificanceTesting(dataset, FOM, method = "ORH")
    if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
    varTR <- ret$varComp$varTR
    cov1 <- ret$varComp$cov1
    cov2 <- ret$varComp$cov2
    cov3 <- ret$varComp$cov3
    varEps <- ret$varComp$var
    KStar <- length(dataset$NL[1,1,,1])
  } else stop("method must be DBMH or ORH")
  
  if (option != "ALL"){
    nCases <- 2000
    j <- 2
    randomSampleSize <- NULL
    while (nCases >= 20) {
      j <- j + 1
      if (j > 100) break
      if (method == "DBMH") {
        ret <- searchNumCasesDBM (J = j, varYTR, varYTC, varYEps, effectSize, alpha, desiredPower, option)
      }
      else { 
        ret <- searchNumCasesOR (J = j, varTR, cov1, cov2, cov3, varEps, effectSize, alpha, KStar, desiredPower, option)
      }
      
      ret <- MyLittleHelper (j, ret, randomSampleSize, option)
      nCases <- ret$nCases
      power <- ret$power
      
      if (nCases > 2000) {
        randomSampleSize <- rbind(randomSampleSize, c(j, ">2000", NA))
      } else if (nCases < 20) {
        randomSampleSize <- rbind(randomSampleSize, c(j, "<20", NA))
      } else {
        randomSampleSize <- rbind(randomSampleSize, c(j, nCases, signif(power, 3)))
      }
      
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
                                   power = randomSampleSize[, 3]) 
                                   # stringsAsFactors = FALSE)
    
    # return(randomSampleSize)
  } else {
    powerTable <- list()
    for (option in c("RRRC", "FRRC", "RRFC")){
      randomSampleSize <- NULL
      nCases <- 2000
      j <- 2
      while (nCases >= 20) {
        j <- j + 1
        if (j > 100) break
        if (method == "DBMH") {
          ret <- searchNumCasesDBM (J = j, varYTR, varYTC, varYEps, effectSize, alpha, desiredPower, option)
        }
        else { 
          ret <- searchNumCasesOR (J = j, varTR, cov1, cov2, cov3, varEps, effectSize, alpha, KStar, desiredPower, option)
        }
        
        ret <- MyLittleHelper (j, ret, randomSampleSize, option)
        nCases <- ret$nCases
        power <- ret$power
        
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
                                     power = randomSampleSize[, 3]) 
                                     # stringsAsFactors = FALSE)
      powerTable <- c(powerTable, list(randomSampleSize))
    }
    names(powerTable) <- c("powerTableRRRC", "powerTableFRRC", "powerTableRRFC")
    return(powerTable)
  }
} 



MyLittleHelper <- function(j, ret, randomSampleSize, option) 
{
  if (option == "RRRC")
  {
    nCases <- ret$KRRRC
    power <- ret$powerRRRC
  } else if (option == "FRRC") {
    nCases <- ret$KFRRC
    power <- ret$powerFRRC
  } else if (option == "RRFC") {
    nCases <- ret$KRRFC
    power <- ret$powerRRFC
  } else stop("Incorrect option flag.")
  
  return(list(
    nCases = nCases,
    power = power
  ))
}


