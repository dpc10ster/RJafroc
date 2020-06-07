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
#' @param analysisOption Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" (the default).
#' 
#' 
#' @return A list containing up to 3 (depending on \code{analysisOption}) dataframes. 
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
#' ## SsPowerTable(dataset02, FOM = "Wilcoxon", method = "DBMH")
#' 
#' ## Example of sample size calculation with OR method
#' ## SsPowerTable(dataset02, FOM = "Wilcoxon", method = "ORH")
#' }
#'  
#' @export

SsPowerTable <- function(dataset, FOM, effectSize = NULL, alpha = 0.05, desiredPower = 0.8, 
                         method = "DBMH", analysisOption = "ALL") {
  
  options(stringsAsFactors = FALSE)
  if (!(analysisOption %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect analysisOption.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  if (dataset$descriptions$type != "ROC") stop("Dataset must be of type ROC")
  
  if (method == "DBMH") {
    ret <- StSignificanceTesting(dataset, FOM, method = "DBMH")
    if (is.null(effectSize)) effectSize <- ret$RRRC$ciDiffTrt$Estimate
    varYTR <- ret$ANOVA$VarCom["VarTR",1]
    varYTC <- ret$ANOVA$VarCom["VarTC",1]
    varYEps <- ret$ANOVA$VarCom["VarErr",1]
  } else if (method == "ORH") {
    ret <- StSignificanceTesting(dataset, FOM, method = "ORH")
    if (is.null(effectSize)) effectSize <- ret$RRRC$ciDiffTrt$Estimate
    varTR <- ret$ANOVA$VarCom["VarTR",1]
    cov1 <- ret$ANOVA$VarCom["Cov1",1]
    cov2 <- ret$ANOVA$VarCom["Cov2",1]
    cov3 <- ret$ANOVA$VarCom["Cov3",1]
    varEps <- ret$ANOVA$VarCom["Var",1]
    KStar <- length(dataset$ratings$NL[1,1,,1])
  } else stop("method must be DBMH or ORH")
  
  if (analysisOption != "ALL"){
    nCases <- 2000
    j <- 2
    randomSampleSize <- NULL
    while (nCases >= 20) {
      j <- j + 1
      if (j > 100) break
      if (method == "DBMH") {
        ret <- searchNumCasesDBM (J = j, varYTR, varYTC, varYEps, effectSize, alpha, desiredPower, analysisOption)
      }
      else { 
        ret <- searchNumCasesOR (J = j, varTR, cov1, cov2, cov3, varEps, effectSize, alpha, KStar, desiredPower, analysisOption)
      }
      
      ret <- MyLittleHelper (j, ret, randomSampleSize, analysisOption)
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
                                   power = randomSampleSize[, 3])#, 
                                   # stringsAsFactors = FALSE)
    # 5/4/20 removing all this as I better understand data.frame()
    
    # return(randomSampleSize)
  } else {
    powerTable <- list()
    for (analysisOption in c("RRRC", "FRRC", "RRFC")){
      randomSampleSize <- NULL
      nCases <- 2000
      j <- 2
      while (nCases >= 20) {
        j <- j + 1
        if (j > 100) break
        if (method == "DBMH") {
          ret <- searchNumCasesDBM (J = j, varYTR, varYTC, varYEps, effectSize, alpha, desiredPower, analysisOption)
        }
        else { 
          ret <- searchNumCasesOR (J = j, varTR, cov1, cov2, cov3, varEps, effectSize, alpha, KStar, desiredPower, analysisOption)
        }
        
        ret <- MyLittleHelper (j, ret, randomSampleSize, analysisOption)
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
                                     power = randomSampleSize[, 3])#, 
                                     # stringsAsFactors = FALSE)
      # 5/4/20 removing all this as I better understand data.frame()
      
      powerTable <- c(powerTable, list(randomSampleSize))
    }
    names(powerTable) <- c("powerTableRRRC", "powerTableFRRC", "powerTableRRFC")
    return(powerTable)
  }
} 



MyLittleHelper <- function(j, ret, randomSampleSize, analysisOption) 
{
  if (analysisOption == "RRRC")
  {
    nCases <- ret$KRRRC
    power <- ret$powerRRRC
  } else if (analysisOption == "FRRC") {
    nCases <- ret$KFRRC
    power <- ret$powerFRRC
  } else if (analysisOption == "RRFC") {
    nCases <- ret$KRRFC
    power <- ret$powerRRFC
  } else stop("Incorrect analysisOption flag.")
  
  return(list(
    nCases = nCases,
    power = power
  ))
}


