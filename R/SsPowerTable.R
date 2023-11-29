#' Generate a power table using the OR method
#' 
#' @description  Generate combinations of numbers of readers J and numbers of 
#'    cases K for desired power and specified random factor(s)
#' 
#' @param dataset The \bold{pilot} ROC dataset to be used to extrapolate 
#'    to the \bold{pivotal} study.
#'    
#' @param FOM The figure of merit.
#' 
#' @param effectSize The effect size to be used in the \bold{pivotal} study, 
#'    default value is \code{NULL}. See Details.
#'    
#' @param alpha The The size of the test, default is 0.05.
#' 
#' @param desiredPower The desired statistical power, default is 0.8.
#' 
#' @param analysisOption Specification of random factor(s): "RRRC" (the default), 
#'    "FRRC", or "RRFC".
#' 
#' @return A list containing up to 3 (depending on \code{analysisOption}) 
#'    data frames. 
#' 
#'    Each dataframe contains 3 arrays:
#' @return \item{numReaders}{The numbers of readers in the pivotal study.}  
#' 
#' @return \item{numCases}{The numbers of cases in the pivotal study.}
#' 
#' @return \item{power}{The estimated statistical powers.}
#' 
#' @details The default \code{effectSize} uses the observed effect size in the 
#'    pilot study. A supplied numeric value over-rides the default value.
#' 
#'@note The procedure is valid for ROC studies only; for FROC studies see online books.
#'
#'
#' @examples
#' \donttest{
#' ## Examples with CPU or elapsed time > 5s
#' ##              user    system elapsed
#' ## SsPowerTable 20.033  0.037  20.077    
#'
#' ## Example of sample size calculation with OR method
#' ## SsPowerTable(dataset02, FOM = "Wilcoxon", method = "OR")
#' }
#'  
#' @export

SsPowerTable <- function(dataset, FOM, effectSize = NULL, alpha = 0.05, 
                         desiredPower = 0.8, 
                         analysisOption = "RRRC") {
  
  options(stringsAsFactors = FALSE)
  if (!(analysisOption %in% c("RRRC", "FRRC", "RRFC"))) stop ("Incorrect analysisOption.")
  if (dataset$descriptions$type != "ROC") stop("Dataset must be of type ROC")
  if (length(dataset$ratings$NL[,1,1,1]) != 2) stop("dataset must have exactly two treatments")
  
  KStar <- length(dataset$ratings$NL[1,1,,1])
  
  ret <- St(dataset, FOM, method = "OR")
  if (is.null(effectSize)) effectSize <- as.numeric(ret$FOMs$trtMeanDiffs)
  VarTR <- ret$ANOVA$VarCom["VarTR",1]
  Cov1 <- ret$ANOVA$VarCom["Cov1",1]
  Cov2 <- ret$ANOVA$VarCom["Cov2",1]
  Cov3 <- ret$ANOVA$VarCom["Cov3",1]
  Var <- ret$ANOVA$VarCom["Var",1]
  
  nCases <- 2000
  j <- 2
  randomSampleSize <- NULL
  while (nCases >= 20) {
    j <- j + 1
    if (j > 100) break
    ret <- searchNumCasesOR(J = j, VarTR, Cov1, Cov2, Cov3, Var, effectSize, alpha, KStar, desiredPower, analysisOption)
    
    ret <- nCasesPower (ret, analysisOption)
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
  powerTable <- list(randomSampleSize)
  if (analysisOption == "RRRC") names(powerTable) <- "powerTableRRRC"
  if (analysisOption == "FRRC") names(powerTable) <- "powerTableFRRC"
  if (analysisOption == "RRFC") names(powerTable) <- "powerTableRRFC"
  return(powerTable)
} 



nCasesPower <- function(ret, analysisOption) 
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


