#' Number of cases, for specified number of readers, to achieve desired power
#' 
#' @description  Number of cases to achieve the desired power, for 
#'   specified number of readers J, and specified DBMH or ORH analysis method
#' 
#' @param dataset The \bold{pilot} The \strong{pilot} ROC dataset.
#' @param FOM The figure of merit.
#' @param J The number of readers in the \strong{pivotal} study.
#' @param effectSize The effect size to be used in the \strong{pivotal} study.
#' @param method "DBMH" (default) or "ORH".
#' @param alpha The significance level of the study, default is 0.05.
#' @param desiredPower The desired statistical power, default is 0.8.
#' @param option Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" (the default).
#' 
#' @return A list of two elements:
#' @return \item{K}{The minimum number of cases K in the pivotal study 
#'    to just achieve the desired statistical power.}
#' @return \item{power}{The predicted statistical power.}
#' 
#' @details The default \code{effectSize} uses the observed effect size in the pilot 
#'    study. A numeric value over-rides the default value.
#' 
#' 
#' 
#' @references 
#' Hillis SL, Obuchowski NA, Berbaum KS (2011) Power Estimation for Multireader ROC Methods: 
#' An Updated and Unified Approach, Acad Radiol, 18, 129--142.
#' 
#' Hillis SL, Obuchowski NA, Schartz KM, Berbaum KS (2005) A comparison of the Dorfman-Berbaum-Metz 
#' and Obuchowski-Rockette methods for receiver operating characteristic (ROC) data, 
#' Statistics in Medicine, 24:10, 1579--607.
#' 
#' @examples
#' ## An example using the DBM method
#' SsSampleSizeKGivenJ(dataset02, J = 6, method = "DBMH")
#' 
#' ## An example using the OR method
#' SsSampleSizeKGivenJ(dataset02, J = 6, method = "ORH")
#' 
#'
#' \donttest{ 
## Example of power calculations using the DBM variance components, 
## and scanning the number of readers
#' for (J in 6:10) {
#'  ret <- SsSampleSizeKGivenJ(dataset02, J = J, option = "RRRC") 
#'  message("# of readers = ", J, " estimated # of cases = ", ret$K, ", predicted power = ",
#'     signif(ret$powerRRRC,3), "\n")
#' }
#' }
#' 
#' @export

SsSampleSizeKGivenJ <- function(dataset, J, FOM = "Wilcoxon", effectSize = NULL, 
                                method = "DBMH", alpha = 0.05, desiredPower = 0.8, option = "ALL") {
  
  if (!(option %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect option.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  
  if (method == "DBMH") {
    ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "DBMH")
    if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
    varYTR <- ret$varComp$varTR
    varYTC <- ret$varComp$varTC
    varYEps <- ret$varComp$varErr
    ret <- searchNumCasesDBM (J, varYTR, varYTC, varYEps, effectSize, alpha, desiredPower, option)
  } else if (method == "ORH") {
    ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "ORH")
    if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
    varTR <- ret$varComp$varTR
    cov1 <- ret$varComp$cov1
    cov2 <- ret$varComp$cov2
    cov3 <- ret$varComp$cov3
    varEps <- ret$varComp$var
    effectSize <- ret$ciDiffTrtRRRC$Estimate
    KStar <- length(dataset$NL[1,1,,1])
    ret <- searchNumCasesOR (J, varTR, cov1, cov2, cov3, varEps, effectSize, alpha, KStar, desiredPower, option)
  } else stop("method must be DBMH or ORH")
  
  return(ret)
} 



searchNumCasesDBM <- function(J, varYTR, varYTC, varYEps, effectSize, alpha, desiredPower, option)
{
  
  if (option == "RRRC" || option == "ALL"){
    K <- 1
    power <- 0
    while (power <= desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKDbmVarComp (J, K, effectSize, varYTR, varYTC, varYEps, alpha, option)
      power <- ret$powerRRRC
    }
    powerRRRC <- power
    KRRRC <- K
  }
  
  if (option == "FRRC" || option == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKDbmVarComp (J, K, effectSize, varYTR, varYTC, varYEps, alpha, option)
      power <- ret$powerFRRC
    }
    powerFRRC <- power
    KFRRC <- K
  } 
  
  if (option == "RRFC" || option == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKDbmVarComp (J, K, effectSize, varYTR, varYTC, varYEps, alpha, option)
      power <- ret$powerRRFC
    }
    powerRRFC <- power
    KRRFC <- K
  } 
  
  if (option == "ALL"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC,
                      KFRRC = KFRRC, 
                      powerFRRC = powerFRRC, 
                      KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }else if (option == "RRRC"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC
    ))
  }else if (option == "FRRC"){
    return(data.frame(KFRRC = KFRRC, 
                      powerFRRC = powerFRRC))
  }else if (option == "RRFC"){
    return(data.frame(KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }
  
}



searchNumCasesOR <- function(J, varTR, cov1, cov2, cov3, varEps, effectSize, alpha, KStar, desiredPower, option)
{
  
  K <- 1
  power <- 0
  if (option == "RRRC" || option == "ALL"){
    K <- 1
    power <- 0
    while (power <= desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKOrVarComp (J, K, KStar, effectSize, varTR, cov1, cov2, cov3, varEps, alpha, option)
      power <- ret$powerRRRC
    }
    powerRRRC <- power
    KRRRC <- K
  }
  if (option == "FRRC" || option == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKOrVarComp (J, K, KStar, effectSize, varTR, cov1, cov2, cov3, varEps, alpha, option)
      power <- ret$powerFRRC
    }
    powerFRRC <- power
    KFRRC <- K
  } 
  if (option == "RRFC" || option == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKOrVarComp (J, K, KStar, effectSize, varTR, cov1, cov2, cov3, varEps, alpha, option)
      power <- ret$powerRRFC
    }
    powerRRFC <- power
    KRRFC <- K
  } 
  
  if (option == "ALL"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC,
                      KFRRC = KFRRC, 
                      powerFRRC = powerFRRC, 
                      KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }else if (option == "RRRC"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC
    ))
  }else if (option == "FRRC"){
    return(data.frame(KFRRC = KFRRC, 
                      powerFRRC = powerFRRC))
  }else if (option == "RRFC"){
    return(data.frame(KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }
  
}

