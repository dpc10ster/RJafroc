#' Number of cases, for specified number of readers, to achieve desired power
#' 
#' @description  Number of cases to achieve the desired power, for 
#'   specified number of readers J, and specified DBMH or ORH analysis method
#' 
#' @param dataset The \bold{pilot} ROC dataset to be used to extrapolate to the \bold{pivotal} study. If 
#'     missing, then variance components and effectSize must be passed as additional parameters.
#' @param J The number of readers in the pivotal study.
#' @param alpha The significance level of the study, default is 0.05.
#' @param effectSize The effect size to be used in the pivotal study, default value is \code{NULL}.
#' @param desiredPower The desired statistical power, default is 0.8.
#' @param option Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" (the default).
#' @param method "DBMH" (default) or "ORH".
#' @param ... Other parameters, OR or DBM variance components, passed internally, see details
#' 
#'
#' @return A list of two elements:
#' @return \item{K}{The minimum number of cases K in the pivotal study 
#'    to just achieve the desired statistical power.}
#' @return \item{power}{The predicted statistical power.}
#' 
#' @details Other parameters \code{...} are reserved for internal use. The default \code{effectSize}
#'     uses the observed effect size in the pilot study. A numeric value over-rides the default value.
#' 
#' 
#' @note The procedure is valid for ROC studies only; for FROC studies see Online Appendix Chapter 19.
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
#' \dontrun{ 
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

SsSampleSizeKGivenJ <- function(dataset, J, alpha = 0.05, effectSize = NULL, 
                                desiredPower = 0.8, option = "ALL", method = "DBMH", ...) {
  
  if (!(option %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect option.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  
  extraArgs <- list(...)
  if (!missing(dataset)){
    if (method == "DBMH") {
      ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "DBMH")
      if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
      varYTR <- ret$varComp$varComp[3]
      varYTC <- ret$varComp$varComp[4]
      varYEps <- ret$varComp$varComp[6]
      calculatedParameters <- list(varYTR = varYTR, varYTC = varYTC, varYEps = varYEps, effectSize = effectSize)
    } else if (method == "ORH") {
      ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "ORH")
      if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
      varTR <- ret$varComp$varCov[2]
      cov1 <- ret$varComp$varCov[3]
      cov2 <- ret$varComp$varCov[4]
      cov3 <- ret$varComp$varCov[5]
      varEps <- ret$varComp$varCov[6]
      KStar <- length(dataset$NL[1,1,,1])
      calculatedParameters <- list(cov1 = cov1,cov2 = cov2, cov3 = cov3, varEps = varEps, 
                                   varTR = varTR, effectSize = effectSize, KStar = KStar)
    } else stop("1:method must be DBMH or ORH")
  } else {
    calculatedParameters <- c(extraArgs, list(effectSize = effectSize))
  }
  
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
      power <- do.call("SsPowerGivenJK", 
                       c(list(J = J, K = K, option = "RRRC", method = method, alpha = alpha), calculatedParameters))
      power <- power$powerRRRC
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
      power <- do.call("SsPowerGivenJK", 
                       c(list(J = J, K = K, option = "FRRC", method = method, alpha = alpha), calculatedParameters))
      power <- power$powerFRRC
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
      power <- do.call("SsPowerGivenJK", 
                       c(list(J = J, K = K, option = "RRFC", method = method, alpha = alpha), calculatedParameters))
      power <- power$powerRRFC
    }
    powerRRFC <- power
    KRRFC <- K
  } 
  if (option == "ALL"){
    return(list(KRRRC = KRRRC, powerRRRC = powerRRRC,
                KFRRC = KFRRC, powerFRRC = powerFRRC,
                KRRFC = KRRFC, powerRRFC = powerRRFC))
  }else if (option == "RRRC"){
    return(list(KRRRC = KRRRC, powerRRRC = powerRRRC))
  }else if (option == "FRRC"){
    return(list(KFRRC = KFRRC, powerFRRC = powerFRRC))
  }else if (option == "RRFC"){
    return(list(KRRFC = KRRFC, powerRRFC = powerRRFC))
  }
} 
