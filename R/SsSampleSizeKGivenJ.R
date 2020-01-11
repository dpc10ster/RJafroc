#' Number of cases, for specified number of readers, to achieve desired power
#' 
#' @description  Number of cases to achieve the desired power, for 
#'   specified number of readers J, and specified DBMH or ORH analysis method
#' 
#' @param dataset The \bold{pilot} dataset. If set to NULL 
#'    then variance components must be supplied.
#' @param ... Optional variance components, varYTR, varYTC and varYEps. These are
#'    needed if dataset is not supplied.
#' @param FOM The figure of merit. Not needed if variance components are supplied.
#' @param J The number of readers in the \strong{pivotal} study.
#' @param effectSize The effect size to be used in the \strong{pivotal} study.
#'    Default is NULL. Must be supplied if dataset is set to NULL and variance 
#'    components are supplied.
#' @param method "DBMH" (default) or "ORH".
#' @param alpha The significance level of the study, default is 0.05.
#' @param desiredPower The desired statistical power, default is 0.8.
#' @param option Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" 
#'    (the default).
#' 
#' @return A list of two elements:
#' @return \item{K}{The minimum number of cases K in the pivotal study 
#'    to just achieve the desired statistical power. This is calculated 
#'    for each value of option.}
#' @return \item{power}{The predicted statistical power.}
#' 
#' @details \code{effectSize} = NULL uses the \strong{observed} effect size in 
#'    the pilot study. A numeric value over-rides the default value. This 
#'    argument must be supplied if dataset = NULL and variance compenents 
#'    (the optional ... arguments) are supplied.
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
#' ## the following two should give identical results
#' SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "DBMH")
#' a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")$varComp
#' SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "DBMH", 
#'    list(varYTR = a$varTR, varYTC = a$varTC, varYEps = a$varErr))
#'
#'
#' \donttest{ 
#' ## the following two should give identical results
#' SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "ORH")
#' a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$varComp
#' KStar <- length(dataset02$NL[1,1,,1])
#' SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "ORH", 
#'    list(KStar = KStar, varTR = a$varTR, cov1 = a$cov1, cov2 = a$cov2, 
#'    cov3 = a$cov3, varEps = a$var))
#'    
## Example of power calculations using the DBM variance components, 
## and scanning the number of readers
#' for (J in 6:10) {
#'  ret <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = J, option = "RRRC") 
#'  message("# of readers = ", J, " estimated # of cases = ", ret$K, 
#'  ", predicted power = ", signif(ret$powerRRRC,3), "\n")
#' }
#' }
#' 
#' @export

SsSampleSizeKGivenJ <- function(dataset, ..., J, FOM, effectSize = NULL, 
                                method = "DBMH", alpha = 0.05, desiredPower = 0.8, option = "ALL") {
  
  if (!(option %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect option.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  if (!is.null(dataset) && (length(list(...)) > 0)) stop("dataset and variance components cannot both be supplied as arguments")
  
  if (method == "DBMH") {
    if (!(is.null(dataset))) {
      ret <- StSignificanceTesting(dataset, FOM, method = "DBMH")
      if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
      varYTR <- ret$varComp$varTR
      varYTC <- ret$varComp$varTC
      varYEps <- ret$varComp$varErr
    } else {
      if (is.null(effectSize)) stop("When using variance components as input, effect size needs to be explicitly specified.")
      extraParms <- list(...)[[1]]
      if ("varYTR" %in% names(extraParms)) varYTR <- extraParms$varYTR else stop("missing varYTR")
      if ("varYTC" %in% names(extraParms)) varYTC <- extraParms$varYTC else stop("missing varYTC")
      if ("varYEps" %in% names(extraParms)) varYEps <- extraParms$varYEps else stop("missing varYEps")
    }
    ret <- searchNumCasesDBM (J, varYTR, varYTC, varYEps, effectSize, alpha, desiredPower, option)
  } else if (method == "ORH") {
    if (!(is.null(dataset))) {
      ret <- StSignificanceTesting(dataset, FOM, method = "ORH")
      if (is.null(effectSize)) effectSize <- ret$ciDiffTrtRRRC$Estimate
      varTR <- ret$varComp$varTR
      cov1 <- ret$varComp$cov1
      cov2 <- ret$varComp$cov2
      cov3 <- ret$varComp$cov3
      varEps <- ret$varComp$var
      KStar <- length(dataset$NL[1,1,,1])
    } else {
      if (is.null(effectSize)) stop("When using variance components as input, effect size needs to be explicitly specified.")
      extraParms <- list(...)[[1]]
      if ("KStar" %in% names(extraParms)) KStar <- extraParms$KStar else stop("missing KStar")
      if ("varTR" %in% names(extraParms)) varTR <- extraParms$varTR else stop("missing varTR")
      if ("cov1" %in% names(extraParms)) cov1 <- extraParms$cov1 else stop("missing cov1")
      if ("cov2" %in% names(extraParms)) cov2 <- extraParms$cov2 else stop("missing cov2")
      if ("cov3" %in% names(extraParms)) cov3 <- extraParms$cov3 else stop("missing cov3")
      if ("varEps" %in% names(extraParms)) varEps <- extraParms$varEps else stop("missing varEps")
    }
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

