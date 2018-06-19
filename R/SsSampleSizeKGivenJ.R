#' Number of cases, for specified number of readers, to achieve desired power
#' 
#' @description  Number of cases to achieve the desired power, for 
#'   specified number of readers J, and specified DBM or OR variance components.
#' 
#' @usage SsSampleSizeKGivenJ (J, alpha = 0.05, effectSize = 0.05, 
#'   desiredPower = 0.8, option = "ALL", method = "DBMH", ...) 
#' 
#' 
#' @param J {The number of readers in the pivotal study}
#' @param alpha The significance level of the study, default value is 0.05.
#' @param effectSize The effect size to be used in the study, default value is 0.05.
#' @param desiredPower The desired statistical power, default value is 0.8.
#' @param option Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" (the default).
#' @param method "DBMH" (default) or "ORH".
#' @param ...  Other necessary parameters, OR or DBM variance components, see details
#' 
#'
#' @return A list of two elements:
#' @return \item{K}{The minimum number of cases K in the pivotal study 
#'    to just achieve the desired statistical power.}
#' @return \item{power}{The predicted statistical power.}
#' 
#' @details Regarding other parameters (...), see details in \link{SsPowerGivenJK}.
#'    An additional parameter \code{KStar}, the number of cases in the pilot study, 
#'    \strong{is required when using OR variability parameters}.
#'
#'
#'
#'@note The procedure is valid for ROC studies only; for FROC studies see Online Appendix Chapter 19.
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
#' ## Following is an example of sample size calculation using the DBM variance 
#' ## components and jackknifing (the default) to
#' ## estimate the variance components
#' retDbm <- StSignificanceTesting(data = dataset02, 
#' FOM = "Wilcoxon", method = "DBMH")
#' effectSize <- retDbm$ciDiffTrtRRRC$Estimate
#' varCompDBM <- retDbm$varComp
#' varYTR <- varCompDBM$varComp[3]
#' varYTC <- varCompDBM$varComp[4]
#' varYEps <- varCompDBM$varComp[6]
#' SsSampleSizeKGivenJ(J = 6, varYTR = varYTR, varYTC = varYTC, varYEps = varYEps, 
#'                  effectSize =effectSize)
#'                      
#' ## Following is an example of sample size calculation using the OR variance components
#' retOR <- StSignificanceTesting(data = dataset02, FOM = "Wilcoxon", 
#' covEstMethod = "Jackknife", method = "ORH")
#' effectSize <- retOR$ciDiffTrtRRRC$Estimate
#' varCompOR <- retOR$varComp
#' varTR <- varCompOR$varCov[2]
#' cov1 <- varCompOR$varCov[3]
#' cov2 <- varCompOR$varCov[4]
#' cov3 <- varCompOR$varCov[5]
#' varEps <- varCompOR$varCov[6]
#' KStar <- 114
#' SsSampleSizeKGivenJ(J = 6, cov1 = cov1, cov2 = cov2, cov3 = cov3, varTR = varTR, varEps= varEps, 
#'                 KStar = KStar, effectSize =effectSize, method = "ORH")
#'
#' \dontrun{ 
#' ## Following is an example of power calculations using the DBM variance components, 
#' ## and scanning the number of readers
#' retDbm <- StSignificanceTesting(data = dataset02, 
#' FOM = "Wilcoxon", method = "DBMH")                     
#' effectSize <- retDbm$ciDiffTrtRRRC$Estimate
#' varYTR <- retDbm$varComp$varComp[3]
#' varYTC <- retDbm$varComp$varComp[4]
#' varYEps <- retDbm$varComp$varComp[6]
#' effectSize <- retDbm$ciDiffTrtRRRC$Estimate
#' for (J in 6:10) {
#'  ret <- SsSampleSizeKGivenJ(J = J, varYTR = varYTR, varYTC = varYTC, 
#'  varYEps = varYEps, effectSize =effectSize) 
#'  message("# of readers = ", J, " estimated # of cases = ", ret$K, ", predicted power = ",
#'     signif(ret$powerRRRC,3), "\n")
#' }
#' 
#' ## Following is an example of power calculations using the ORH variance components, 
#' ## using bootstrap to estimate variance components
#' retOR <- StSignificanceTesting(data = dataset02, FOM = "Wilcoxon", 
#' covEstMethod = "Bootstrap", method = "ORH")
#' effectSize <- retOR$ciDiffTrtRRRC$Estimate
#' varCompOR <- retOR$varComp
#' varTR <- varCompOR$varCov[2]
#' cov1 <- varCompOR$varCov[3]
#' cov2 <- varCompOR$varCov[4]
#' cov3 <- varCompOR$varCov[5]
#' varEps <- varCompOR$varCov[6]
#' KStar <- length(dataset02$NL[1,1,,1])
#' SsSampleSizeKGivenJ(J = 6, cov1 = cov1, cov2 = cov2, cov3 = cov3, 
#' varTR = varTR, varEps= varEps, 
#'                  KStar = KStar, effectSize =effectSize, method = "ORH")
#' }
#' 
#' @export

SsSampleSizeKGivenJ <- function(J, alpha = 0.05, effectSize = 0.05, 
                                desiredPower = 0.8, option = "ALL", method = "DBMH", ...) {
  allParameters <- c(as.list(environment()), list(...))
  allParameters <- allParameters[ - which(names(allParameters) == "desiredPower")]
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
      allParameters$option <- "RRRC"
      power <- do.call("SsPowerGivenJK", c(allParameters, list(K = K)))
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
      allParameters$option <- "FRRC"
      power <- do.call("SsPowerGivenJK", c(allParameters, list(K = K)))
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
      allParameters$option <- "RRFC"
      power <- do.call("SsPowerGivenJK", c(allParameters, list(K = K)))
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
