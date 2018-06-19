#' @title Statistical power for specified numbers of readers and cases in an ROC study
#' 
#' @description Calculate the statistical power for specified numbers of readers J, 
#'    cases K, analysis method and DBM or OR variances components
#' 
#' @param J The number of readers in the pivotal study
#' @param K The number of cases in the pivotal study
#' @param effectSize The effect size to be used in the calculation, 
#'    the sign is unimportant, see Ch 11 in book for guidance.
#' @param method "DBMH" or "ORH"
#' @param option "RRRC", "FRRC", "RRFC" or "ALL"; the default is "ALL"
#' @param alpha The significance level, default is 0.05.
#' @param ...  Other necessary parameters, OR or DBM variance components, see details
#'
#' 
#' @return The expected statistical power.
#' 
#' @details Regarding other parameters (...) needed are 
#'    \strong{either} 
#'    the set of of DBM variance components, i.e, 
#'    (\code{varYTR}, \code{varYTC}, and \code{varYEps}),
#'    \strong{or}
#'    the set of OR covariance matrix elements, the treatment-reader variance and 
#'    number of cases in pilot study 
#'    i.e, (\code{cov1}, \code{cov2}, \code{cov3}, \code{varEps}, \code{varTR} and 
#'    \code{KStar}).
#'     
#' If both of are given, DBM variance components are used and the OR values are ignored.
#' 
#' Either numeric values, for example, of \code{varYTR}, \code{varYTC}, \code{varYEps}
#'    can be supplied, \strong{provided they are in that order}, 
#'    or the function call must explicitly state, for example, 
#'    \code{cov1 = value1}, \code{cov2 = value2}, \code{cov3 = value3}, 
#'    \code{varTR = value4}, \code{varEps = value5}, 
#'    \code{KStar = value6}, i.e., in any order.
#' 
#' 
#' @examples
#' ## An example of sample size calculation with DBM variance componements
#' retDbm <- StSignificanceTesting(data = dataset02, 
#' FOM = "Wilcoxon", method = "DBMH")
#' effectSize <- retDbm$ciDiffTrtRRRC$Estimate
#' varCompDBM <- retDbm$varComp
#' varYTR <- varCompDBM$varComp[3]
#' varYTC <- varCompDBM$varComp[4]
#' varYEps <- varCompDBM$varComp[6]
#' ## should give close to 80% power for RRRC
#' SsPowerGivenJK(6, 251, effectSize, "DBMH", varYTR = varYTR, varYTC = varYTC, 
#'              varYEps = varYEps)
#'                      
#' ## An example of sample size calculation with OR variance componements.
#' retOR <- StSignificanceTesting(data = dataset02, 
#' FOM = "Wilcoxon", covEstMethod = "Jackknife", method = "ORH")
#' effectSize <- retOR$ciDiffTrtRRRC$Estimate
#' varCompOR <- retOR$varComp
#' varTR <- varCompOR$varCov[2]
#' cov1 <- varCompOR$varCov[3]
#' cov2 <- varCompOR$varCov[4]
#' cov3 <- varCompOR$varCov[5]
#' varEps <- varCompOR$varCov[6]
#' KStar <- length(dataset02$NL[1,1,,1])
#' ## same sample size as above, different method, should again give close to 80% power for RRRC
#' SsPowerGivenJK(6, 251, effectSize, "ORH", cov1 = cov1, cov2 = cov2, cov3 = cov3, 
#'              varEps = varEps, varTR = varTR, KStar = KStar)
#' 
#' @references 
#' Hillis SL, Obuchowski NA, Berbaum KS (2011). Power Estimation for Multireader ROC Methods: 
#' An Updated and Unified Approach. Acad Radiol, 18, 129--142.
#' 
#' Hillis SL, Obuchowski NA, Schartz KM, Berbaum KS (2005). A comparison of the Dorfman-Berbaum-Metz 
#' and Obuchowski-Rockette methods for receiver operating characteristic (ROC) data. 
#' Statistics in Medicine, 24(10), 1579--607.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.crcpress.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#' @export
#' @importFrom stats qf pf
#' 
SsPowerGivenJK <- function(J, K, effectSize, method, option = "ALL", alpha = 0.05, 
  ...) {
  args <- list(...)
  if ((method == "DBMH") && length(args) == 3){
    if (is.null(names(args))){
      varYTR <- args[[1]]
      varYTC <- args[[2]]
      varYEps <- args[[3]]
    }else if (all((names(args) %in% c("varYTR", "varYTC", "varYEps")))){
      varYTR <- args$varYTR
      varYTC <- args$varYTC
      varYEps <- args$varYEps
    }else{
      stop("The number and names of variance components must match the specified method.")
    }
    if (option == "RRRC" || option == "ALL") {
      fDen <- (max(0, varYTR) + 1 / K * (varYEps + J * max(varYTC, 0)))
      #ddfH <- fDen^2/((varYTR + 1 / K * varYEps)^2/(J - 1))
      ddfHRRRC <- fDen^2/((max(0, varYTR) + 1 / K * varYEps)^2/(J - 1))
      deltaRRRC <- ((effectSize)^2 * J/2) / fDen
      fvalueRRRC <- qf(1 - alpha, 1, ddfHRRRC)
      powerRRRC <- pf(fvalueRRRC, 1, ddfHRRRC, ncp = deltaRRRC, FALSE)
    } 
    if (option == "RRFC" || option == "ALL") {
      fDen <- (max(0, varYTR) + 1 / K * (varYEps))
      ddfHRRFC <- J - 1
      deltaRRFC <- ((effectSize)^2 * J/2) / fDen
      fvalueRRFC <- qf(1 - alpha, 1, ddfHRRFC)
      powerRRFC <- pf(fvalueRRFC, 1, ddfHRRFC, ncp = deltaRRFC, FALSE)
    } 
    if (option == "FRRC" || option == "ALL") {
      fDen <- (1 / K * (varYEps + J * max(varYTC, 0)))
      deltaFRRC <- ((effectSize)^2 * J/2) / fDen
      ddfHFRRC <- K - 1
      fvalueFRRC <- qf(1 - alpha, 1, ddfHFRRC)
      powerFRRC <- pf(fvalueFRRC, 1, ddfHFRRC, ncp = deltaFRRC, FALSE)
    }
  }else if ((method == "ORH") && length(args) == 6){
    if (is.null(names(args))){
      cov1 <- args[[1]]
      cov2 <- args[[2]]
      cov3 <- args[[3]]
      varTR <- args[[4]]
      varEps <- args[[5]]
      KStar <- args[[6]]
    }else if (all((names(args) %in% c("cov1", "cov2", "cov3", "varTR", "varEps", "KStar")))){
      cov1 <- args$cov1
      cov2 <- args$cov2
      cov3 <- args$cov3
      varTR <- args$varTR
      varEps <- args$varEps
      KStar <- args$KStar
    }else{
      stop("The number and names of variance components must match the specified method.")
    }
    if (option == "RRRC" || option == "ALL") {
      fDen <- max(0, varTR) + KStar / K * (varEps - cov1 + (J - 1) * max(cov2 - cov3, 0))
      ddfHRRRC <- fDen^2/((max(0, varTR) + KStar / K * (varEps - cov1 - max(cov2 - cov3, 0)))^2 / (J - 1))
      deltaRRRC <- ((effectSize)^2 * J/2) / fDen
      fvalueRRRC <- qf(1 - alpha, 1, ddfHRRRC)
      powerRRRC <- pf(fvalueRRRC, 1, ddfHRRRC, ncp = deltaRRRC, FALSE)
    } 
    if (option == "RRFC" || option == "ALL") {
      fDen <- (max(0, varTR) + KStar / K * (varEps - cov1 - max(cov2 - cov3, 0)))
      ddfHRRFC <- J - 1
      deltaRRFC <- ((effectSize)^2 * J/2) / fDen
      fvalueRRFC <- qf(1 - alpha, 1, ddfHRRFC)
      powerRRFC <- pf(fvalueRRFC, 1, ddfHRRFC, ncp = deltaRRFC, FALSE)
    } 
    if (option == "FRRC" || option == "ALL") {
      fDen <- (KStar / K * (varEps - cov1 + (J - 1) * max(cov2 - cov3, 0)))
      ddfHFRRC <- K - 1
      deltaFRRC <- ((effectSize)^2 * J/2) / fDen
      fvalueFRRC <- qf(1 - alpha, 1, ddfHFRRC)
      powerFRRC <- pf(fvalueFRRC, 1, ddfHFRRC, ncp = deltaFRRC, FALSE)
    }
  }else{
    stop("The number and names of variance components must match the specified method.")
  }
  if (option == "ALL"){
    return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC,
                powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC,
                powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  }else if (option == "RRRC"){
    return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC))
  }else if (option == "FRRC"){
    return(list(powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC))
  }else if (option == "RRFC"){
    return(list(powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  }
  
} 
