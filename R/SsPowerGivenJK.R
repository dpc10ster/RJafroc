#' @title Statistical power for specified numbers of readers and cases in an ROC study
#' 
#' @description Calculate the statistical power for specified numbers of readers J, 
#'    cases K, analysis method and DBM or OR variances components
#' 
#'   
#'   
#' @param dataset The \bold{pilot} ROC dataset to be used to extrapolate to the \bold{pivotal} study. If 
#'     missing, then variance components and effectSize must be passed as additional parameters.
#' @param J The number of readers in the pivotal study.
#' @param K The number of cases in the pivotal study.
#' @param method "DBMH" or "ORH".
#' @param option Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" (the default).
#' @param alpha The significance level, default is 0.05.
#' @param ...  Other parameters, OR or DBM variance components, passed internally, see details
#'
#' 
#' @return The expected statistical power.
#' 
#' 
#' @details Other parameters \code{...} are reserved for internal use.
#' 
#' @examples
#' ## An example of sample size calculation with DBM variance componements
#' SsPowerGivenJK(dataset02, 6, 251, method = "DBMH")
#'                      
#' ## An example of sample size calculation with OR variance componements.
#' SsPowerGivenJK(dataset02, 6, 251, method = "ORH")
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
SsPowerGivenJK <- function(dataset, J, K, method = "DBMH", option = "ALL", alpha = 0.05, ...) {
  
  if (!(option %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect option.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  
  args <- list(...)
  if ((!missing(dataset) && (length(args) == 0))){
    if (dataset$dataType != "ROC") stop("Dataset must be of type ROC")
    if (method == "DBMH") {
      ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "DBMH")
      effectSize <- ret$ciDiffTrtRRRC$Estimate
      stop("this needs fixing")
      varYTR <- ret$varComp$varComp[3]
      varYTC <- ret$varComp$varComp[4]
      varYEps <- ret$varComp$varComp[6]
      ret <- HelperDBMH (J, K, option, varYTR, varYTC, varYEps, effectSize, alpha)
    } else if (method == "ORH") {
      ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "ORH")
      varTR <- ret$varComp$varCov[2]
      cov1 <- ret$varComp$varCov[3]
      cov2 <- ret$varComp$varCov[4]
      cov3 <- ret$varComp$varCov[5]
      varEps <- ret$varComp$varCov[6]
      effectSize <- ret$ciDiffTrtRRRC$Estimate
      KStar <- length(dataset$NL[1,1,,1])
      ret <- HelperORH (J, K, option, varTR, cov1, cov2, cov3, varEps, effectSize, KStar, alpha)
    } else stop("method must be DBMH or ORH")
  }
  
  if ((missing(dataset))){
    if ((method == "DBMH") && length(args) == 4){
      if (all((names(args) %in% c("varYTR", "varYTC", "varYEps", "effectSize")))){
        varYTR <- args$varYTR
        varYTC <- args$varYTC
        varYEps <- args$varYEps
        effectSize <- args$effectSize
      } else{
        stop("The order, varYTR, varYTC, varEps, effectSize must match.")
      }
      ret <- HelperDBMH (J, K, option, varYTR, varYTC, varYEps, effectSize, alpha)
    } else if ((method == "ORH") && length(args) == 7){
      if (all((names(args) %in% c("cov1", "cov2", "cov3", "varTR", "varEps", "effectSize", "KStar")))){
        varTR <- args$varTR
        cov1 <- args$cov1
        cov2 <- args$cov2
        cov3 <- args$cov3
        varEps <- args$varEps
        effectSize <- args$effectSize
        KStar <- args$KStar
      } else{
        stop("The number and names of variance components must match the specified method.")
      }
      ret <- HelperORH (J, K, option, varTR, cov1, cov2, cov3, varEps, effectSize, KStar, alpha)
    } else{
      stop("The number and names of variance components must match the specified method.")
    }
  }  
return(ret)
} 


HelperDBMH <- function(J, K, option, varYTR, varYTC, varYEps, effectSize, alpha){
  
  if (option == "RRRC" || option == "ALL") {
    fDen <- (max(0, varYTR) + 1 / K * (varYEps + J * max(varYTC, 0)))
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
  
  if (option == "ALL"){
    return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC,
                powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC,
                powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  } else if (option == "RRRC"){
    return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC))
  } else if (option == "FRRC"){
    return(list(powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC))
  } else if (option == "RRFC"){
    return(list(powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  }
}

HelperORH <- function(J, K, option, varTR, cov1, cov2, cov3, varEps, effectSize, KStar, alpha){
  
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
  
  if (option == "ALL"){
    return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC,
                powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC,
                powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  } else if (option == "RRRC"){
    return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC))
  } else if (option == "FRRC"){
    return(list(powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC))
  } else if (option == "RRFC"){
    return(list(powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  }
}  