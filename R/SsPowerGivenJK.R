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
#'
#' 
#' @return The expected statistical power.
#' 
#' 
#' 
#' @examples
#' ## An example of sample size calculation with DBM variance componements
#' SsPowerGivenJK(dataset02, 6, 251, method = "DBMH")
#'                      
#' ## An example of sample size calculation with OR variance componements.
#' SsPowerGivenJK(dataset02, 6, 251, method = "ORH")
#' 
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
SsPowerGivenJK <- function(dataset, J, K, method = "DBMH", option = "ALL", alpha = 0.05) {
  
  if (!(option %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect option.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  
  if (dataset$dataType != "ROC") stop("Dataset must be of type ROC")
  if (method == "DBMH") {
    ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "DBMH")
    effectSize <- ret$ciDiffTrtRRRC$Estimate
    varYTR <- ret$varComp$varTR
    varYTC <- ret$varComp$varTC
    varYEps <- ret$varComp$varErr
    ret <- powerStatsGivenJKDBMH (J, K, option, varYTR, varYTC, varYEps, effectSize, alpha)
  } else if (method == "ORH") {
    ret <- StSignificanceTesting(dataset, FOM = "Wilcoxon", method = "ORH")
    varTR <- ret$varComp$varTR
    cov1 <- ret$varComp$cov1
    cov2 <- ret$varComp$cov2
    cov3 <- ret$varComp$cov3
    varEps <- ret$varComp$var
    effectSize <- ret$ciDiffTrtRRRC$Estimate
    KStar <- length(dataset$NL[1,1,,1])
    ret <- powerStatsGivenJKORH (J, K, option, varTR, cov1, cov2, cov3, varEps, effectSize, KStar, alpha)
  } else stop("method must be DBMH or ORH")
  
  return(ret)
} 


powerStatsGivenJKDBMH <- function(J, K, option, varYTR, varYTC, varYEps, effectSize, alpha){
  
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
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      ddfHRRRC = ddfHRRRC, 
                      fRRRC = fvalueRRRC, 
                      powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      ddfHFRRC = ddfHFRRC, 
                      fFRRC = fvalueFRRC, 
                      powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      ddfHRRFC = ddfHRRFC, 
                      fRRFC = fvalueRRFC))
  } else if (option == "RRRC"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      ddfHRRRC = ddfHRRRC, 
                      fRRRC = fvalueRRRC))
  } else if (option == "FRRC"){
    return(data.frame(powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      ddfHFRRC = ddfHFRRC, 
                      fFRRC = fvalueFRRC))
  } else if (option == "RRFC"){
    return(data.frame(powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      ddfHRRFC = ddfHRRFC, 
                      fRRFC = fvalueRRFC))
  }
  
  # if (option == "ALL"){
  #   return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC,
  #               powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC,
  #               powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  # } else if (option == "RRRC"){
  #   return(list(powerRRRC = powerRRRC, ncpRRRC = deltaRRRC, ddfHRRRC = ddfHRRRC, fRRRC = fvalueRRRC))
  # } else if (option == "FRRC"){
  #   return(list(powerFRRC = powerFRRC, ncpFRRC = deltaFRRC, ddfHFRRC = ddfHFRRC, fFRRC = fvalueFRRC))
  # } else if (option == "RRFC"){
  #   return(list(powerRRFC = powerRRFC, ncpRRFC = deltaRRFC, ddfHRRFC = ddfHRRFC, fRRFC = fvalueRRFC))
  # }
}

powerStatsGivenJKORH <- function(J, K, option, varTR, cov1, cov2, cov3, varEps, effectSize, KStar, alpha){
  
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
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      ddfHRRRC = ddfHRRRC, 
                      fRRRC = fvalueRRRC, 
                      powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      ddfHFRRC = ddfHFRRC, 
                      fFRRC = fvalueFRRC, 
                      powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      ddfHRRFC = ddfHRRFC, 
                      fRRFC = fvalueRRFC))
  } else if (option == "RRRC"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      ddfHRRRC = ddfHRRRC, 
                      fRRRC = fvalueRRRC))
  } else if (option == "FRRC"){
    return(data.frame(powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      ddfHFRRC = ddfHFRRC, 
                      fFRRC = fvalueFRRC))
  } else if (option == "RRFC"){
    return(data.frame(powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      ddfHRRFC = ddfHRRFC, 
                      fRRFC = fvalueRRFC))
  }
}  