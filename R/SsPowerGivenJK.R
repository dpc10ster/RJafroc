#' @title Statistical power for specified numbers of readers and cases
#' 
#' @description Calculate the statistical power for specified numbers of readers J, 
#'    cases K, analysis method and DBM or OR variances components
#' 
#'   
#'   
#' @param dataset The \bold{pilot} dataset. If set to NULL 
#'    then variance components must be supplied.
#' @param ... Optional variance components, varYTR, varYTC and varYEps. These are
#'    needed if dataset is not supplied.
#' @param FOM The figure of merit
#' @param J The number of readers in the pivotal study.
#' @param K The number of cases in the pivotal study.
#' @param effectSize The effect size to be used in the \strong{pivotal} study.
#'    Default is NULL, which uses the observed effect size in the pilot dataset. 
#'    Must be supplied if dataset is set to NULL and variance 
#'    components are supplied.
#' @param method "DBMH" (the default) or "ORH".
#' @param analysisOption Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" (the default).
#' @param alpha The significance level, default is 0.05.
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" or "ALROC";
#'     where to evaluate a partial curve based figure of merit. The default is 0.2.
#' 
#' @return The expected statistical power.
#' 
#' @details The default \code{effectSize} uses the observed effect size in the pilot 
#'    study. A numeric value over-rides the default value. This argument must be supplied 
#'    if dataset = NULL and variance compenents (the ... arguments) are supplied.
#'
#' 
#' @examples
#' ## the following two should give identical results
#' SsPowerGivenJK(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 251, method = "DBMH")
#' 
#' a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")$VarCom
#' SsPowerGivenJK(dataset = NULL, J = 6, K = 251, effectSize = 0.05, method = "DBMH", 
#'                     list(
#'                     varYTR = a["VarTR","Estimates"], 
#'                     varYTC = a["VarTC","Estimates"], 
#'                     varYEps = a["VarErr","Estimates"]))
#'                      
#' ## the following two should give identical results
#' SsPowerGivenJK(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, K = 251, method = "ORH")
#' 
#' a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$VarCom
#' KStar <- length(dataset02$ratings$NL[1,1,,1])
#' SsPowerGivenJK(dataset = NULL, effectSize = 0.05, J = 6, K = 251, method = "ORH", 
#'    list(KStar = KStar, 
#'    VarTR = a["VarTR","Estimates"], 
#'    Cov1 = a["Cov1","Estimates"], 
#'    Cov2 = a["Cov2","Estimates"], 
#'    Cov3 = a["Cov3","Estimates"], 
#'    Var = a["Var","Estimates"]))

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
SsPowerGivenJK <- function(dataset, 
                           ..., 
                           FOM, 
                           FPFValue = 0.2, 
                           J, 
                           K, 
                           effectSize = NULL, 
                           method = "DBMH", 
                           analysisOption = "ALL", 
                           alpha = 0.05) {
  
  if (!(analysisOption %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect analysisOption.")
  if (!(method %in% c("DBMH", "ORH"))) stop ("Incorrect method.")
  if (!is.null(dataset) && (dataset$dataType == "LROC") && !(FOM %in% c("Wilcoxon", "PCL", "ALROC"))) stop("Incorrect FOM used with LROC dataset")
  if (!is.null(dataset) && (length(list(...)) > 0)) stop("dataset and variance components cannot both be supplied as arguments")
  
  if (method == "DBMH") {
    if (!(is.null(dataset))) {
      ret <- StSignificanceTesting(dataset, FOM, FPFValue, method = "DBMH")
      if (is.null(effectSize)) effectSize <- ret$RRRC$ciDiffTrt$Estimate
      varYTR <- ret$ANOVA$VarCom["VarTR",1]
      varYTC <- ret$ANOVA$VarCom["VarTC",1]
      varYEps <- ret$ANOVA$VarCom["VarErr",1]
    } else {
      if (is.null(effectSize)) stop("When using variance components as input, effect size needs to be explicitly specified.")
      extraParms <- list(...)[[1]]
      if ("varYTR" %in% names(extraParms)) varYTR <- extraParms$varYTR else stop("missing varYTR")
      if ("varYTC" %in% names(extraParms)) varYTC <- extraParms$varYTC else stop("missing varYTC")
      if ("varYEps" %in% names(extraParms)) varYEps <- extraParms$varYEps else stop("missing varYEps")
    }
    ret <- SsPowerGivenJKDbmVarComp (J, K, effectSize, varYTR, varYTC, varYEps, alpha, analysisOption )
  } else if (method == "ORH") {
    if (!(is.null(dataset))) {
      ret <- StSignificanceTesting(dataset, FOM, FPFValue, method = "ORH")
      # TBA need to check here
      VarTR <- ret$ANOVA$VarCom["VarTR",1]
      Cov1 <- ret$ANOVA$VarCom["Cov1",1]
      Cov2 <- ret$ANOVA$VarCom["Cov2",1]
      Cov3 <- ret$ANOVA$VarCom["Cov3",1]
      Var <- ret$ANOVA$VarCom["Var",1]
      if (is.null(effectSize)) effectSize <- ret$RRRC$ciDiffTrt$Estimate
      KStar <- length(dataset$ratings$NL[1,1,,1])
    } else {
      if (is.null(effectSize)) stop("When using variance components as input, effect size needs to be explicitly specified.")
      extraParms <- list(...)[[1]]
      if ("KStar" %in% names(extraParms)) KStar <- extraParms$KStar else stop("missing KStar")
      if ("VarTR" %in% names(extraParms)) VarTR <- extraParms$VarTR else stop("missing VarTR")
      if ("Cov1" %in% names(extraParms)) Cov1 <- extraParms$Cov1 else stop("missing Cov1")
      if ("Cov2" %in% names(extraParms)) Cov2 <- extraParms$Cov2 else stop("missing Cov2")
      if ("Cov3" %in% names(extraParms)) Cov3 <- extraParms$Cov3 else stop("missing Cov3")
      if ("Var" %in% names(extraParms)) Var <- extraParms$Var else stop("missing Var")
    }
    ret <- SsPowerGivenJKOrVarComp (J, K, KStar, effectSize = effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha, analysisOption)
  } else stop("method must be DBMH or ORH")
  
  return(ret)
} 




#' Power given J, K and Dorfman-Berbaum-Metz variance components
#' @param J The number of readers
#' @param K The number of cases
#' @param effectSize The effect size
#' @param varYTR The treatment-reader DBM variance component
#' @param varYTC The treatment-case DBM variance component
#' @param varYEps The error-term DBM variance component
#' @param alpha The size of the test (default = 0.05)
#' @param analysisOption The desired generalization ("RRRC", "FRRC", "RRFC", "ALL")
#' 
#' @return A list object containing the estimated power and associated statistics
#'    for each desired generalization.
#'   
#' @details The variance components are obtained using \link{StSignificanceTesting}
#'    with \code{method = "DBMH"}.
#' 
#' @examples 
#' VarCom <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH", 
#'    analysisOption = "RRRC")$ANOVA$VarCom
#' varYTR <- VarCom["VarTR",1]
#' varYTC <- VarCom["VarTC",1]
#' varYEps <- VarCom["VarErr",1]
#' ret <- SsPowerGivenJKDbmVarComp (J = 5, K = 100, effectSize = 0.05, varYTR, 
#'    varYTC, varYEps, analysisOption = "RRRC")
#' cat("RRRC power = ", ret$powerRRRC)
#'   
#' @export
#' 
SsPowerGivenJKDbmVarComp <- function(J, K, effectSize, varYTR, varYTC, varYEps, alpha = 0.05, analysisOption){
  
  if (analysisOption == "RRRC" || analysisOption == "ALL") {
    fDen <- (max(0, varYTR) + 1 / K * (varYEps + J * max(varYTC, 0)))
    ddfHRRRC <- fDen^2/((max(0, varYTR) + 1 / K * varYEps)^2/(J - 1))
    deltaRRRC <- ((effectSize)^2 * J/2) / fDen
    fvalueRRRC <- qf(1 - alpha, 1, ddfHRRRC)
    powerRRRC <- pf(fvalueRRRC, 1, ddfHRRRC, ncp = deltaRRRC, FALSE)
  }
  
  if (analysisOption == "RRFC" || analysisOption == "ALL") {
    fDen <- (max(0, varYTR) + 1 / K * (varYEps))
    ddfHRRFC <- J - 1
    deltaRRFC <- ((effectSize)^2 * J/2) / fDen
    fvalueRRFC <- qf(1 - alpha, 1, ddfHRRFC)
    powerRRFC <- pf(fvalueRRFC, 1, ddfHRRFC, ncp = deltaRRFC, FALSE)
  }
  
  if (analysisOption == "FRRC" || analysisOption == "ALL") {
    fDen <- (1 / K * (varYEps + J * max(varYTC, 0)))
    deltaFRRC <- ((effectSize)^2 * J/2) / fDen
    ddfHFRRC <- K - 1
    fvalueFRRC <- qf(1 - alpha, 1, ddfHFRRC)
    powerFRRC <- pf(fvalueFRRC, 1, ddfHFRRC, ncp = deltaFRRC, FALSE)
  }
  
  if (analysisOption == "ALL"){
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
  } else if (analysisOption == "RRRC"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      ddfHRRRC = ddfHRRRC, 
                      fRRRC = fvalueRRRC))
  } else if (analysisOption == "FRRC"){
    return(data.frame(powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      ddfHFRRC = ddfHFRRC, 
                      fFRRC = fvalueFRRC))
  } else if (analysisOption == "RRFC"){
    return(data.frame(powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      ddfHRRFC = ddfHRRFC, 
                      fRRFC = fvalueRRFC))
  }
  
}




#' Power given J, K and Obuchowski-Rockette variance components
#' @param J The number of readers in the \strong{pivotal} study
#' @param K The number of cases in the \strong{pivotal} study
#' @param KStar The number of cases in the \strong{pilot} study
#' @param effectSize The effect size
#' @param VarTR The treatment-reader OR variance component
#' @param Cov1 The OR Cov1 covariance
#' @param Cov2 The OR Cov2 covariance
#' @param Cov3 The OR Cov3 covariance
#' @param Var The OR pure variance term
#' @param alpha The size of the test (default = 0.05)
#' @param analysisOption The desired generalization ("RRRC", "FRRC", "RRFC", "ALL")
#' 
#' @return A list object containing the estimated power and associated statistics
#'    for each desired generalization.
#'   
#' @details The variance components are obtained using \link{StSignificanceTesting} 
#'     with \code{method = "ORH"}.
#' 
#' @examples 
#' dataset <- dataset02 ## the pilot study
#' KStar <- length(dataset$ratings$NL[1,1,,1])
#' VarCom <- StSignificanceTesting(dataset, FOM = "Wilcoxon", 
#' method = "ORH", analysisOption = "RRRC")$ANOVA$VarCom
#' VarTR <- VarCom["VarTR",1]
#' Cov1 <- VarCom["Cov1",1]
#' Cov2 <- VarCom["Cov2",1]
#' Cov3 <- VarCom["Cov3",1]
#' Var <- VarCom["Var",1]
#' ret <- SsPowerGivenJKOrVarComp (J = 5, K = 100, KStar = KStar,  
#'    effectSize = 0.05, VarTR, Cov1, Cov2, Cov3, Var, analysisOption = "RRRC")
#'     
#' cat("RRRC power = ", ret$powerRRRC)
#'
#'   
#' @export
#' 
SsPowerGivenJKOrVarComp <- function(J, K, KStar, effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha = 0.05, analysisOption){
  
  if (analysisOption == "RRRC" || analysisOption == "ALL") {
    fDen <- max(0, VarTR) + KStar / K * (Var - Cov1 + (J - 1) * max(Cov2 - Cov3, 0))
    ddfHRRRC <- fDen^2/((max(0, VarTR) + KStar / K * (Var - Cov1 - max(Cov2 - Cov3, 0)))^2 / (J - 1))
    deltaRRRC <- ((effectSize)^2 * J/2) / fDen
    fvalueRRRC <- qf(1 - alpha, 1, ddfHRRRC)
    powerRRRC <- pf(fvalueRRRC, 1, ddfHRRRC, ncp = deltaRRRC, FALSE)
  }
  
  if (analysisOption == "RRFC" || analysisOption == "ALL") {
    fDen <- (max(0, VarTR) + KStar / K * (Var - Cov1 - max(Cov2 - Cov3, 0)))
    ddfHRRFC <- J - 1
    deltaRRFC <- ((effectSize)^2 * J/2) / fDen
    fvalueRRFC <- qf(1 - alpha, 1, ddfHRRFC)
    powerRRFC <- pf(fvalueRRFC, 1, ddfHRRFC, ncp = deltaRRFC, FALSE)
  }
  
  if (analysisOption == "FRRC" || analysisOption == "ALL") {
    fDen <- (KStar / K * (Var - Cov1 + (J - 1) * max(Cov2 - Cov3, 0)))
    ddfHFRRC <- K - 1
    deltaFRRC <- ((effectSize)^2 * J/2) / fDen
    fvalueFRRC <- qf(1 - alpha, 1, ddfHFRRC)
    powerFRRC <- pf(fvalueFRRC, 1, ddfHFRRC, ncp = deltaFRRC, FALSE)
  }
  
  if (analysisOption == "ALL"){
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
  } else if (analysisOption == "RRRC"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      ddfHRRRC = ddfHRRRC, 
                      fRRRC = fvalueRRRC))
  } else if (analysisOption == "FRRC"){
    return(data.frame(powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      ddfHFRRC = ddfHFRRC, 
                      fFRRC = fvalueFRRC))
  } else if (analysisOption == "RRFC"){
    return(data.frame(powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      ddfHRRFC = ddfHRRFC, 
                      fRRFC = fvalueRRFC))
  }
}  



