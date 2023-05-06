#' Number of cases, for specified number of readers, to achieve desired power
#' 
#' @description  Number of cases to achieve the desired power, for 
#'   specified number of readers J, and specified DBM or ORH analysis method
#' 
#' @param dataset The \bold{pilot} dataset. If set to NULL 
#'    then variance components must be supplied.
#' @param ... Optional variance components, VarTR, VarTC and VarErr. These are
#'    needed if dataset is not supplied.
#' @param FOM The figure of merit. Not needed if variance components are supplied.
#' @param J The number of readers in the \strong{pivotal} study.
#' @param effectSize The effect size to be used in the \strong{pivotal} study.
#'    Default is NULL. Must be supplied if dataset is set to NULL and variance 
#'    components are supplied.
#' @param method "OR" (default) or "DBM".
#' @param alpha The significance level of the study, default is 0.05.
#' @param desiredPower The desired statistical power, default is 0.8.
#' @param analysisOption Desired generalization, "RRRC", "FRRC", "RRFC" or "ALL" 
#'    (the default).
#' @param LegacyCode Logical, default is \code{FALSE}, if \code{TRUE} the DBM
#'    method is used. Otherwise the OR method is used.
#' 
#' @return A list of two elements:
#' @return \item{K}{The minimum number of cases K in the pivotal study 
#'    to just achieve the desired statistical power, calculated 
#'    for each value of \code{analysisOption}.}
#' @return \item{power}{The predicted statistical power.}
#' 
#' @details \code{effectSize} = NULL uses the \strong{observed} effect size in 
#'    the pilot study. A numeric value over-rides the default value. This 
#'    argument must be supplied if dataset = NULL and variance compenents 
#'    (the optional ... arguments) are supplied.
#' 
#'@note The procedure is valid for ROC studies only; for FROC studies see 
#'   Vignettes 19.
#' 
#' @examples
#' ## the following two should give identical results
#' SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "DBM")
#' 
#' a <- UtilVarComponentsDBM(dataset02, FOM = "Wilcoxon")$VarCom
#' SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "DBM", LegacyCode = TRUE,
#'    list(VarTR = a["VarTR",1], 
#'    VarTC = a["VarTC",1], 
#'    VarErr = a["VarErr",1]))
#'
#' ## the following two should give identical results
#' SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", effectSize = 0.05, J = 6, method = "OR")
#' 
#' a <- UtilVarComponentsOR(dataset02, FOM = "Wilcoxon")$VarCom
#' KStar <- length(dataset02$ratings$NL[1,1,,1])
#' SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "OR", 
#'    list(KStar = KStar, 
#'    VarTR = a["VarTR",1], 
#'    Cov1 = a["Cov1",1], 
#'    Cov2 = a["Cov2",1], 
#'    Cov3 = a["Cov3",1], 
#'    Var = a["Var",1]))
#'
#' \donttest{ 
## Example of power calculations using the DBM variance components, 
## and scanning the number of readers
#' for (J in 6:10) {
#'  ret <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = J, analysisOption = "RRRC") 
#'  message("# of readers = ", J, " estimated # of cases = ", ret$K, 
#'  ", predicted power = ", signif(ret$powerRRRC,3), "\n")
#' }
#' }
#' 
#' @export

SsSampleSizeKGivenJ <- function(dataset, ..., J, FOM, effectSize = NULL, 
                                method = "OR", alpha = 0.05, desiredPower = 0.8, 
                                analysisOption = "RRRC", LegacyCode = FALSE) {
  
  if (!(analysisOption %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect analysisOption.")
  if (!(method %in% c("DBM", "OR"))) stop ("Incorrect method.")
  if (!is.null(dataset) && (length(list(...)) > 0)) stop("dataset and variance components cannot both be supplied as arguments")
  if (!is.null(dataset) && (dataset$descriptions$type != "ROC")) stop("Must specify an ROC dataset, not LROC or FROC")
  if (!is.null(dataset) && (length(dataset$ratings$NL[,1,1,1]) != 2)) stop("dataset must have exactly two treatments")
  if (!is.null(dataset) && (dataset$descriptions$design == "FACTRL-X-MOD")) stop("cannot use cross-modality dataset")
  
  if ((method == "DBM") && !LegacyCode) {
    method <- "OR"
  } 
  
  if ((method == "DBM") && LegacyCode) {
    if (!(is.null(dataset))) {
      ret <- StSignificanceTesting(dataset, FOM, method = "DBM")
      if (is.null(effectSize)) effectSize <- as.numeric(ret$FOMs$trtMeanDiffs)
      VarTR <- ret$ANOVA$VarCom["VarTR",1]
      VarTC <- ret$ANOVA$VarCom["VarTC",1]
      VarErr <- ret$ANOVA$VarCom["VarErr",1]
    } else {
      if (is.null(effectSize)) stop("When using variance components as input, effect size needs to be explicitly specified.")
      extraParms <- list(...)[[1]]
      if ("VarTR" %in% names(extraParms)) VarTR <- extraParms$VarTR else stop("missing VarTR")
      if ("VarTC" %in% names(extraParms)) VarTC <- extraParms$VarTC else stop("missing VarTC")
      if ("VarErr" %in% names(extraParms)) VarErr <- extraParms$VarErr else stop("missing VarErr")
    }
    ret <- searchNumCasesDBM (J, VarTR, VarTC, VarErr, effectSize, alpha, desiredPower, analysisOption)
  } else if (method == "OR") {
    if (!(is.null(dataset))) {
      ret <- StSignificanceTesting(dataset, FOM, method = "OR")
      if (is.null(effectSize)) effectSize <- as.numeric(ret$FOMs$trtMeanDiffs)
      VarTR <- ret$ANOVA$VarCom["VarTR",1]
      Cov1 <- ret$ANOVA$VarCom["Cov1",1]
      Cov2 <- ret$ANOVA$VarCom["Cov2",1]
      Cov3 <- ret$ANOVA$VarCom["Cov3",1]
      Var <- ret$ANOVA$VarCom["Var",1]
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
    ret <- searchNumCasesOR (J, VarTR, Cov1, Cov2, Cov3, Var, effectSize, alpha, KStar, desiredPower, analysisOption)
  } else stop("method must be ORH or DBM")
  return(ret)
} 



searchNumCasesDBM <- function(J, VarTR, VarTC, VarErr, effectSize, alpha, desiredPower, analysisOption)
{
  
  if (analysisOption == "RRRC" || analysisOption == "ALL"){
    K <- 1
    power <- 0
    while (power <= desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKDbmVarCom (J, K, effectSize, VarTR, VarTC, VarErr, alpha, analysisOption)
      power <- ret$powerRRRC
    }
    powerRRRC <- power
    KRRRC <- K
  }
  
  if (analysisOption == "FRRC" || analysisOption == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKDbmVarCom (J, K, effectSize, VarTR, VarTC, VarErr, alpha, analysisOption)
      power <- ret$powerFRRC
    }
    powerFRRC <- power
    KFRRC <- K
  } 
  
  if (analysisOption == "RRFC" || analysisOption == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKDbmVarCom (J, K, effectSize, VarTR, VarTC, VarErr, alpha, analysisOption)
      power <- ret$powerRRFC
    }
    powerRRFC <- power
    KRRFC <- K
  } 
  
  if (analysisOption == "ALL"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC,
                      KFRRC = KFRRC, 
                      powerFRRC = powerFRRC, 
                      KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }else if (analysisOption == "RRRC"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC
    ))
  }else if (analysisOption == "FRRC"){
    return(data.frame(KFRRC = KFRRC, 
                      powerFRRC = powerFRRC))
  }else if (analysisOption == "RRFC"){
    return(data.frame(KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }
  
}



searchNumCasesOR <- function(J, VarTR, Cov1, Cov2, Cov3, Var, effectSize, alpha, KStar, desiredPower, analysisOption)
{
  
  K <- 1
  power <- 0
  if (analysisOption == "RRRC" || analysisOption == "ALL"){
    K <- 1
    power <- 0
    while (power <= desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKOrVarCom (J, K, KStar, effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha, analysisOption)
      power <- ret$powerRRRC
    }
    powerRRRC <- power
    KRRRC <- K
  }
  if (analysisOption == "FRRC" || analysisOption == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKOrVarCom (J, K, KStar, effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha, analysisOption)
      power <- ret$powerFRRC
    }
    powerFRRC <- power
    KFRRC <- K
  } 
  if (analysisOption == "RRFC" || analysisOption == "ALL") {
    K <- 1
    power <- 0
    while (power < desiredPower) {
      if (K > 2000) {
        break
      }
      K <- K + 1
      ret <- SsPowerGivenJKOrVarCom (J, K, KStar, effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha, analysisOption)
      power <- ret$powerRRFC
    }
    powerRRFC <- power
    KRRFC <- K
  } 
  
  if (analysisOption == "ALL"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC,
                      KFRRC = KFRRC, 
                      powerFRRC = powerFRRC, 
                      KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }else if (analysisOption == "RRRC"){
    return(data.frame(KRRRC = KRRRC, 
                      powerRRRC = powerRRRC
    ))
  }else if (analysisOption == "FRRC"){
    return(data.frame(KFRRC = KFRRC, 
                      powerFRRC = powerFRRC))
  }else if (analysisOption == "RRFC"){
    return(data.frame(KRRFC = KRRFC, 
                      powerRRFC = powerRRFC))
  }
  
}

