#' @title Statistical power for specified numbers of readers and cases
#' 
#' @description Calculate the statistical power for specified numbers of readers J, 
#'    cases K, analysis method and DBM or OR variances components
#' 
#' @param dataset The \bold{pilot} dataset. If set to NULL 
#'    then variance components must be supplied.
#'    
#' @param ... Optional variance components: \bold{needed if \code{dataset} 
#'    is not supplied.}
#'    
#' @param FOM The figure of merit.
#' 
#' @param J The number of readers in the pivotal study.
#' 
#' @param K The number of cases in the pivotal study.
#' 
#' @param effectSize The effect size to be used in the \strong{pivotal} study.
#'    Default is NULL, which uses the observed effect size in the pilot dataset. 
#'    Must be supplied if dataset is set to NULL and variance 
#'    components are supplied.
#'    
#' @param method "OR" (the default) or "DBM" (but see \code{UseDBMHB2004} option
#'     below).
#'     
#' @param covEstMethod Specify the variance covariance estimation method(s): 
#'     "jackknife" (the default), "bootstrap" or "DeLong" (for ROC datasets).
#' 
#' @param analysisOption Specify the random factor(s): "RRRC" (the default), 
#'     "RRFC", "FRRC" or "ALL".
#' 
#' @param UseDBMHB2004 Logical, defaults to \code{FALSE}, which results in OR 
#'    sample size method being used, even if DBM method is specified, as in 
#'    Hillis 2011 & 2018 papers. If \code{TRUE} the method based on 
#'    Hillis-Berbaum 2004 sample size paper is used.
#'    
#' @param alpha The significance level, default is 0.05.
#' 
#' @return The expected statistical power in pivotal study for the given conditions and J and K.
#' 
#' @details The default \code{effectSize} uses the observed effect size in the 
#'    pilot study. A numeric value over-rides the default value. This argument 
#'    must be supplied if dataset = NULL and variance compenents 
#'    (the ... arguments) are supplied.
#'
#'@note The procedure is valid for ROC studies only; for FROC studies see 
#'   Vignettes 19.
#' 
#' @examples
#' ## EXAMPLE 1: RRRC power 
#' ## specify 2-modality ROC dataset and force DBM alg.
#' SsPowerGivenJK(dataset = dataset02, FOM = "Wilcoxon", effectSize = 0.05, 
#' J = 6, K = 251, method = "DBM", UseDBMHB2004 = TRUE) # RRRC is default  
#' 
#' ## EXAMPLE 1A: FRRC power 
#' SsPowerGivenJK(dataset = dataset02, FOM = "Wilcoxon", effectSize = 0.05, 
#' J = 6, K = 251, method = "DBM", UseDBMHB2004 = TRUE, analysisOption = "FRRC") 
#' 
#' ## EXAMPLE 1B: RRFC power 
#' SsPowerGivenJK(dataset = dataset02, FOM = "Wilcoxon", effectSize = 0.05, 
#' J = 6, K = 251, method = "DBM", UseDBMHB2004 = TRUE, analysisOption = "RRFC") 
#' 
#' ## EXAMPLE 2: specify NULL dataset & DBM var. comp. & force DBM-based alg.
#' vcDBM <- UtilDBMVarComp(dataset02, FOM = "Wilcoxon")$VarCom
#' SsPowerGivenJK(dataset = NULL, FOM = "Wilcoxon", J = 6, K = 251, 
#' effectSize = 0.05, method = "DBM", UseDBMHB2004 = TRUE, 
#' list( 
#' VarTR = vcDBM["VarTR","Estimates"], # replace rhs with actual values as in 4A
#' VarTC = vcDBM["VarTC","Estimates"], # do:
#' VarErr = vcDBM["VarErr","Estimates"])) # do:
#'                      
#' ## EXAMPLE 3: specify 2-modality ROC dataset and use OR-based alg.
#' SsPowerGivenJK(dataset = dataset02, FOM = "Wilcoxon", effectSize = 0.05, 
#' J = 6, K = 251)
#' 
#' ## EXAMPLE 4: specify NULL dataset & OR var. comp. & use OR-based alg.
#' JStar <- length(dataset02$ratings$NL[1,,1,1])
#' KStar <- length(dataset02$ratings$NL[1,1,,1])
#' vcOR <- UtilOrVarCov(dataset02, FOM = "Wilcoxon")$VarCom
#' SsPowerGivenJK(dataset = NULL, FOM = "Wilcoxon", effectSize = 0.05, J = 6, 
#' K = 251, list(JStar = JStar, KStar = KStar, 
#'    VarTR = vcOR["VarTR","Estimates"], # replace rhs with actual values as in 4A
#'    Cov1 = vcOR["Cov1","Estimates"],   # do:
#'    Cov2 = vcOR["Cov2","Estimates"],   # do:
#'    Cov3 = vcOR["Cov3","Estimates"],   # do:
#'    Var = vcOR["Var","Estimates"]))
#'    
#' ## EXAMPLE 4A: specify NULL dataset & OR var. comp. & use OR-based alg.
#' SsPowerGivenJK(dataset = NULL, FOM = "Wilcoxon", effectSize = 0.05, J = 6, 
#' K = 251, list(JStar = 5, KStar = 114, 
#'    VarTR = 0.00020040252,
#'    Cov1 = 0.00034661371,
#'    Cov2 = 0.00034407483,
#'    Cov3 = 0.00023902837,
#'    Var = 0.00080228827))
#'    
#' ## EXAMPLE 5: specify NULL dataset & DBM var. comp. & use OR-based alg.
#' ## The DBM var. comp. are converted internally to OR var. comp.
#' vcDBM <- UtilDBMVarComp(dataset02, FOM = "Wilcoxon")$VarCom
#' KStar <- length(dataset02$ratings$NL[1,1,,1])
#' SsPowerGivenJK(dataset = NULL, J = 6, K = 251, effectSize = 0.05, 
#' method = "DBM", FOM = "Wilcoxon",
#' list(KStar = KStar,                # replace rhs with actual values as in 5A 
#' VarR = vcDBM["VarR","Estimates"], # do:
#' VarC = vcDBM["VarC","Estimates"], # do:
#' VarTR = vcDBM["VarTR","Estimates"], # do:
#' VarTC = vcDBM["VarTC","Estimates"], # do:
#' VarRC = vcDBM["VarRC","Estimates"], # do:
#' VarErr = vcDBM["VarErr","Estimates"]))
#' 
#' ## EXAMPLE 5A: specify NULL dataset & DBM var. comp. & use OR-based alg.
#' SsPowerGivenJK(dataset = NULL, J = 6, K = 251, effectSize = 0.05, 
#' method = "DBM", FOM = "Wilcoxon",
#' list(KStar = 114,
#' VarR = 0.00153499935,
#' VarC = 0.02724923428,
#' VarTR = 0.00020040252,
#' VarTC = 0.01197529621,
#' VarRC = 0.01226472859,
#' VarErr = 0.03997160319))
#' 
#' 
#' @references 
#' Hillis SL, Berbaum KS (2004). Power Estimation for the Dorfman-Berbaum-Metz Method. 
#'    Acad Radiol, 11, 1260--1273.
#' 
#' Hillis SL, Obuchowski NA, Berbaum KS (2011). Power Estimation for Multireader ROC Methods: 
#' An Updated and Unified Approach. Acad Radiol, 18, 129--142.
#' 
#' Hillis SL, Schartz KM (2018). Multireader sample size program for 
#'    diagnostic studies: demonstration and methodology. Journal of 
#'    Medical Imaging, 5(04).
#'    
#' @export
#' @importFrom stats qf pf
#' 
SsPowerGivenJK <- function(dataset, 
                           ...,
                           FOM, 
                           J, 
                           K, 
                           effectSize = NULL, 
                           method = "OR", 
                           covEstMethod = "jackknife",
                           analysisOption = "RRRC",
                           UseDBMHB2004 = FALSE,
                           alpha = 0.05) {
  
  if (!(analysisOption %in% c("ALL", "RRRC", "FRRC", "RRFC"))) stop ("Incorrect analysisOption.")
  if (!(method %in% c("DBM", "OR"))) stop ("Incorrect method.")
  if (!is.null(dataset) && (dataset$descriptions$type != "ROC")) stop("Must specify an ROC dataset, not LROC or FROC")
  if (!is.null(dataset) && (length(list(...)) > 0)) stop("dataset and variance components cannot both be supplied as arguments")
  if (!is.null(dataset) && (length(dataset$ratings$NL[,1,1,1]) != 2)) stop("dataset must have exactly two treatments")
  if (!is.null(dataset) && (dataset$descriptions$design == "FACTRL-X-MOD")) stop("cannot use cross-modality dataset")
  
  if ((method == "DBM") && (UseDBMHB2004 == TRUE)) {
    # use original sample size formulae based on DBM variance components
    # as in 2004 paper
    if (!(is.null(dataset))) {
      ret <- St(dataset, FOM, method = "DBM")
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
    #VarTR <- max(VarTR,0)
    ret <- SsPowerGivenJKDbmVarCom (J, K, effectSize, VarTR, VarTC, VarErr, alpha, analysisOption )
  } else if ((method == "DBM") && (UseDBMHB2004 == FALSE)) {
    if (!(is.null(dataset))) {
      # dataset is specified
      # convert DBM variance components to OR variance components 
      # and use OR variance component based sample size formulae
      # as in 2011 and 2018 papers
      IStar <- length(dataset$ratings$NL[,1,1,1]); if (IStar != 2) stop("Must specify 2 modality dataset")
      JStar <- length(dataset$ratings$NL[1,,1,1])
      KStar <- length(dataset$ratings$NL[1,1,,1])
      ret <- St(dataset, FOM, method = "DBM")
      if (is.null(effectSize)) effectSize <- as.numeric(ret$FOMs$trtMeanDiffs)
      MST <- ret$ANOVA$TRCanova["T", "MS"]
      MSR <- ret$ANOVA$TRCanova["R", "MS"]
      MSC <- ret$ANOVA$TRCanova["C", "MS"]
      MSTR <- ret$ANOVA$TRCanova["TR", "MS"]
      MSTC <- ret$ANOVA$TRCanova["TC", "MS"]
      MSRC <- ret$ANOVA$TRCanova["RC", "MS"]
      MSTRC <- ret$ANOVA$TRCanova["TRC", "MS"]
      OrVarCom <- DbmMs2OrVarCom(IStar, JStar, KStar, MST, MSR, MSC, MSTR, MSTC, MSRC, MSTRC)
      Cov1 <- OrVarCom$Cov1  
      Cov2 <- OrVarCom$Cov2  
      Cov3 <- OrVarCom$Cov3  
      Var <- OrVarCom$Var
      VarTR <- OrVarCom$VarTR
    } else {
      # dataset NOT specified instead dataset modality, reader and case sizes and DBM var. comps. specified. 
      if (is.null(effectSize)) stop("When using variance components as input, effect size needs to be explicitly specified.")
      extraParms <- list(...)[[1]]
      IStar <- 2
      # following are DBM pseudovalue based quantities
      if ("KStar" %in% names(extraParms)) KStar <- extraParms$KStar else stop("missing KStar")
      if ("VarR" %in% names(extraParms)) VarR <- extraParms$VarR else stop("missing VarR")
      if ("VarC" %in% names(extraParms)) VarC <- extraParms$VarC else stop("missing VarC")
      if ("VarTR" %in% names(extraParms)) VarTR <- extraParms$VarTR else stop("missing VarTR")
      if ("VarTC" %in% names(extraParms)) VarTC <- extraParms$VarTC else stop("missing VarTC")
      if ("VarRC" %in% names(extraParms)) VarRC <- extraParms$VarRC else stop("missing VarRC")
      if ("VarErr" %in% names(extraParms)) VarErr <- extraParms$VarErr else stop("missing VarErr")
      DBMVarComp <- data.frame(VarR = VarR, VarC = VarC, VarTR = VarTR, VarTC = VarTC, VarRC = VarRC, VarErr = VarErr)
      ORVarCom <- UtilDBM2ORVarCom(KStar, t(DBMVarComp))
      # following are OR FOM based quantities
      Cov1 <- ORVarCom["Cov1", 1]  
      Cov2 <- ORVarCom["Cov2", 1]  
      Cov3 <- ORVarCom["Cov3", 1]  
      Var <- ORVarCom["Var", 1]
      VarTR <- ORVarCom["VarTR", 1]
    }
    #VarTR <- max(VarTR,0)
    ret <- SsPowerGivenJKOrVarCom (J, K, KStar, effectSize = effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha, analysisOption)
  } 
  else if (method == "OR") {
    if (!(is.null(dataset))) {
      ret <- St(dataset, FOM, method = "OR", analysisOption = analysisOption, covEstMethod = covEstMethod)
      VarTR <- ret$ANOVA$VarCom["VarTR","Estimates"]
      Cov1 <- ret$ANOVA$VarCom["Cov1","Estimates"]
      Cov2 <- ret$ANOVA$VarCom["Cov2","Estimates"]
      Cov3 <- ret$ANOVA$VarCom["Cov3","Estimates"]
      Var <- ret$ANOVA$VarCom["Var","Estimates"]
      if (is.null(effectSize)) effectSize <- as.numeric(ret$FOMs$trtMeanDiffs)
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
    #VarTR <- max(VarTR,0)
    ret <- SsPowerGivenJKOrVarCom (J, K, KStar, effectSize = effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha, analysisOption)
  } else stop("method must be DBM or ORH")
  
  return(ret)
} 




#' Power given J, K and Dorfman-Berbaum-Metz variance components
#' @param J The number of readers
#' @param K The number of cases
#' @param effectSize The effect size
#' @param VarTR The modality-reader DBM variance component
#' @param VarTC The modality-case DBM variance component
#' @param VarErr The error-term DBM variance component
#' @param alpha The size of the test (default = 0.05)
#' @param analysisOption Specify the random factor(s): "RRRC", "FRRC", "RRFC", "ALL"
#' 
#' @return A list containing the estimated power and associated statistics
#'    for the specified random factor(s).
#'   
#' @details The variance components are obtained using \link{St}
#'    with \code{method = "DBM"}.
#' 
#' @examples 
#' VarCom <- St(dataset02, FOM = "Wilcoxon", method = "DBM", 
#'    analysisOption = "RRRC")$ANOVA$VarCom
#' VarTR <- VarCom["VarTR",1]
#' VarTC <- VarCom["VarTC",1]
#' VarErr <- VarCom["VarErr",1]
#' ret <- SsPowerGivenJKDbmVarCom (J = 5, K = 100, effectSize = 0.05, VarTR, 
#'    VarTC, VarErr, analysisOption = "RRRC")
#' cat("RRRC power = ", ret$powerRRRC)
#'   
#' @export
#' 
SsPowerGivenJKDbmVarCom <- function(J, K, effectSize, VarTR, VarTC, VarErr, alpha = 0.05, analysisOption = "RRRC"){
  
  if (analysisOption == "RRRC" || analysisOption == "ALL") {
    # background on following equations ...
    # ignoring the max() constraints that I put in, my
    # denom = VarTR + (VarErr + J * VarTC) / K
    # denom of 2004 paper, Eqn. 6, after moving (2/J) to the numerator, is
    # denom = VarTR + (VarErr + J * VarTC) / K 
    # QED
    den <- (max(VarTR, 0) + (VarErr + J * max(VarTC, 0)) / K)
    # next eqn is obvious
    deltaRRRC <- ((effectSize)^2 * J/2) / den
    # next expression can be derived from eqn for ddf in book page 234 by using
    # expressions for MS in terms of variances in 2004: eqn 4.
    # See RJafrocBook for details (TBA):
    # special case for I = 2
    # df2 = den^2*(J-1)/(VarTR + VarErr/K)^2 
    df2RRRC <- den^2 * (J - 1) / (max(VarTR, 0) + VarErr / K)^2
    fvalueRRRC <- qf(1 - alpha, 1, df2RRRC)
    powerRRRC <- pf(fvalueRRRC, 1, df2RRRC, ncp = deltaRRRC, FALSE)
  }
  
  if (analysisOption == "FRRC" || analysisOption == "ALL") {
    # set VarTR = 0 in RRRC formulae
    den <- (VarErr + J * max(VarTC, 0)) / K
    deltaFRRC <- ((effectSize)^2 * J/2) / den
    df2FRRC <- K - 1
    fvalueFRRC <- qf(1 - alpha, 1, df2FRRC)
    powerFRRC <- pf(fvalueFRRC, 1, df2FRRC, ncp = deltaFRRC, FALSE)
  }
  
  if (analysisOption == "RRFC" || analysisOption == "ALL") {
    # set VarTC = 0 in RRRC formulae
    den <- max(VarTR, 0) + VarErr/K
    deltaRRFC <- ((effectSize)^2 * J/2) / den
    df2RRFC <- J - 1
    fvalueRRFC <- qf(1 - alpha, 1, df2RRFC)
    powerRRFC <- pf(fvalueRRFC, 1, df2RRFC, ncp = deltaRRFC, FALSE)
  }
  
  if (analysisOption == "ALL"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      df2RRRC = df2RRRC, 
                      fRRRC = fvalueRRRC, 
                      powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      df2FRRC = df2FRRC, 
                      fFRRC = fvalueFRRC, 
                      powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      df2RRFC = df2RRFC, 
                      fRRFC = fvalueRRFC))
  } else if (analysisOption == "RRRC"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      df2RRRC = df2RRRC, 
                      fRRRC = fvalueRRRC))
  } else if (analysisOption == "FRRC"){
    return(data.frame(powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      df2FRRC = df2FRRC, 
                      fFRRC = fvalueFRRC))
  } else if (analysisOption == "RRFC"){
    return(data.frame(powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      df2RRFC = df2RRFC, 
                      fRRFC = fvalueRRFC))
  }
  
}




#' Power given J, K and Obuchowski-Rockette variance components
#' @param J The number of readers in the \strong{pivotal} study
#' @param K The number of cases in the \strong{pivotal} study
#' @param KStar The number of cases in the \strong{pilot} study
#' @param effectSize The effect size
#' @param VarTR The modality-reader OR variance component
#' @param Cov1 The OR Cov1 covariance
#' @param Cov2 The OR Cov2 covariance
#' @param Cov3 The OR Cov3 covariance
#' @param Var The OR pure variance term
#' @param alpha The size of the test (default = 0.05)
#' @param analysisOption Specify the random factor(s): "RRRC", "FRRC", "RRFC", "ALL"
#' 
#' @return A list containing the estimated power and associated statistics
#'    for the specified random factor(s).
#'   
#' @details The variance components are obtained using \link{St} 
#'     with \code{method = "OR"}.
#' 
#' @examples 
#' dataset <- dataset02 ## the pilot study
#' KStar <- length(dataset$ratings$NL[1,1,,1])
#' VarCom <- St(dataset, FOM = "Wilcoxon", 
#' method = "OR", analysisOption = "RRRC")$ANOVA$VarCom
#' VarTR <- VarCom["VarTR",1]
#' Cov1 <- VarCom["Cov1",1]
#' Cov2 <- VarCom["Cov2",1]
#' Cov3 <- VarCom["Cov3",1]
#' Var <- VarCom["Var",1]
#' ret <- SsPowerGivenJKOrVarCom (J = 5, K = 100, KStar = KStar,  
#'    effectSize = 0.05, VarTR, Cov1, Cov2, Cov3, Var, analysisOption = "RRRC")
#'     
#' cat("RRRC power = ", ret$powerRRRC)
#'
#' @importFrom stats qchisq  
#' @export
#' 
SsPowerGivenJKOrVarCom <- function(J, K, KStar, effectSize, VarTR, Cov1, Cov2, Cov3, Var, alpha = 0.05, analysisOption = "RRRC"){

  VarTR <- max(VarTR,0) # TBA this is a big question mark for me
  
  if (analysisOption == "RRRC" || analysisOption == "ALL") {
    # following equations are from Hillis et al Academic Radiology, Vol 18, No 2, February 2011
    den <- VarTR + (KStar / K) * (Var - Cov1 + (J - 1) * max(Cov2 - Cov3, 0)) # Eqn. 10
    df2RRRC <- den^2/((VarTR + (KStar / K) * (Var - Cov1 - max(Cov2 - Cov3, 0)))^2 / (J - 1)) # Eqn. 10
    deltaRRRC <- ((effectSize)^2 * J/2) / den # Eqn. 10
    fvalueRRRC <- qf(1 - alpha, 1, df2RRRC)
    powerRRRC <- pf(fvalueRRRC, 1, df2RRRC, ncp = deltaRRRC, FALSE)
  }
  
  if (analysisOption == "FRRC" || analysisOption == "ALL") {
    # den <- (KStar / K * (Var - Cov1 + (J - 1) * max(Cov2 - Cov3, 0)))
    # deltaFRRC <- ((effectSize)^2 * J/2) / den
    # df2FRRC <- K - 1
    # fvalueFRRC <- qf(1 - alpha, 1, df2FRRC)
    # powerFRRC <- pf(fvalueFRRC, 1, df2FRRC, ncp = deltaFRRC, FALSE)
    den <- ((KStar / K) * (Var - Cov1 + (J - 1) * max(Cov2 - Cov3, 0))) # matches Table 2, 2nd half, left col; 
    deltaFRRC <- ((effectSize)^2 * J/2) / den
    df2FRRC <- NA
    chsqFRRC <- qchisq(1 - alpha, 1)
    powerFRRC <- pchisq(chsqFRRC, 1, ncp = deltaFRRC, FALSE)
  }
  
  if (analysisOption == "RRFC" || analysisOption == "ALL") {
    den <- (VarTR + KStar / K * (Var - Cov1 - max(Cov2 - Cov3, 0)))
    df2RRFC <- J - 1
    deltaRRFC <- ((effectSize)^2 * J/2) / den
    fvalueRRFC <- qf(1 - alpha, 1, df2RRFC)
    powerRRFC <- pf(fvalueRRFC, 1, df2RRFC, ncp = deltaRRFC, FALSE)
  }
  
  if (analysisOption == "ALL"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      df2RRRC = df2RRRC, 
                      fRRRC = fvalueRRRC, 
                      powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      df2FRRC = df2FRRC, 
                      chsqFRRC = chsqFRRC, 
                      powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      df2RRFC = df2RRFC, 
                      fRRFC = fvalueRRFC))
  } else if (analysisOption == "RRRC"){
    return(data.frame(powerRRRC = powerRRRC, 
                      ncpRRRC = deltaRRRC, 
                      df2RRRC = df2RRRC, 
                      fRRRC = fvalueRRRC))
  } else if (analysisOption == "FRRC"){
    return(data.frame(powerFRRC = powerFRRC, 
                      ncpFRRC = deltaFRRC, 
                      df2FRRC = df2FRRC, 
                      chsqFRRC = chsqFRRC))
  } else if (analysisOption == "RRFC"){
    return(data.frame(powerRRFC = powerRRFC, 
                      ncpRRFC = deltaRRFC, 
                      df2RRFC = df2RRFC, 
                      fRRFC = fvalueRRFC))
  }
}  


# From Ref1 = Hillis et al Acad. Rad. 18, 2 2011 (Table 1) and Ref2 = Hillis et al Acad. Rad. 15, 5 2008 (Table 4)
DbmMs2OrVarCom <- function(IStar, JStar, KStar, MST, MSR, MSC, MSTR, MSTC, MSRC, MSTRC) {
  # first OK/NOTOK refers to Ref1 above and second OK/NOTOK to Ref2
  MST  <-  MST/KStar # OK
  MSR  <-  MSR/KStar # Ref1 has typo; OK
  MSTR  <-  MSTR/KStar # OK OK
  Var  <-  (MSC + (IStar-1)*MSTC + (JStar-1)*MSRC + (IStar-1)*(JStar-1)*MSTRC)/IStar/JStar/KStar # OK OK
  # Cov1  <-  (MSC- MSTC + (JStar-1)*MSRC - MSTRC)/IStar/JStar/KStar # OK NOTOK
  # Cov2 <-  (MSC- MSRC + (IStar-1)*MSTC - MSTRC)/IStar/JStar/KStar # OK NOTOK 
  Cov1  <-  (MSC- MSTC + (JStar-1)*(MSRC - MSTRC))/IStar/JStar/KStar # NOTOK OK
  Cov2 <-  (MSC- MSRC + (IStar-1)*(MSTC - MSTRC))/IStar/JStar/KStar # NOTOK OK
  Cov3 <-  (MSC- MSTC - MSRC + MSTRC)/IStar/JStar/KStar # OK OK
  VarTR <- MSTR - Var + Cov1 + max(Cov2 - Cov3, 0) # Hillis 2011 Eqn 9

  return(list(
    MST = MST,
    MSR = MSR,
    MSTR = MSTR,
    VarTR = VarTR,
    Var = Var,
    Cov1 = Cov1,
    Cov2 = Cov2,
    Cov3 = Cov3
  ))
}


