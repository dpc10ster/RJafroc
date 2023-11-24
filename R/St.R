#' Perform DBM or OR significance testing for a one treatment factorial or 
#'     two-treatment crossed modality dataset
#' 
#' @description  Performs DBM or OR significance testing for the dataset. 
#'
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}. 
#'     The dataset design can be "FCTRL" or "FCTRL-X-MOD". 
#'     
#' @param FOM The figure of merit, see \code{\link{UtilFigureOfMerit}}
#' 
#' @param method The significance testing method to be used:  
#'    \code{"DBM"} for the Dorfman-Berbaum-Metz method or \code{"OR"} 
#'    for the Obuchowski-Rockette method (default).  
#'    
#' @param covEstMethod The covariance matrix estimation method in \code{ORH} 
#'    analysis (for \code{method = "DBM"} the jackknife is always used).
#'    \itemize{ 
#'    \item \code{"Jackknife"} (default), 
#'    \item \code{"Bootstrap"}, in which case \code{nBoots} is relevant, default 
#'    200, 
#'    \item \code{"DeLong"}; requires \code{FOM = "Wilcoxon" or "ROI" or "HrAuc"}.
#' }   
#' 
#' @param analysisOption Determines which factors are regarded as random and 
#'     which are fixed:
#' \itemize{ 
#'    \item \code{"RRRC"} = random-reader random case (default),
#'    \item \code{"FRRC"} = fixed-reader random case, 
#'    \item \code{"RRFC"} = random-reader fixed case, 
#'    \item \code{"ALL"} =  all 3 allowed options.
#' }    
#' 
#' @param alpha The significance level (alpha) of the test of the null hypothesis 
#' that all modality effects are zero (default: alpha = 0.05).
#'    
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" 
#'     or "ALROC"; where to evaluate a partial curve based figure of merit. 
#'     (default: FPFValue = 0.2).
#'     
#' @param nBoots The number of bootstraps (defaults to 200), only needed if 
#'    \code{covEstMethod = "bootstrap"} and \code{method = "OR"} 
#'    
#' @param seed For bootstraps the seed of the RNG (default: seed = \code{NULL}), 
#'     only needed if \code{method = "OR"} and \code{covEstMethod = "bootstrap"}.
#'    
#'     
#' @param details Amount of explanations in output, default is 0 for no 
#'     explanations, 1 for some explanations and 2 for most explanations (2 is
#'     recommended for cross modality datasets).
#'    
#'     
#' @return A list containing the results of the analysis.
#' 
#' @examples
#' result <- St(dataset02,FOM = "Wilcoxon", method = "DBM") 
#' result <- St(dataset02,FOM = "Wilcoxon", method = "OR")
#' result <- St(datasetX, FOM = "wAFROC", method = "OR", analysisOption = "ALL")
#' 
#' \donttest{
#' result <- St(dataset05, FOM = "wAFROC")
#' result <- St(dataset05, FOM = "HrAuc", method = "DBM") 
#' } 
#'
#' @note \code{details} = 0 should suffice for factorial dataset analysis since
#'     the names of the output lists are self-explanatory. For cross-modality 
#'     analysis \code{details} = 2 is suggested to better understand the output. 
#' 
#' @references
#' Dorfman DD, Berbaum KS, Metz CE (1992) ROC characteristic rating analysis: 
#' Generalization to the Population of Readers and Patients with the Jackknife 
#' method, Invest. Radiol. 27, 723-731.
#' 
#' Obuchowski NA, Rockette HE (1995) Hypothesis Testing of the Diagnostic 
#' Accuracy for Multiple Diagnostic Tests: An ANOVA Approach with Dependent 
#' Observations, Communications in Statistics: Simulation and Computation 24, 
#' 285-308.
#' 
#' Hillis SL (2014) A marginal-mean ANOVA approach for analyzing multireader 
#' multicase radiological imaging data, Statistics in medicine 33, 330-360.
#' 
#' Thompson JD, Chakraborty DP, Szczepura K, et al. (2016) Effect of reconstruction 
#' methods and x-ray tube current-time product  on nodule detection in an 
#' anthropomorphic thorax phantom: a crossed-modality JAFROC observer study. 
#' Medical Physics. 43(3):1265-1274.
#' 
#' Chakraborty DP (2017) \emph{Observer Performance Methods for Diagnostic Imaging - Foundations, 
#' Modeling, and Applications with R-Based Examples}, CRC Press, Boca Raton, FL. 
#' \url{https://www.routledge.com/Observer-Performance-Methods-for-Diagnostic-Imaging-Foundations-Modeling/Chakraborty/p/book/9781482214840}
#' 
#'
#' @importFrom stats pf pt qt
#' @importFrom Rcpp evalCpp
#' @useDynLib RJafroc
#'
#'      
#' @export
St <- function(dataset, 
               FOM, 
               method = "OR", 
               covEstMethod = "jackknife", 
               analysisOption = "RRRC",
               alpha = 0.05,
               FPFValue = 0.2,
               nBoots = 200, 
               seed = NULL,
               details = 0)
{
  
  isValidDataset(dataset, FOM, method, analysisOption, covEstMethod = covEstMethod)
  
  if (method == "DBM"){
    
    ret <- StDBMAnalysis(dataset, 
                         FOM, 
                         analysisOption,
                         alpha,
                         FPFValue,
                         details)
    
    return(ret)
    
  } else { # method == "OR"
    
    ret <- StORAnalysis(dataset,
                        FOM,
                        covEstMethod, 
                        analysisOption,
                        alpha,
                        FPFValue,
                        nBoots, 
                        seed,
                        details)
    
    return(ret)
    
  }
  
}


MyOutput <- function(fileName) {
  
  fn <- system.file("extdata", fileName, package = "RJafroc", mustWork = TRUE)
  x <- readLines(fn)
  for (i in 1:length(x)) cat(sprintf("%-s\n", x[i]))
  
}


DisclaimerDsSummary <- function (dataset, FOM, method, analysisOption) {
  
  MyOutput("OUTPUT/DISCLAIMER.txt")
  
  cat(paste("R version:", R.version$version.string,"\n"))
  cat(paste("RJafroc version:", packageVersion("RJafroc"),"\n"))
  dateTime <- paste0("Run date: ", base::format(Sys.time(), "%b %d %Y %a %X %Z"))
  cat(paste(dateTime, "\n"))
  
  cat(sprintf("Input Data Set              :  %s\n", dataset$descriptions$name))
  cat(sprintf("Data type                   :  %s\n", dataset$descriptions$type))
  cat(sprintf("Design type                 :  %s\n", dataset$descriptions$design))
  cat(sprintf("FOM selected                :  %s\n", FOM))
  cat(sprintf("Analysis method             :  %s\n", method))
  cat(sprintf("Analysis Option             :  %s\n", analysisOption))
  
  
}


Explanations <- function(dataset, FOM, method, analysisOption, details) {
  
  if (details > 0) {
    DisclaimerDsSummary(dataset, FOM, method, analysisOption)  
  }
  
  if (dataset$descriptions$design == "FCTRL") 
  {
    
    NL <- dataset$ratings$NL
    LL <- dataset$ratings$LL
    lesionID <- dataset$lesions$IDs
    maxNL <- dim(NL)[4]
    dataType <- dataset$descriptions$type
    modalityID <- dataset$descriptions$modalityID
    I <- length(modalityID)
    readerID <- dataset$descriptions$readerID
    J <- length(readerID)
    K <- dim(NL)[3]
    K2 <- dim(LL)[3]
    K1 <- K - K2
    UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
    nLesionPerCase <- rowSums(lesionID != UNINITIALIZED)
    
    if (details > 0) {
      if (method == "OR") {
        cat(sprintf("Significance testing method :  %s\n", toupper("Obuchowski-Rockette-Hillis")))
      } else {
        cat(sprintf("Significance testing method :  %s\n", toupper("Dorfman-Berbaum-Metz-Hillis")))
      } 
      cat(sprintf("Number of Readers           :  %d\n", J))
      cat(sprintf("# Treatments modality       :  %d\n", I))
      cat(sprintf("Number of Normal Cases      :  %d\n", K1))
      cat(sprintf("Number of Abnormal Cases    :  %d\n", K2))
      cat(sprintf("Fraction of Normal Cases    :  %f\n", K1/K))
      
      if (FOM %in% c("MaxNLF", "HrSp")) { 
        K <- K1
        cat(sprintf("choice of FOM implies only non-diseased cases are used in the analysis\n"))
      } else if (FOM %in% c("MaxLLF", "HrSe")) {
        K <- K2
        cat(sprintf("choice of FOM implies only diseased cases are used in the analysis\n"))
      }
      
      if (dataType == "FROC") {
        cat(sprintf("Min number of lesions per diseased case   :  %d\n", 
                    min(nLesionPerCase)))
        cat(sprintf("Max number of lesions per diseased case   :  %d\n", 
                    max(nLesionPerCase)))
        cat(sprintf("Mean number of lesions per diseased case  :  %f\n", 
                    mean(nLesionPerCase)))
        cat(sprintf("Total number of lesions                   :  %d\n", 
                    sum(nLesionPerCase)))
        
        cat(sprintf("Excel file modality IDs     :  %s\n", paste(names(modalityID), collapse = ", ")))
        cat(sprintf("Excel file reader IDs       :  %s\n\n", paste(names(readerID), collapse = ", ")))
        MyOutput("OUTPUT/OVERVIEW.txt")
      }
    }
    
    if (details > 1) {
      MyOutput(paste0("OUTPUT/OR/FCTRL/", analysisOption, "-", method, "-", dataset$descriptions$design, ".txt"))
    }
    
    if (details > 0) {
      MyOutput("OUTPUT/RESULTS.txt")
    }
    
  } else {
    
    NL <- dataset$ratings$NL
    LL <- dataset$ratings$LL
    lesionID <- dataset$lesions$IDs
    maxNL <- dim(NL)[5]
    dataType <- dataset$descriptions$type
    modalityID1 <- dataset$descriptions$modalityID1
    modalityID2 <- dataset$descriptions$modalityID2
    I1 <- length(modalityID1)
    I2 <- length(modalityID2)
    readerID <- dataset$descriptions$readerID
    J <- length(readerID)
    K <- dim(NL)[4]
    K2 <- dim(LL)[4]
    K1 <- K - K2
    UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
    nLesionPerCase <- rowSums(lesionID != UNINITIALIZED)
    
    cat(sprintf("Number of Readers           :  %d\n", J))
    cat(sprintf("# Treatments modality 1     :  %d\n", I1))
    cat(sprintf("# Treatments modality 2     :  %d\n", I2))
    cat(sprintf("Number of Normal Cases      :  %d\n", K1))
    cat(sprintf("Number of Abnormal Cases    :  %d\n", K2))
    cat(sprintf("Fraction of Normal Cases    :  %f\n", K1/K))
    
    if (details > 0) {
      if (method == "OR") {
        cat(sprintf("Significance testing method :  %s\n", toupper("Obuchowski-Rockette-Hillis")))
      } else {
        cat(sprintf("Significance testing method :  %s\n", toupper("Dorfman-Berbaum-Metz-Hillis")))
      } 
      cat(sprintf("Number of Readers           :  %d\n", J))
      cat(sprintf("# treatments 1st modality   :  %d\n", I1))
      cat(sprintf("# treatments 2nd modality   :  %d\n", I2))
      cat(sprintf("Number of Normal Cases      :  %d\n", K1))
      cat(sprintf("Number of Abnormal Cases    :  %d\n", K2))
      cat(sprintf("Fraction of Normal Cases    :  %f\n", K1/K))
      
      if (FOM %in% c("MaxNLF", "HrSp")) { # !!!DPC!!! need to check these FOMs
        K <- K1
        cat(sprintf("choice of FOM implies only non-diseased cases are used in the analysis\n"))
      } else if (FOM %in% c("MaxLLF", "HrSe")) {
        K <- K2
        cat(sprintf("choice of FOM implies only diseased cases are used in the analysis\n"))
      }
      
      UNINITIALIZED <- RJafrocEnv$UNINITIALIZED
      nLesionPerCase <- rowSums(lesionID != UNINITIALIZED)
      
      if (dataType == "FROC") {
        cat(sprintf("Min number of lesions per diseased case   :  %d\n", 
                    min(nLesionPerCase)))
        cat(sprintf("Max number of lesions per diseased case   :  %d\n", 
                    max(nLesionPerCase)))
        cat(sprintf("Mean number of lesions per diseased case  :  %f\n", 
                    mean(nLesionPerCase)))
        cat(sprintf("Total number of lesions                   :  %d\n", 
                    sum(nLesionPerCase)))
        
      }
    }
    
    if (details > 0) {
      cat(sprintf("Excel file modality IDs 1st treatment :  %s\n", paste(names(modalityID1), collapse = ", ")))
      cat(sprintf("Excel file modality IDs 2nd treatment :  %s\n", paste(names(modalityID2), collapse = ", ")))
      cat(sprintf("Excel file reader IDs                 :  %s\n\n", paste(names(readerID), collapse = ", ")))
      MyOutput("OUTPUT/OVERVIEW.txt")
    }
    
    if (details > 1) {
      MyOutput(paste0("OUTPUT/DBM/FCTRLX/", analysisOption, "-", method, "-", dataset$descriptions$design, ".txt"))
    }
    
    if (details > 0) {
      MyOutput("OUTPUT/RESULTS.txt")
    }
  }
}






