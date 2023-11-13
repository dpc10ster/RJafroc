#' Performs DBM or OR significance testing for factorial or crossed modality datasets
#' 
#' @description  Performs DBM or OR significance testing for specified dataset. 
#'
#' @param dataset The dataset to be analyzed, see \code{\link{RJafroc-package}}. 
#'     The dataset design can be "FCTRL" or "FCTRL-X-MOD". 
#'     
#' @param FOM The figure of merit, see \code{\link{UtilFigureOfMerit}}
#' 
#' @param method The significance testing method to be used:  
#'    \code{"DBM"} for the Dorfman-Berbaum-Metz method or \code{"OR"}, 
#'    for the Obuchowski-Rockette method (default).  
#'    
#' @param covEstMethod The covariance matrix estimation method
#'    in \code{ORH} analysis (for \code{method = "DBM"} the jackknife is always used).
#'    \itemize{ 
#'    \item \code{"Jackknife"}, the default, 
#'    \item \code{"Bootstrap"}, in which case \code{nBoots} is relevant, default is 200, 
#'    \item \code{"DeLong"}; requires \code{FOM = "Wilcoxon" or "ROI" or "HrAuc"}.
#' }   
#' 
#' @param analysisOption Determines which factors are regarded as random and 
#'     which are fixed:
#' \itemize{ 
#'    \item \code{"RRRC"} = random-reader random case, the default,
#'    \item \code{"FRRC"} = fixed-reader random case, 
#'    \item \code{"RRFC"} = random-reader fixed case, 
#'    \item \code{"ALL"} =  all 3 allowed options.
#' }    
#' 
#' @param alpha The significance level (alpha) of the test of the null hypothesis 
#' that all modality effects are zero; the default is 0.05
#'    
#' @param FPFValue Only needed for \code{LROC} data \strong{and} FOM = "PCL" 
#'     or "ALROC"; where to evaluate a partial curve based figure of merit. 
#'     The default is 0.2.
#'     
#' @param nBoots The number of bootstraps (defaults to 200), only needed if 
#'    \code{covEstMethod = "bootstrap"} and \code{method = "OR"} 
#'    
#' @param seed For bootstraps the seed of the RNG (defaults to \code{NULL}), only 
#'     needed if \code{covEstMethod = "bootstrap"} and \code{method = "OR"} 
#'    
#'     
#' @return \strong{For \code{method = "DBM"} the returned list contains 4 dataframes:}
#' @return \item{FOMs}{Contains \code{foms}, \code{trtMeans} and \code{trtMeanDiffs}: 
#'    see return of \code{\link{UtilFigureOfMerit}}}
#' @return \item{ANOVA}{Contains \code{TRCAnova}, \code{VarCom}, \code{IndividualTrt} 
#'    and \code{IndividualRdr} ANOVA tables of pseudovalues}
#' @return \item{RRRC}{Contains results of \code{"RRRC"} analyses: \code{FTests}, 
#'    \code{ciDiffTrt}, \code{ciAvgRdrEachTrt}}
#' @return \item{FRRC}{Contains results of \code{"FRRC"} analyses: \code{FTests}, 
#'    \code{ciDiffTrt}, \code{ciAvgRdrEachTrt}, \code{ciDiffTrtEachRdr}}
#' @return \item{RRFC}{Contains results of \code{"RRFC"} analyses: \code{FTests}, 
#'    \code{ciDiffTrt}, \code{ciAvgRdrEachTrt}}
#' 
#' @return \strong{For \code{method = "OR"} the return list contains 4 dataframes:}
#' @return \item{FOMs}{Contains \code{foms}, \code{trtMeans} and \code{trtMeanDiffs}: 
#'    \code{\link{UtilFigureOfMerit}}}
#' @return \item{ANOVA}{Contains \code{TRAnova}, \code{VarCom}, \code{IndividualTrt} 
#'    and \code{IndividualRdr} ANOVA tables of FOM values}
#' @return \item{RRRC}{Contains results of \code{"RRRC"} analyses - same 
#'    organization as DBM, see above}
#' @return \item{FRRC}{Contains results of \code{"FRRC"} analyses - ditto}
#' @return \item{RRFC}{Contains results of \code{"RRFC"} analyses- ditto}
#' 
#' 
#' @examples
#' result <- St(dataset02,FOM = "Wilcoxon", method = "DBM") 
#' result <- St(dataset02,FOM = "Wilcoxon", method = "OR")
#' result <- St(datasetXModality, FOM = "wAFROC", method = "OR", analysisOption = "ALL")
#' 
#' \donttest{
#' result <- St(dataset05, FOM = "wAFROC")
#' result <- St(dataset05, FOM = "HrAuc", method = "DBM") 
#' } 
#'
#' 
#' @references
#' Dorfman DD, Berbaum KS, Metz CE (1992) ROC characteristic rating analysis: 
#' Generalization to the Population of Readers and Patients with the Jackknife method, Invest. Radiol. 27, 723-731.
#' 
#' Obuchowski NA, Rockette HE (1995) Hypothesis Testing of the Diagnostic Accuracy for Multiple Diagnostic Tests:  
#' An ANOVA Approach with Dependent Observations, Communications in Statistics: Simulation and Computation 24, 285-308.
#' 
#' Hillis SL (2014) A marginal-mean ANOVA approach for analyzing multireader multicase radiological imaging data, 
#' Statistics in medicine 33, 330-360.
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
               seed = NULL)
{
  
  isValidDataset(dataset, FOM, method, analysisOption, covEstMethod = covEstMethod)
  
  if (method == "DBM"){

    ret <- StDBMAnalysis(dataset, 
                          FOM, 
                          analysisOption,
                          alpha,
                          FPFValue)

    return(ret)
    
  } else { # method == "OR"
    
    ret <- StORAnalysis(dataset,
                        FOM,
                        method, 
                        covEstMethod, 
                        analysisOption,
                        alpha,
                        FPFValue,
                        nBoots, 
                        seed)
    
    return(ret)
    
  }
  
}

