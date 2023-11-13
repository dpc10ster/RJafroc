#' Performs DBM or OR significance testing for factorial dataset Old Code
#' 
#' @description  (DBM) or Obuchowski-Rockette (OR) 
#'    significance testing, for specified dataset. 
#'
#' @param dataset The dataset to be analyzed. 
#'     
#' @param FOM The figure of merit
#' 
#' @param FPFValue Only needed for \code{LROC} data.
#'     
#' @param alpha The significance level of the test; the default is 0.05
#'    
#' @param method The testing method to be used:  
#'    \code{"DBM"} (the default), representing the 
#'    Dorfman-Berbaum-Metz method or \code{"OR"}, representing the 
#'    Obuchowski-Rockette method.
#'    
#' @param covEstMethod The covariance matrix estimation method
#' 
#' @param nBoots The number of bootstraps (defaults to 200) 
#'    
#' @param analysisOption Determines which factors are regarded as random vs. fixed:
#' 
#' @keywords internal
#' 
#' @export

# 
# Code from version 0.0.1 of RJafroc (see RJafroc_0.0.1.tar) 
# with corrections that fixes the Lucy McGowan found bug in version 0.0.1. 
# This is intended to check the current code for errors that might creep in as 
# I attempt to improve the organization of the code and the output.  
# 
SigTestOldCode <- function(dataset, FOM, FPFValue = 0.2, alpha = 0.05, method = "DBM", 
                              covEstMethod = "jackknife", nBoots = 200, analysisOption = "ALL")
{
  
  if (dataset$descriptions$design != "FCTRL") stop("old code requires FCTRL study design")
  
  if (method == "DBM"){
    
    return(DBMHAnalysis(dataset, FOM, alpha, analysisOption)) # original code: StOldCode.R
    
  } else if (method == "OR"){
    
    return(ORHAnalysis(dataset, FOM, alpha, covEstMethod, nBoots, analysisOption)) # original code: StOldCode.R
    
  } else {
    errMsg <- sprintf("analysis method must be `DBM` or `OR`", method)
    stop(errMsg)
  }
}


