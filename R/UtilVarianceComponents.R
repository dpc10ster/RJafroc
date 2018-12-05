#' ## potential project for summer student
#' #' Variance Components for DBM or Obuchowski-Rockette analysis
#' #'
#' #' @description Estimates pseudovalue variance components (denoted by Y 
#' #'    prefix to R, C, TR, ...) for Dorfman-Berbaum-Metz (DBM) or FOM 
#' #'    variance components for Obuchowski-Rockette (OR) methods for 
#' #'    the specified dataset.
#' #'
#' #' @param dataset {The dataset to be analyzed, see \code{\link{RJafroc-package}}}
#' #' @param FOM {The figure of merit to be used in the analysis, default 
#' #'    is \code{"wJAFROC"}, see \code{\link{UtilFigureOfMerit}}}
#' #' @param method The analysis method to be used. There are two options: 
#' #'    \code{"DBMH"} (default) or \code{"ORH"}, representing the 
#' #'    Dorfman-Berbaum-Metz and ther Obuchowski-Rockette methods, respectively.
#' #' @param covEstMethod For ORH analysis, the method used to estimate the 
#' #'    covariance matrix. It can be \code{"Jackknife"} (the default), 
#' #'    \code{"Bootstrap"} or \code{"DeLong"}; the last assumes the 
#' #'    Wilcoxon FOM is chosen, otherwise an error will result. This parameter 
#' #'    is not relevant if the analysis method is \code{"DBMH"}.
#' #' @param nBoots For ORH analysis, the number of bootstraps (default is 200), 
#' #'    used if the \code{"Bootstrap"} method is used to estimate the covariance matrix.
#' #'
#' #' @return For the \code{DBMH} method, the return value is a list with following 
#' #'    elements (all are pseduovalue based):
#' #' @return \item{varYR}{The reader variance component.}
#' #' @return \item{varYC}{The case variance component.}
#' #' @return \item{varYTR}{The treatment-reader variance component.}
#' #' @return \item{varYTC}{The treatment-case variance component.}
#' #' @return \item{varYRC}{The reader-case variance component.}
#' #' @return \item{varYEps}{The error variance component.}
#' #'
#' #' @return For the \code{ORH} method the return value is a list with following 
#' #'    elements (all are FOM based):
#' #' @return \item{varR}{The reader variance component component.}
#' #' @return \item{varTR}{The treatment-reader variance component. }
#' #' @return \item{cov1}{The cov1 component (same readers, different treatments).}
#' #' @return \item{cov2}{The cov2 component (different readers, same treatment).}
#' #' @return \item{cov3}{The reader-case component (different readers, different treatments).}
#' #' @return \item{varEps}{The diagonal term of the covariance matrix.}
#' #'
#' #' @examples
#' #' VarianceComponentsY  <- UtilVarianceComponents(dataset02, 
#' #'    FOM = "Wilcoxon", method = "DBMH")
#' #'
#' #' VarianceComponentsFom  <- UtilVarianceComponents(dataset02, 
#' #'    FOM = "Wilcoxon", method = "ORH")
#' #'
#' #' @export
#' UtilVarianceComponents <- function(dataset, FOM = "wJAFROC", method = "DBMH", 
#'                                    covEstMethod = "Jackknife", nBoots = 200)
#' {
#'   if (method == "DBMH"){
#'     ret <- StSignificanceTesting(dataset, FOM = FOM, method = "DBMH", alpha = 0.05, option = "RRRC", VarCompFlag = TRUE)
#'   }else if (method == "ORH"){
#'     ret <- StSignificanceTesting(dataset, FOM = FOM, method = "ORH", alpha = 0.05, covEstMethod, nBoots, 
#'                                  option = "RRRC", VarCompFlag = TRUE)
#'   }else{
#'     errMsg <- sprintf("%s is not a valid analysis method.", method)
#'     stop(errMsg)
#'   }
#'   return (ret)
#' }