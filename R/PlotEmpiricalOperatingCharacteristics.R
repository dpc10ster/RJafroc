#' Plot empirical operating characteristics, ROC, FROC or LROC
#'
#' Plot empirical operating characteristics (operating points connected by
#' straight lines) for specified modalities and readers, or, if desired, plots
#' (no operating points) averaged over specified modalities and / or readers.
#'
#' @param dataset Dataset object.
#' @param trts List or vector: \strong{integer} indices of modalities to be
#'   plotted. Default is 1.
#' @param rdrs List or vector: \strong{integer} indices of readers to be
#'   plotted. Default is 1.
#' @param opChType Type of operating characteristic to be plotted: \code{"ROC"},
#'   \code{"FROC"}, \code{"AFROC"},  \code{"wAFROC"}, \code{"AFROC1"},
#'   \code{"wAFROC1"}, or \code{"LROC"}.
#' @param legend.position Where to position the legend. The default is c(0.8,
#'   0.2), i.e., 0.8 rightward and 0.2 upward (the plot is a unit square).
#' @param maxDiscrete maximum number of op. points in order to be considered
#'   discrete and to be displayed by symbols and connecting lines; any more
#'   points will be regarded as continuous and only connected by lines; default
#'   is 10.
#'
#' @details The \code{trts} and \code{rdrs} are vectors or lists of
#'   \strong{integer} indices, not the corresponding \strong{string} IDs. For
#'   example, if the string ID of the first reader is "0", the value in
#'   \code{rdrs} should be \strong{1} not \strong{0}. The legend will display
#'   the string IDs.
#'
#'   If both of \code{trts} and \code{rdrs} are vectors, all combinations of
#'   modalities and readers are plotted. See Example 1.
#'
#'   If both \code{trts} and \code{rdrs} are \code{lists}, they must have the
#'   same length. Only the combination of modality and reader at the same
#'   position in their respective lists are plotted. If some elements of the
#'   modalities and / or readers lists are vectors, the average operating
#'   characteristic over the implied modalities and / or readers are plotted.
#'   See Example 2.
#'
#'   For \code{LROC} datasets, \code{opChType} can be "ROC" or "LROC".
#'
#' @return A \pkg{ggplot2} object containing the operating characteristic
#'   plot(s) and a data frame containing the points defining the operating
#'   characteristics.
#'
#' @return \item{Plot}{\pkg{ggplot2} object. For continuous or averaged data,
#'   operating characteristics curves are plotted \strong{without} showing
#'   operating points. For binned (individual) data, both operating points and
#'   connecting lines are shown. To avoid clutter, if there are more than 20
#'   operating points, they are not shown.}
#'
#' @return \item{Points}{Data frame with four columns: abscissa, ordinate,
#'   \code{class} (which codes modality and reader names) and \code{type}, which
#'   can be \code{"D"} for discrete ratings, \code{"C"} for continuous ratings,
#'   i.e., more than 20 operating points, or \code{"A"}, for reader averaged.}
#'
#'
#' @examples
#' ## Example 1
#' ## Plot individual empirical ROC plots for all combinations of modalities
#' ## 1 and 2 and readers 1, 2 and 3. Six operating characteristics are plotted.
#'
#' ret <- PlotEmpiricalOperatingCharacteristics(dataset =
#' dataset02, trts = c(1:2), rdrs = c(1:3), opChType = "ROC")
#' ## print(ret$Plot)
#'
#' ## Example 2
#' ## Empirical wAFROC plots, consisting of
#' ## three sub-plots:
#' ## (1) sub-plot, red, with operating points, for the 1st modality (string ID "1") and the 2nd
#' ## reader (string ID "3"), labeled "M:1 R:3"
#' ## (2) sub-plot, green, no operating points, for the 2nd modality (string ID "2") AVERAGED
#' ## over the 2nd and 3rd readers (string IDs "3" and "4"), labeled "M:2  R: 3 4"
#' ## (3) sub-plot, blue, no operating points, AVERAGED over the first two modalities
#' ## (string IDs "1" and "2") AND over the 1st, 2nd and 3rd readers
#' ## (string IDs "1", "3" and "4"), labeled "M: 1 2  R: 1  3 4"
#'
#' plotT <- list(1, 2, c(1:2))
#' plotR <- list(2, c(2:3), c(1:3))
#'
#' ret <- PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = plotT,
#'    rdrs = plotR, opChType = "wAFROC")
#' ## print(ret$Plot)
#'
#' ## Example 3
#' ## Correspondences between indices and string identifiers for modalities and
#' ## readers in this dataset (apparently reader "2" did not complete the study).
#'
#' ## names(dataset04$descriptions$readerID)
#' ## [1] "1" "3" "4" "5"
#'
#' @importFrom  ggplot2 ggplot aes geom_point geom_line theme
#' @export

####################################################################################################################
PlotEmpiricalOperatingCharacteristics <- function(dataset, trts = 1, rdrs = 1, opChType, legend.position = c(0.8, 0.3), maxDiscrete = 10) 
{
  
  if (dataset$descriptions$type == "ROI") stop("No operating characteristics are defined for an ROI dataset")
  
  if (opChType == "ROC"){
    if (dataset$descriptions$type == "FROC") ds <- DfFroc2Roc(dataset) 
    else if (dataset$descriptions$type == "LROC") ds <- DfLroc2Roc(dataset) 
    else ds <- dataset
  } else if ((opChType %in% c("FROC", "AFROC", "wAFROC", "AFROC1", "wAFROC1")) && (dataset$descriptions$type == "FROC")) {
    ds <- dataset
  } else if ((opChType == "LROC") && (dataset$descriptions$type == "LROC")) {
    ds <- dataset
  } else {
    errMsg <- sprintf("%s is not a valid operating characteristic for this dataset", opChType)
    stop(errMsg)
  }
  
  ret <- gpfPlotGenericEmpiricalOperatingCharacteristic(ds, trts, rdrs, opChType = opChType, legend.position, maxDiscrete)
  
  return(ret)
} 




