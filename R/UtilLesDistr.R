#' The \code{lesionID} distribution of a dataset \bold{or} a supplied 1D-array
#' 
#' @description The \code{lesionID} distribution of a dataset \bold{or} of a supplied 
#'   1D-array. The \code{lesionID} field is described in the format of the Excel 
#'   input file, 
#'   see \href{https://dpc10ster.github.io/RJafrocQuickStart/quick-start-froc.html}{QuickStart}.
#'   (use Command click to view the link).
#' 
#' @param dsOrArr A dataset object or a 1D-array. 
#'
#' @return A data frame containing the number of \code{lesionID}s per case, 
#'   \code{lesID}, and the frequency distribution of the \code{lesionID}s,
#'   \code{Freq}.
#' 
#' @details Apart from ratings two characteristics of an FROC dataset 
#'    affect the FOM: the distribution of \code{lesionID}s per case and the lesion 
#'    weights. This function addresses the former. 
#'    The latter is addressed in \link{UtilLesWghts}. 
#'    The return value \code{LD} is a dataframe containing 2 equal length 
#'    vectors: the \code{lesionID} labels and the corresponding fractions of 
#'    lesions per diseased case in the dataset. For ROC or LROC data this 
#'    vector is always \code{c(1)}, since all diseased cases contain one lesion. 
#'    For FROC data the first element of the dataframe, i.e., \code{lesID}, contains 
#'    the numbers of lesions per diseased case. The second element, i.e., 
#'    \code{Freq}, contains the fraction of dis. cases containing one lesion, 
#'    the fraction containing two lesions, etc. 
#'    See \link{PlotRsmOperatingCharacteristics} for a function that depends 
#'    on \code{LD}.
#' 
#' 
#' @examples
#' UtilLesDistr(dataset01) # FROC dataset
#' ##    lesID       Freq
#' ## 1      1 0.93258427
#' ## 2      2 0.06741573 
#' ## In the Excel input file, 93 percent of lesions have lesionID = 1 
#' ## and the rest have lesionID = 2
#' 
#' 
#' UtilLesDistr(dataset02) # ROC dataset
#' ##       lesID Freq
#' ## 1         1    1
#' ## In the Excel input file all dis. cases have one lesion
#' 
#' UtilLesDistr(datasetCadLroc) # LROC dataset
#' ##       lesID Freq
#' ## 1         1    1
#' ## In the Excel input file all dis. cases have one lesion
#' 
#' UtilLesDistr(c(0.5, 0.3, 0.1, 0.1))
#' ##      lesID Freq
#' ## 1        1  0.5
#' ## 2        2  0.3
#' ## 3        3  0.1
#' ## 4        4  0.1  
#' ## An example of array input; 50 percent of the cases have lesionID = 1, 
#' ## 30 percent have lesionID = 2, etc.  
#' 
#' UtilLesDistr(dataset11) ## big froc dataset
#' ##     lesID        Freq
#' ## 1       1 0.217391304
#' ## 2       2 0.200000000
#' ## 3       3 0.113043478
#' ## 4       4 0.086956522
#' ## 5       5 0.043478261
#' ## 6       6 0.095652174
#' ## 7       7 0.052173913
#' ## 8       8 0.069565217
#' ## 9       9 0.017391304
#' ## 10     10 0.026086957
#' ## 11     11 0.026086957
#' ## 12     12 0.026086957
#' ## 16     16 0.017391304
#' ## 20     20 0.008695652
#' 
#' ## This dataset has lots of lesions (3D imaging for lung cancer). 
#' ## The lesionIDs range from 1 to 20 with a few missing, 
#' ## e.g., lesionID = 13 is not present in any diseased case.
#' ## Cases with lesionID = 1 have frequency 0.217, those with lesionID = 16 
#' ## have frequency 0.174, those with lesionID = 20 have frequency 0.00870, etc.
#' 
#' @export
 
UtilLesDistr <- function(dsOrArr)
{ 
  if (is.list(dsOrArr) && (length(dsOrArr) == 3)) {
    # this is a dataset object
    lesNum <- dsOrArr$lesions$perCase
    maxLL <- max(lesNum)
    tabLesNum <- tabulate(lesNum)
    LD <- data.frame(lesID = seq(1:maxLL), Freq = tabLesNum/sum(tabLesNum))
  } else if (is.vector(dsOrArr)) {
    #if (length(which(dsOrArr == 0)) != 0) stop("Zeroes not allowed in array input")
    maxLL <- length(dsOrArr)
    Freq <- dsOrArr/sum(dsOrArr)  # the frequencies have been already supplied
    lesNum <- seq(1, maxLL)
    LD <- data.frame(lesID = lesNum, Freq = Freq)
  } else stop("Must supply dataset or 1D array argument to UtilLesDistr()")
  
  # remove all lesion IDs that do not occur in dsOrArr
  if (length(which(LD$Freq == 0)) != 0) LD <- LD[-which(LD$Freq == 0),]
  
  return(LD)
}
