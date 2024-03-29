% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{dataset01}
\alias{dataset01}
\title{TONY FROC dataset}
\format{
A list with 3 elements: \code{$ratings}, \code{$lesions} and \code{$descriptions}; \code{$ratings}
   contain 3 elements, \code{$NL}, \code{$LL} and \code{$LL_IL} as sub-lists; \code{$lesions}
   contain 3 elements, \code{$perCase}, \code{$IDs} and \code{$weights} as sub-lists; \code{$descriptions}
   contain 7 elements, \code{$fileName}, \code{$type}, \code{$name},
   \code{$truthTableStr}, \code{$design}, \code{$modalityID} and \code{$readerID} as sub-lists;
\itemize{
\item{\code{rating$NL}}{, num [1:2, 1:5, 1:185, 1:3], ratings of non-lesion localizations, NLs}
\item{\code{rating$LL}}{, num [1:2, 1:5, 1:89, 1:2], ratings of lesion localizations, LLs}
\item{\code{rating$LL_IL}}{NA, this placeholder is used only for LROC data}
\item{\code{lesions$perCase}}{, int [1:89], number of lesions per diseased case}
\item{\code{lesions$IDs}}{, num [1:89, 1:2], numeric labels of lesions on diseased cases}
\item{\code{lesions$weights}}{, num [1:89, 1:2], weights (or clinical importances) of lesions}
\item{\code{descriptions$fileName}}{, chr, "dataset01", base name of dataset in `data` folder}
\item{\code{descriptions$type}}{, chr "FROC", the data type}
\item{\code{descriptions$name}}{, chr "TONY", the name of the dataset}
\item{\code{descriptions$truthTableStr}}{, num [1:2, 1:5, 1:185, 1:3] 1 1 1 1 ..., truth table structure}
\item{\code{descriptions$design}}{, chr "FCTRL", study design, factorial dataset}
\item{\code{descriptions$modalityID}}{, chr [1:2] "BT" "DM", modality labels}
\item{\code{descriptions$readerID}}{, chr [1:5] "1" "2" "3" "4" ..., reader labels}
}
}
\usage{
dataset01
}
\description{
This is referred to in the book as the "TONY" dataset. It consists of 185 cases, 
89 of which are diseased, interpreted in two treatments
("BT" = breast tomosynthesis and "DM" = digital mammography) by five radiologists using the FROC paradigm.
}
\examples{
res <- str(dataset01)
## PlotEmpiricalOperatingCharacteristics(dataset = dataset01, opChType = "wAFROC")$Plot

}
\references{
Chakraborty DP, Svahn T (2011) Estimating the parameters of a model 
of visual search from ROC data: an alternate method for fitting proper ROC curves. 
PROC SPIE 7966.
}
\keyword{datasets}
