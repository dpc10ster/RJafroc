#' Convert an FROC dataset to an AFROC dataset
#' 
#' @description Converts an FROC dataset to a AFROC dataset, where only the highest rated mark on each non-diseased case is counted 
#' and all lesion localizations are counted 
#' 
#' 
#' @param dataset The dataset to be converted, \code{\link{RJafroc-package}}.
#' 
#' @return An AFROC dataset
#' 
#' @details The first list member of the AFROC dataset is \code{NL}, whose third dimension 
#' has length \code{(K1 + K2)}, the total number of cases. The ratings of cases 
#' \code{(K1 + 1)} through \code{(K1 + K2)} are \code{-Inf}. \strong{In an AFROC 
#' dataset FPs are only possible on non-diseased cases.} The second member of the 
#' list is \code{LL}. Its third dimension has length \code{K2}, the total number 
#' of diseased cases. This is because LLs are only possible on diseased cases. The 
#' structure is shown below:
#' \itemize{
#' \item{\code{NL}}{ Ratings array [1:I, 1:J, 1:(K1+K2), 1:maxNL], of non-lesion localizations, NLs}
#' \item{\code{LL}}{ Ratings array [1:I, 1:J, 1:K2, 1:maxLL], of lesion localizations, LLs}
#' \item{\code{lesionVector}}{ array [1:K2], number of lesions per diseased case}
#' \item{\code{lesionID}}{ array [1:K2, 1:maxLL], labels of lesions on diseased cases}
#' \item{\code{lesionWeight}}{ array [1:K2, 1:maxLL], weights (or clinical importances) of lesions}
#' \item{\code{dataType}}{ "FROC", the data type}
#' \item{\code{modalityID}}{ [1:I] inherited modality labels}
#' \item{\code{readerID}}{ [1:J] inherited reader labels}
#' }
#'  
#'
#' @examples
#' afrocDataSet <- DfFroc2Afroc(dataset05)
#' p <- PlotEmpiricalOperatingCharacteristics(afrocDataSet, trts = 1, rdrs = 1, opChType = "wAFROC")
#' print(p$Plot)
#' str(afrocDataSet)
#' 
#' @export

DfFroc2Afroc <- function (dataset){
  if (dataset$dataType != "FROC") stop("The dataset has to be FROC")
  NL <- dataset$NL
  LL <- dataset$LL
  K <- dim(NL)[3]
  K2 <- dim(LL)[3]
  K1 <- K - K2 

  NL <- apply(NL, c(1, 2, 3), max)
  dim(NL) <- c(dim(NL), 1)
  dataset$NL <- NL[,,,1, drop = FALSE]
  dataset$NL[,,(K1+1):K,1] <- -Inf

  return (dataset)
}



