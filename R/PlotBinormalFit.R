#' Plot binormal fit
#' 
#' @description Plot the binormal-predicted ROC curve with provided parameters 
#' 
#' @param a vector: the mean(s) of the diseased distribution(s).
#' @param b vector: the standard deviations(s) of the diseased distribution(s).
#' 
#' @details \code{a} and \code{b} must have the same length. The predicted ROC curve 
#'    for each \code{a} and \code{b} pair will be plotted.
#' 
#' @return A \pkg{ggplot2} object of the plotted ROC curve(s) are returned. 
#'    Use \code{print} function to display the saved object.
#' 
#' @examples 
#' binormalPlot <- PlotBinormalFit(c(1, 2), c(0.5, 0.5))
#' print(binormalPlot)
#' 
#' @import ggplot2
#' @export

PlotBinormalFit <- function(a, b){
  if (length(a) != length(b))
    stop("The lengths of a and b do not match.")
  plotZeta <- seq(-3, 20, by = 0.1)
  plotBM <- NULL
  for (i in 1:length(a)){
    FPF <- 1 - pnorm(plotZeta)
    FPF <- c(1, FPF, 0)
    TPF <- pnorm(a[i] - b[i] * plotZeta)  
    TPF <- c(1, TPF, 0)
    
    plotBM <- rbind(plotBM, data.frame(FPF = FPF, TPF = TPF, Treatment = as.character(i)))
  }
  bmROCCurve <- with(plotBM,{
    ggplot() + geom_line(mapping = aes(x = FPF, y = TPF, color = Treatment), data = plotBM, size = 1)
  })
  
  return(bmROCCurve)
}