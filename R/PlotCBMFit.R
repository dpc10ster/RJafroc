#' Plot CBM fitted curve
#' 
#' @description  Plot the CBM-predicted ROC curve with provided CBM parameters
#' 
#' @param mu vector: the mean(s) of the z-samples of the diseased distribution(s) where the disease is visible
#' @param alpha vector: the proportion(s) of the diseased distribution(s) where the disease is visible
#' 
#' @details \code{mu} and \code{alpha} must have equal length. 
#' The predicted ROC curve for each \code{mu} and \code{alpha} pair will be plotted.
#' 
#' @return A \pkg{ggplot2} object of the plotted ROC curve(s) 
#' 
#' @examples 
#' cbmPlot <- PlotCbmFit(c(1, 2), c(0.5, 0.5))
#' print(cbmPlot)
#' 
#' 
#' @references 
#' Dorfman DD, Berbaum KS (2000) A contaminated binormal model for ROC data: Part II. 
#' A formal model, Acad Radiol 7, 427--437.
#' 
#' 
#' @import ggplot2
#' @export

PlotCbmFit <- function(mu, alpha){
  if (length(mu) != length(alpha))
    stop("The lengths of mu and alpha do not match.")
  plotZeta <- seq(-20, 20, by = 0.01)
  plotCBM <- NULL
  for (i in 1:length(mu)){
    FPF <- 1 - pnorm(plotZeta)
    TPF <- (1 - alpha[i]) * (1 - pnorm(plotZeta)) + alpha[i] * (1 - pnorm(plotZeta, mean = mu[i]))
    plotCBM <- rbind(plotCBM, data.frame(FPF = FPF, TPF = TPF, Treatment = as.character(i)))
  }
  cbmROCCurve <- with(plotCBM,{
    ggplot() + geom_line(mapping = aes(x = FPF, y = TPF, color = Treatment), data = plotCBM, size = 1)
  })
  
  return(cbmROCCurve)
}