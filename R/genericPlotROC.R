genericPlotROC <- function(fp, tp, fpfPred, tpfPred, errBar = TRUE, legendPosition = c(1,0)) {
  K1 <- length(fp)
  K2 <- length(tp)
  ret1 <- RawOpPtsROC2ROC(fp, tp) 
  fpf <- ret1$fpf;tpf <- ret1$tpf

  plotGeneric <- rbind(data.frame(fpf = fpfPred, tpf = tpfPred))
  plotOpPnts <- rbind(data.frame(fpf = fpf, tpf = tpf))
  fittedPlot <- ggplot(mapping = aes(x = fpf, y = tpf), color = "black") + 
    geom_line(data = plotGeneric, size = 1) + geom_point(data = plotOpPnts, size = 4) +
    theme(legend.position=legendPosition)
  if (errBar){
    ciX <- binom.confint(x = fpf * K1, n = K1, methods = "exact")
    ciY <- binom.confint(x = tpf * K2, n = K2, methods = "exact")
    ciXUpper <- ciX$upper
    ciXLower <- ciX$lower
    ciYUpper <- ciY$upper
    ciYLower <- ciY$lower
    for (pt in 1:length(fpf)){ 
      if (((pt != 1) && pt != length(fpf))) next
      ciX <- data.frame(fpf = c(ciXUpper[pt], ciXLower[pt]), tpf = c(tpf[pt], tpf[pt]))
      ciY <- data.frame(fpf = c(fpf[pt], fpf[pt]), tpf = c(ciYUpper[pt], ciYLower[pt]))
      fittedPlot <- fittedPlot + geom_line(data = ciY, aes(x = fpf, y = tpf), color = "black") + 
        geom_line(data = ciX, aes(x = fpf, y = tpf), color = "black")
      barRgt <- data.frame(fpf = c(ciXUpper[pt], ciXUpper[pt]), tpf = c(tpf[pt] - 0.01, tpf[pt] + 0.01))
      barLft <- data.frame(fpf = c(ciXLower[pt], ciXLower[pt]), tpf = c(tpf[pt] - 0.01, tpf[pt] + 0.01))
      barUp <- data.frame(fpf = c(fpf[pt] - 0.01, fpf[pt] + 0.01), tpf = c(ciYUpper[pt], ciYUpper[pt]))
      barBtm <- data.frame(fpf = c(fpf[pt] - 0.01, fpf[pt] + 0.01), tpf = c(ciYLower[pt], ciYLower[pt]))
      fittedPlot <- fittedPlot + 
        geom_line(data = barRgt, aes(x = fpf, y = tpf), color = "black") + 
        geom_line(data = barLft, aes(x = fpf, y = tpf), color = "black") + 
        geom_line(data = barUp, aes(x = fpf, y = tpf), color = "black") + 
        geom_line(data = barBtm, aes(x = fpf, y = tpf), color = "black")
    }
  }
  return(
    fittedPlot = fittedPlot
  )
}