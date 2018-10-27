genericPlotROC <- function(fp, tp, fpfPred, tpfPred, method = "ROC") {
  ret1 <- RawOpPtsROC2ROC(fp, tp) 
  fpf <- ret1$fpf;tpf <- ret1$tpf
  
  color <- "black"
  ROCPred <- rbind(data.frame(fpf = fpfPred, tpf = tpfPred))
  ROCOpPoints <- rbind(data.frame(fpf = fpf, tpf = tpf))  
  dfROCPred <- data.frame(fpf = ROCPred$fpf, tpf = ROCPred$tpf, color = color, 
                          type = "individual")
  dfROCPoints <- data.frame(fpf = ROCOpPoints$fpf, tpf = ROCOpPoints$tpf, color = color, 
                            type = "individual")
  
  fittedPlot <- ggplot(mapping = aes(x = fpf, y = tpf), color = "black") + 
    geom_line(data = dfROCPred, size = 1) + 
    geom_point(data = dfROCPoints, size = 4)
  
  if (method == "RSM"){
    ROCDashes <- rbind(data.frame(fpf = c(fpfPred[1], 1), tpf = c(tpfPred[1], 1)))
    dfROCDashes <- data.frame(fpf = ROCDashes$fpf, tpf = ROCDashes$tpf, color = color, 
                              type = "individual")
    fittedPlot <- fittedPlot +
      geom_line(data = dfROCDashes, linetype = 3, size = 2)
  }
  
  if (TRUE){
    K1 <- length(fp)
    K2 <- length(tp)
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
        geom_line(data = barBtm, aes(x = fpf, y = tpf), color = "black") +
        xlab("FPF") + ylab("TPF")
    }
  }
  return(
    fittedPlot = fittedPlot
  )
}