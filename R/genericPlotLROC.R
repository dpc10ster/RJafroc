LrocOperatingPointsFromRatings <- function( zk1, zk2Cl ) 
{
  if (FALSE) { # this is to check the method, see ChkLrocFoms.xlsx
    zk1 <- c(rep(0, 4), 1.424443, rep(-50,5))
    zk2Cl <- c(0.5542791, 1.2046176, -50, 3.4596787, 2.732657)
    K2 <- length(zk2Cl)
    K1 <- length(zk1) - K2
    zk1 <- zk1[zk1 != -50]
    zk2Cl <- zk2Cl[zk2Cl != -50]
  } else {
    K2 <- length(zk2Cl)
    K1 <- length(zk1) - K2
    zk1 <- zk1[is.finite(zk1)]
    zk2Cl <- zk2Cl[is.finite(zk2Cl)]
  }
  
  FPF <- 1
  PCL <- NULL
  zk1Temp <- zk1;zk2ClTemp <- zk2Cl
  while(1) {
    cutoff <- min( c( zk1Temp, zk2ClTemp ) )
    zk1Temp <- zk1[ zk1 > cutoff ]
    zk2ClTemp <- zk2Cl[ zk2Cl > cutoff ]
    FPF1 <- length( zk1Temp ) / K1
    PCL1 <- length( zk2ClTemp ) / K2
    FPF <- c( FPF, FPF1 )
    if (length(PCL) == 0) PCL <- c(PCL1, PCL1) else PCL <- c( PCL, PCL1 )
    if( FPF1 == 0 && PCL1 == 0 ) {
      break
    }
  }
  
  FPF <- rev(FPF)
  PCL <- rev(PCL)
  
  return( list(
    FPF = FPF,
    PCL = PCL
  ) )
}




LrocPlots <- function (zjk1, zjk2,doJ) 
{
  J <- length(zjk1[,1])
  j <- 1;zjk1Temp <- zjk1[j,];zk2Temp <- zjk2[j,]
  lroc <- LrocOperatingPointsFromRatings( zjk1Temp, zk2Temp )
  FPF <- lroc$FPF;PCL <- lroc$PCL
  lrocPlotData <- data.frame(FPF = FPF, PCL = PCL, reader = "R-CAD")
  for (j in 2:J) {
    if ((j - 1) %in% doJ) {
      zjk1Temp <- zjk1[j,]
      zk2Temp <- zjk2[j,]    
      lroc <- LrocOperatingPointsFromRatings( zjk1Temp, zk2Temp )
      FPF <- lroc$FPF
      PCL <- lroc$PCL
      reader = paste0("R-", as.character(j - 1))
      lrocPlotData <- rbind(lrocPlotData, data.frame(FPF = FPF, PCL = PCL, reader = reader))
    }
  }
  
  lrocPlot <- ggplot(data = lrocPlotData, aes(x = FPF, y = PCL, color = reader)) + geom_line()
  g <- ggplot_build(lrocPlot)
  colors <- as.character(unique(g$data[[1]]$colour))
  colors[1] <- "#000000"
  sizes <- c(2, rep(1, length(doJ)))
  lrocPlot <- ggplot(data = lrocPlotData, aes(x = FPF, y = PCL, color = reader)) + geom_line(aes(size = reader)) + 
    scale_color_manual(values = colors) + scale_size_manual(values = sizes) + 
    theme(legend.title = element_blank(), legend.position = legendPosition <- c(1, 0), legend.justification = c(1, 0))
  return(list(
    lrocPlot = lrocPlot,
    afrocPlot = NULL)
  )
}
