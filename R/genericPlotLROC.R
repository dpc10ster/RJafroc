LrocOperatingPointsFromRatings <- function( zk1, zk2Cl ) 
{
  FPF <- 1
  PCL <- NULL
  zk11 <- zk1;zk2Cl1 <- zk2Cl
  while(1) {
    cutoff <- min( c( zk11, zk2Cl1 ) )
    zk11 <- zk1[ zk1 > cutoff ]
    zk2Cl1 <- zk2Cl[ zk2Cl > cutoff ]
    FPF1 <- length( zk11 ) / length( zk1 )
    PCL1 <- length( zk2Cl1 ) / length( zk2Cl )
    FPF <- c( FPF, FPF1 )
    if (length(PCL) == 0) PCL <- c(PCL1, PCL1) else PCL <- c( PCL, PCL1 )
    if( FPF1 == 0 && PCL1 == 0 ) {
      break
    }
  }  
  return( list(
    FPF = FPF[length(FPF):1],
    PCL = PCL[length(PCL):1]
  ) )
}




LrocPlots <- function (zjk1, zjk2,doJ) 
{
  J <- length(zjk1[,1])
  linetype <- c(1:J) 
  plotchar <- seq(13,13+J,1)
  j <- 1;zk1 <- zjk1[j,];zk2Cl1 <- zjk2[j,]
  lroc <- LrocOperatingPointsFromRatings( zk1, zk2Cl1 )
  FPF <- lroc$FPF;PCL <- lroc$PCL
  lrocPlotData <- data.frame(FPF = FPF, PCL = PCL, reader = "R-CAD")
  for (j in 2:J) {
    if ((j - 1) %in% doJ) {
      zk1 <- zjk1[j,]
      zk2Cl1 <- zjk2[j,]    
      lroc <- LrocOperatingPointsFromRatings( zk1, zk2Cl1 )
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
