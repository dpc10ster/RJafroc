## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)
library(ggplot2)

## ---- fig.align = "center"-----------------------------------------------
ndf <- 1;ddf <- 10;ncp <- c(0,2,5,10)
fCrit <- qf(0.95, ndf,ddf)
cat("critical value of x for rejecting NH is ", fCrit,"\n")
x <- seq(1, 20, 0.1)
myLabel <- c("A", "B", "C", "D")
myLabelIndx <- 1
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  cat("ndf = ", ndf, ", ddf = ", ddf, ", ncp = ", ncp[i], ", prob > fCrit = ", 
      1-pf(fCrit, ndf, ddf, ncp = ncp[i]), "\n")
  curveData <- data.frame(x = x, pdf = y)
  curvePlot <- ggplot(data = curveData, mapping = aes(x = x, y = pdf)) + 
    geom_line() +
    ggtitle(myLabel[myLabelIndx]);myLabelIndx <- myLabelIndx + 1
  print(curvePlot)
}

## ------------------------------------------------------------------------
alpha <- 0.05;cat("alpha = ", alpha, "\n")
fileName <- "VanDyke.lrc"
#fileName <- "Franken1.lrc"
rocData <- DfReadDataFile(fileName, format = "MRMC")
retDbm <- StSignificanceTesting(dataset = rocData, FOM = "Wilcoxon", method = "DBMH") 
varYTR <- retDbm$varComp$varComp[3];varYTC <- retDbm$varComp$varComp[4];varYEps <- retDbm$varComp$varComp[6]
effectSize <- retDbm$ciDiffTrtRRRC$Estimate
cat("effect size = ", effectSize, "\n")

## ------------------------------------------------------------------------
#RRRC
J <- 10; K <- 163
ncp <- (0.5*J*K*(effectSize)^2)/(K*varYTR+max(J*varYTC,0)+varYEps)
MS <- UtilMeanSquares(rocData, FOM = "Wilcoxon", method = "DBMH")
ddf <- (MS$msTR+max(MS$msTC-MS$msTRC,0))^2/(MS$msTR^2)*(J-1)
FCrit <- qf(1 - alpha, 1, ddf)
Power <- 1-pf(FCrit, 1, ddf, ncp = ncp)
cat("J =", J, ", K =", K,", FCrit =", FCrit, ", ddf =", ddf, ", ncp =", ncp, ", RRRC power =", Power, "\n")

## ------------------------------------------------------------------------
#FRRC
J <- 10; K <- 133
ncp <- (0.5*J*K*(effectSize)^2)/(max(J*varYTC,0)+varYEps)
ddf <- (K-1)
FCrit <- qf(1 - alpha, 1, ddf)
Power <- 1-pf(FCrit, 1, ddf, ncp = ncp)
cat("J =", J, ", K =", K,", FCrit =", FCrit, ", ddf =", ddf, ", ncp =", ncp, ", FRRC power =", Power, "\n")

## ------------------------------------------------------------------------
#RRFC
J <- 10; K <- 53
ncp <- (0.5*J*K*(effectSize)^2)/(K*varYTR+varYEps)
ddf <- (J-1)
FCrit <- qf(1 - alpha, 1, ddf)
Power <- 1-pf(FCrit, 1, ddf, ncp = ncp)
cat("J =", J, ", K =", K,", FCrit =", FCrit, ", ddf =", ddf, ", ncp =", ncp, ", RRFC power =", Power, "\n")

