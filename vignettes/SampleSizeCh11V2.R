## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)
library(ggplot2)

## ------------------------------------------------------------------------
alpha <- 0.05;cat("alpha = ", alpha, "\n")
fileName <- "VanDyke.lrc"
#fileName <- "Franken1.lrc"
rocData <- DfReadDataFile(fileName, format = "MRMC")
retDbm <- StSignificanceTesting(dataset = rocData, FOM = "Wilcoxon", method = "DBMH") 
varYTR <- retDbm$varComp$varComp[3];varYTC <- retDbm$varComp$varComp[4];varYEps <- retDbm$varComp$varComp[6]
effectSize <- retDbm$ciDiffTrtRRRC$Estimate

## ------------------------------------------------------------------------
#RRRC
J <- 10; K <- 163 # in the pivotal study
ncp <- (0.5*J*K*(effectSize)^2)/(K*varYTR+max(J*varYTC,0)+varYEps)
MS <- UtilMeanSquares(rocData, FOM = "Wilcoxon", method = "DBMH")
ddf <- (MS$msTR+max(MS$msTC-MS$msTRC,0))^2/(MS$msTR^2)*(J-1)
FCrit <- qf(1 - alpha, 1, ddf)
Power <- 1-pf(FCrit, 1, ddf, ncp = ncp)

## ------------------------------------------------------------------------
#FRRC
J <- 10; K <- 133 # in the pivotal study
ncp <- (0.5*J*K*(effectSize)^2)/(max(J*varYTC,0)+varYEps)
ddf <- (K-1)
FCrit <- qf(1 - alpha, 1, ddf)
Power <- 1-pf(FCrit, 1, ddf, ncp = ncp)

## ------------------------------------------------------------------------
#RRFC
J <- 10; K <- 53 # in the pivotal study
ncp <- (0.5*J*K*(effectSize)^2)/(K*varYTR+varYEps)
ddf <- (J-1)
FCrit <- qf(1 - alpha, 1, ddf)
Power <- 1-pf(FCrit, 1, ddf, ncp = ncp)

