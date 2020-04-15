## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
library(kableExtra)

## ---- fig.show='hold'---------------------------------------------------------
ndf <- 2;ddf <- 10;ncp <- c(0,2,5,10)
alpha <- 0.05
fCrit <- qf(1-alpha, ndf,ddf)
x <- seq(1, 20, 0.1)
myLabel <- c("A", "B", "C", "D")
myLabelIndx <- 1
pFgtFCrit <- NULL
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  pFgtFCrit <- c(pFgtFCrit, 1-pf(fCrit, ndf, ddf, ncp = ncp[i]))
}  
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  curveData <- data.frame(x = x, pdf = y)
  curvePlot <- ggplot(data = curveData, mapping = aes(x = x, y = pdf)) + 
    geom_line() +
    ggtitle(myLabel[myLabelIndx]);myLabelIndx <- myLabelIndx + 1
  print(curvePlot)
}
fCrit_2_10 <- fCrit # convention fCrit_ndf_ddf

## ---- echo=FALSE--------------------------------------------------------------
rowNames <- LETTERS[seq(1, 4)]
myTab <- data.frame(ndf = rep(ndf, 4), ddf = rep(ddf, 4), 
                    fCrit = rep(fCrit, 4), ncp = ncp, 
                    pFgtFCrit = pFgtFCrit)
row.names(myTab) <- rowNames
kable(myTab)

## ---- fig.show='hold', echo=FALSE---------------------------------------------
ndf <- 2;ddf <- 100
fCrit <- qf(1-alpha, ndf,ddf)
x <- seq(1, 20, 0.1)
myLabel <- c("E", "F", "G", "H")
myLabelIndx <- 1
pFgtFCrit <- NULL
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  pFgtFCrit <- c(pFgtFCrit, 1-pf(fCrit, ndf, ddf, ncp = ncp[i]))
}  
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  curveData <- data.frame(x = x, pdf = y)
  curvePlot <- ggplot(data = curveData, mapping = aes(x = x, y = pdf)) + 
    geom_line() +
    ggtitle(myLabel[myLabelIndx]);myLabelIndx <- myLabelIndx + 1
  print(curvePlot)
}
fCrit_2_100 <- fCrit # convention fCrit_ndf_ddf

## ---- echo=FALSE--------------------------------------------------------------
rowNames <- LETTERS[seq(5, 8)]
temp <- data.frame(ndf = rep(ndf, 4), ddf = rep(ddf, 4), 
                    fCrit = rep(fCrit, 4), ncp = ncp, 
                    pFgtFCrit = pFgtFCrit)
row.names(temp) <- rowNames
myTab <- rbind(myTab, temp)
kable(myTab)

## ---- fig.show='hold', echo=FALSE---------------------------------------------
ndf <- 1;ddf <- 100
fCrit <- qf(1-alpha, ndf,ddf)
x <- seq(1, 20, 0.1)
myLabel <- c("I", "J", "K", "L")
pFgtFCrit <- NULL
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  pFgtFCrit <- c(pFgtFCrit, 1-pf(fCrit, ndf, ddf, ncp = ncp[i]))
}  
myLabelIndx <- 1
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  curveData <- data.frame(x = x, pdf = y)
  curvePlot <- ggplot(data = curveData, mapping = aes(x = x, y = pdf)) + 
    geom_line() +
    ggtitle(myLabel[myLabelIndx]);myLabelIndx <- myLabelIndx + 1
  print(curvePlot)
}
fCrit_1_100 <- fCrit # convention fCrit_ndf_ddf

## ---- echo=FALSE--------------------------------------------------------------
rowNames <- LETTERS[seq(9, 12)]
temp <- data.frame(ndf = rep(ndf, 4), ddf = rep(ddf, 4), 
                    fCrit = rep(fCrit, 4), ncp = ncp, 
                    pFgtFCrit = pFgtFCrit)
row.names(temp) <- rowNames
myTab <- rbind(myTab, temp)
kable(myTab)

