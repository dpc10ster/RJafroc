## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)

## ---- fig.align = "center"-----------------------------------------------
ndf <- 2;ddf <- 10;ncp <- c(0,2,5,10)
fCrit1 <- qf(0.95, ndf,ddf)
cat("critical value of x for rejecting NH is ", fCrit1,"\n")
x <- seq(1, 20, 0.1)
myLabel <- c("A", "B", "C", "D")
myLabelIndx <- 1
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  cat("ndf = ", ndf, ", ddf = ", ddf, ", ncp = ", ncp[i], ", prob > fCrit1 = ", 
      1-pf(fCrit1, ndf, ddf, ncp = ncp[i]), "\n")
  curveData <- data.frame(x = x, pdf = y)
  curvePlot <- ggplot(data = curveData, mapping = aes(x = x, y = pdf)) + 
    geom_line() +
    ggtitle(myLabel[myLabelIndx]);myLabelIndx <- myLabelIndx + 1
  print(curvePlot)
}

## ---- fig.align = "center"-----------------------------------------------
ndf <- 2;ddf <- 100
fCrit2 <- qf(0.95, ndf,ddf)
cat("critical value of x for rejecting NH is ", fCrit2,"\n")
x <- seq(1, 20, 0.1)
myLabel <- c("E", "F", "G", "H")
myLabelIndx <- 1
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  cat("ndf = ", ndf, ", ddf = ", ddf, ", ncp = ", ncp[i], ", prob > fCrit2 = ", 
      1-pf(fCrit2, ndf, ddf, ncp = ncp[i]), "\n")
  curveData <- data.frame(x = x, pdf = y)
  curvePlot <- ggplot(data = curveData, mapping = aes(x = x, y = pdf)) + 
    geom_line() +
    ggtitle(myLabel[myLabelIndx]);myLabelIndx <- myLabelIndx + 1
  print(curvePlot)
}

## ---- fig.align = "center"-----------------------------------------------
ndf <- 1;ddf <- 100
fCrit3 <- qf(0.95, ndf,ddf)
cat("critical value of x for rejecting NH is ", fCrit3,"\n")
x <- seq(1, 20, 0.1)
myLabel <- c("I", "J", "K", "L")
myLabelIndx <- 1
for (i in 1:length(ncp))
{
  y <- df(x,ndf,ddf,ncp=ncp[i])
  cat("ndf = ", ndf, ", ddf = ", ddf, ", ncp = ", ncp[i], ", prob > fCrit3 = ", 
      1-pf(fCrit3, ndf, ddf, ncp = ncp[i]), "\n")
  curveData <- data.frame(x = x, pdf = y)
  curvePlot <- ggplot(data = curveData, mapping = aes(x = x, y = pdf)) + 
    geom_line() +
    ggtitle(myLabel[myLabelIndx]);myLabelIndx <- myLabelIndx + 1
  print(curvePlot)
}

