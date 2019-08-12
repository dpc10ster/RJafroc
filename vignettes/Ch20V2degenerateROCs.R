## ----setup, include = FALSE----------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)
  library(ggplot2)

## ----echo=FALSE----------------------------------------------------------
BMPoints <- function(a, b){
  plotZeta <- seq(-20, 20, by = 0.01)
  FPF <- 1 - pnorm(plotZeta)
  FPF <- c(1, FPF, 0)
  TPF <- pnorm(a - b * plotZeta)  
  TPF <- c(1, TPF, 0)
  plotCurve <- data.frame(FPF = FPF, TPF = TPF)
  return(plotCurve)
}

CBMPoints <- function(mu, alpha){
  plotZeta <- seq(-20, 20, by = 0.01)
  FPF <- 1 - pnorm(plotZeta)
  FPF <- c(1, FPF, 0)
  TPF <- (1 - alpha) * (1 - pnorm(plotZeta)) + alpha * (1 - pnorm(plotZeta, mean = mu))
  TPF <- c(1, TPF, 0)
  plotCurve <- data.frame(FPF = FPF, TPF = TPF)
  return(plotCurve)
}

## ---- fig.align = "center"-----------------------------------------------
plotOP <- data.frame(FPF = 0, TPF = 0.75)
a <- 0.6744898; b <- 0
plotCurve <- BMPoints(a, b)
figA <- ggplot(mapping = aes(x = FPF, y = TPF)) + 
  geom_line(data = plotCurve) + 
  geom_point(data = plotOP)  + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("A")
print(figA)

## ------------------------------------------------------------------------
a <- 1.281552; b <- 0
plotCurve <- BMPoints(a, b)
figB <- ggplot(mapping = aes(x = FPF, y = TPF)) + 
  geom_line(data = plotCurve) + 
  geom_point(data = plotOP)  + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("B")

a <- Inf; b <- 0
plotCurve <- BMPoints(a, b)
figC <- ggplot(mapping = aes(x = FPF, y = TPF)) + 
  geom_line(data = plotCurve) + 
  geom_point(data = plotOP)  + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("C")
print(figB);print(figC)

## ---- fig.align = "center"-----------------------------------------------
mu <- Inf; alpha <- 0.75
plotCurve <- CBMPoints(mu, alpha)
figD <- ggplot(mapping = aes(x = FPF, y = TPF)) + 
  geom_line(data = plotCurve) + 
  geom_point(data = plotOP)  + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("D")
print(figD)

