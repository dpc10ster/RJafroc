## ----setup, include = FALSE---------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )
  library(RJafroc)
  library(ggplot2)

## ---- fig.align = "center"----------------------------------------------------
  aArray <- c(0.7, 0.7, 1.5, 2)
  bArray <- c(0.5, 1.5, 0.5, 0.5)
  chance_diag <- data.frame(x = c(0,1), y = c(0,1))
  p <- PlotBinormalFit(aArray, bArray) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position = c(0.85, 0.2))
p <- p + geom_line(data = chance_diag, aes(x = x, y = y), linetype="dotted")
print(p)

## ---- echo=FALSE--------------------------------------------------------------
# x is rocX, i.e., FPF
rocY <- function (x, a, b) {
  y <- pnorm(a + b*qnorm(x))
  return(y)
}

z1 <- seq(-5, 3, by = 0.01)
z2 <- seq(-5, 7, by = 0.01)
FPF <- seq(0.0, 1, 0.01)

pdf1 <- dnorm(z1)

aArray <- c(0.7, 0.7, 1.5, 2)
bArray <- c(0.5, 1.5, 0.5, 0.5)

for (i in 1:1)
{
  a <- aArray[i]
  b <- bArray[i]
  TPF <- rocY(FPF, a, b)
  rocPoints <- data.frame(FPF = FPF, TPF = TPF)
  p <- ggplot(rocPoints, aes(x = FPF, y = TPF)) + 
    geom_line()  + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) 
  p <- p + geom_line(data = chance_diag, aes(x = x, y = y), linetype="dotted")
  print(p)
  
  pdf2 <- dnorm(z2, a/b, sd = 1/b)
  df <- data.frame(z = c(z1, z2), pdfs = c(pdf1, pdf2), 
                   truth = c(rep('non-dis', length(pdf1)), 
                             rep('dis', length(pdf2))))
  
  p <- ggplot(df, aes(x = z, y = pdfs, color = truth)) + 
    geom_line() + 
    scale_colour_manual(values=c("darkgrey","black")) + 
    theme(legend.title = element_blank(), legend.text=element_text(size=7,face="bold"), 
          legend.position = c(0.8, 0.9),
          legend.direction = "horizontal") + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) 
  print(p)
  cat("a = ", a, ", b = ", b, "\n")
}

