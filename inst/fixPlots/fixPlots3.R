rm(list = ls())
library(RJafroc)
library(ggplot2)

ret <- PlotEmpOpChrs(dataset = 
                                               dataset02, trts = c(1:2), rdrs = c(1:3), opChType = "ROC")
print(ret$Plot)

plotT <- list(1, 2, c(1:2))
plotR <- list(2, c(2:3), c(1:3))

ret <- PlotEmpOpChrs(dataset = dataset04, trts = plotT, 
                                             rdrs = plotR, opChType = "wAFROC")                  
print(ret$Plot)

ret <- PlotEmpOpChrs(dataset = dataset04, trts = seq(1:5), 
                                             rdrs = seq(1:4), opChType = "wAFROC")                  
print(ret$Plot)
