context("PlotBinormalFit tests")
library(RJafroc)

test_that("PlotBinormalFit should return a gg/ggplot", {
    aArray <- c(0.7, 0.7, 1.5, 2)
    bArray <- c(0.5, 1.5, 0.5, 0.5)
    p <- PlotBinormalFit(aArray, bArray)
    expect_is( (p),c("gg","ggplot") )
})

