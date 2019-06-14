context("PlotCbmFit tests")
library(RJafroc)

test_that("PlotCbmFit should error on unequal lengths of input", {
expect_error( PlotCbmFit(c(1, 2), c(0.5)),"The lengths of mu and alpha do not match." )
})

test_that("PlotCbmFit should return a class that is gg and ggplot", {
cbmPlot <- PlotCbmFit(c(1, 2), c(0.5, 0.5))
expect_is( cbmPlot,c("gg","ggplot") )
})