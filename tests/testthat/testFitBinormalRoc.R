context("FitBinomalRoc tests")
library(RJafroc)

test_that("FitBinormalRoc should error on degenerate data", {
expect_warning(FitBinormalRoc(datasetDegenerate),"Data is degenerate" )
})

ds <- DfFroc2Roc(dataset01)
retFit <- FitBinormalRoc(ds, 2, 3)

test_that("FitBinormalRoc should return a class that is gg and ggplot", {
expect_is( retFit$fittedPlot,c("gg","ggplot") )
})

test_that("FitBinormalRoc$ChisqrFitStats$chisq value using dataset01", {
expect_equal( retFit$ChisqrFitStats$chisq,15.71449806 ) # hardcoded value for chisq
})
