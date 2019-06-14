context("General tests")
library(RJafroc)

test_that("dataset04 - lesion number is expected", {
expect_equal(length(dataset04$lesionNum),100)
})

test_that("Import includedFrocData.xlsx - ROI datatype", {
fileName <- system.file(
    "extdata", "includedRoiData.xlsx", package = "RJafroc", mustWork = TRUE)
ds <- DfReadDataFile(fileName)
expect_equal(ds$dataType,c("ROI"))
})

test_that("Import includedFrocData.xlsx - FROC datatype", {
fileName <- system.file(
    "extdata", "includedFrocData.xlsx", package = "RJafroc", mustWork = TRUE)
ds <- DfReadDataFile(fileName)
expect_equal(ds$dataType,c("FROC"))
})

test_that("PlotBinormalFit should be a gg and ggplot", {
  aArray <- c(0.7, 0.7, 1.5, 2)
  bArray <- c(0.5, 1.5, 0.5, 0.5)
  p <- PlotBinormalFit(aArray, bArray)
  expect_is( (p),c("gg","ggplot") )
})

test_that("CBMPoints from Ch20V2degenerateROCs",{
  CBMPoints <- function(mu, alpha){
  plotZeta <- seq(-20, 20, by = 0.01)
  FPF <- 1 - pnorm(plotZeta)
  FPF <- c(1, FPF, 0)
  TPF <- (1 - alpha) * (1 - pnorm(plotZeta)) + alpha * (1 - pnorm(plotZeta, mean = mu))
  TPF <- c(1, TPF, 0)
  plotCurve <- data.frame(FPF = FPF, TPF = TPF)
  return(plotCurve) }
  mu <- Inf; alpha <- 0.75
  plotCurve <- CBMPoints(mu, alpha)
  expect_equal(length(plotCurve$TPF),4003)
})