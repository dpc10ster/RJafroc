context("Plotting routines")

test_that("PlotEmpiricalOperatingCharacteristics Independent", {
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset = dataset02, trts = c(1:2), rdrs = c(1:3)), 
    tmp, print = TRUE, update = TRUE)
})

test_that("PlotEmpiricalOperatingCharacteristics Avgerage", {
  Sys.sleep(0.2)
  plotT <- list(1, 2, c(1:2))
  plotR <- list(2, c(2:3), c(1:3))
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = plotT, rdrs = plotR), 
    tmp, print = TRUE, update = TRUE)
})

test_that("PlotBinormalFit", {
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotBinormalFit(c(1, 2), c(0.5, 0.5)), 
    tmp, print = TRUE, update = TRUE)
})

test_that("PlotCbmFit", {
  Sys.sleep(0.2)
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotCbmFit(c(1, 2), c(0.5, 0.5)), 
    tmp, print = TRUE, update = TRUE)
})

test_that("PlotRsmOperatingCharacteristics", {
  Sys.sleep(0.2)
  lesDistr <- rbind(c(1, 0.2), c(2, 0.4), c(3, 0.1), c(4, 0.3))
  lesionWeights <- rbind(c(1.0, -Inf, -Inf, -Inf), 
                         c(0.4,  0.6, -Inf, -Inf), 
                         c(0.2,  0.3,  0.5, -Inf), 
                         c(0.3,  0.4, 0.2,  0.1))
  tmp <- tempfile()
  expect_known_output(
    PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
                                    lesDistr = lesDistr, lesionWeights = lesionWeights, 
                                    legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1)), 
    tmp, print = TRUE, update = TRUE)
})

test_that("PlotOperatingCharacteristics", {
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset04, trts = c(1), rdrs = c(1)), 
    tmp, print = TRUE, update = TRUE)
  
  Sys.sleep(0.2)
  plotT <- list(1, 2, c(1:2))
  plotR <- list(2, c(2:3), c(1:3))
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR), 
    tmp, print = TRUE, update = TRUE)
  
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = 1, rdrs = 1, opChType = "FROC"), 
    tmp, print = TRUE, update = TRUE)
  
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = 1, rdrs = 1, opChType = "AFROC"), 
    tmp, print = TRUE, update = TRUE)
  
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = 1, rdrs = 1, opChType = "wAFROC"), 
    tmp, print = TRUE, update = TRUE)
  
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = 1, rdrs = 1, opChType = "AFROC1"), 
    tmp, print = TRUE, update = TRUE)
  
  Sys.sleep(0.2)
  tmp <- tempfile()
  expect_known_output(
    PlotEmpiricalOperatingCharacteristics(dataset = dataset04, trts = 1, rdrs = 1, opChType = "wAFROC1"), 
    tmp, print = TRUE, update = TRUE)
  
})