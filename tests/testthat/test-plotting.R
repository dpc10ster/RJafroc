context("Plotting routines")

test_that("PlotBinormalFit", {
  
  fn <- paste0(test_path(), "/goodValues/Plots/Binormal")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotBinormalFit(c(1, 2), c(0.5, 0.5))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotBinormalFit(c(1, 2), c(0.5, 0.5)), ret)
  # end of test
  
})

test_that("PlotCbmFit", {
  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues/Plots/Cbm")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotCbmFit(c(1, 2), c(0.5, 0.5))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotCbmFit(c(1, 2), c(0.5, 0.5)), ret)
  # end of test
  
})

test_that("PlotRsmOperatingCharacteristics", {
  Sys.sleep(0.2)
  
  lesDistr <- rbind(c(1, 0.2), c(2, 0.4), c(3, 0.1), c(4, 0.3))
  lesionWeights <- rbind(c(1.0, -Inf, -Inf, -Inf), 
                         c(0.4,  0.6, -Inf, -Inf), 
                         c(0.2,  0.3,  0.5, -Inf), 
                         c(0.3,  0.4, 0.2,  0.1))
  
  fn <- paste0(test_path(), "/goodValues/Plots/Rsm")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
                                           lesDistr = lesDistr, lesionWeights = lesionWeights, 
                                           legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
                                               lesDistr = lesDistr, lesionWeights = lesionWeights, 
                                               legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1)), ret)
  # end of test
  
})

test_that("PlotOperatingCharacteristics", {
  Sys.sleep(0.2)
  
  plotT <- list(1, 2, c(1:2), c(1:2))
  plotR <- list(2, c(2:3), c(1:3), 1)
  
  fn <- paste0(test_path(), "/goodValues/Plots/ds04-ROC")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR) # ROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR), ret)
  # end of test
  
  fn <- paste0(test_path(), "/goodValues/Plots/ds04-FROC")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "FROC") # FROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "FROC"), ret)
  # end of test
  
  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues/Plots/ds04-AFROC")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC") # AFROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC"), ret)
  # end of test
  
  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues/Plots/ds04-wAFROC")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC") # AFROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC"), ret)
  # end of test
  
  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues/Plots/ds04-AFROC1")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC1") # AFROC1
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC1"), ret)
  # end of test
  
  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues/Plots/ds04-wAFROC1")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC1") # wAFROC1
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC1"), ret)
  # end of test
  
})