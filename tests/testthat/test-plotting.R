context("Plotting routines")

test_that("PlotBinormalFit", {
  
  fn <- paste0(test_path(), "/goodValues361/Plots/Binormal", ".rds")
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
  
  fn <- paste0(test_path(), "/goodValues361/Plots/Cbm", ".rds")
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
  lesWghtDistr <- rbind(c(1.0, -Inf, -Inf, -Inf), 
                         c(0.4,  0.6, -Inf, -Inf), 
                         c(0.2,  0.3,  0.5, -Inf), 
                         c(0.3,  0.4, 0.2,  0.1))
  
  fn <- paste0(test_path(), "/goodValues361/Plots/Rsm", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
                                           lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, 
                                           legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),
                                               lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, 
                                               legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1)), ret)

})




test_that("PlotOperatingCharacteristics - ROC & FROC & lists", {
  Sys.sleep(0.2)
  
  plotT <- list(1, 2, c(1:2), c(1:2))
  plotR <- list(2, c(2:3), c(1:3), 1)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-ROC-lists", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR) # ROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR), ret)

  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-wAFROC-lists", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC") # ROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC"), ret)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-FROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "FROC") # FROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "FROC"), ret)

  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-AFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC") # AFROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC"), ret)

  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC") # wAFROC
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC"), ret)

  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-AFROC1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC1") # AFROC1
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC1"), ret)

  Sys.sleep(0.2)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-wAFROC1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC1") # wAFROC1
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC1"), ret)

})  




test_that("PlotOperatingCharacteristics-LROC", {
  Sys.sleep(0.2)
  set.seed(5)
  K1 <- 10;K2 <- 10;mu <- 1;lambda <- 1;nu <- 0.8;zeta1 <- -3;lesionVector <- rep(1, K2)
  frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, lesionVector)
  lrocData <- DfFroc2Lroc(frocData)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-ROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpiricalOperatingCharacteristics(lrocData, opChType = "ROC" )
    saveRDS(p, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(lrocData, opChType = "ROC" ), ret)

  Sys.sleep(0.2)
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-LROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpiricalOperatingCharacteristics(lrocData, opChType = "LROC" )
    saveRDS(p, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(lrocData, opChType = "LROC" ), ret)
  
  plotT <- list(1, 2)
  plotR <- list(seq(1,5), seq(1,5)) # 5 readers
  
  Sys.sleep(0.2)
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-ROC-lists", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpiricalOperatingCharacteristics(lrocData, trts = plotT, rdrs = plotR,  opChType = "ROC")
    saveRDS(p, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(lrocData, trts = plotT, rdrs = plotR,  opChType = "ROC"), ret)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-LROC-lists", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpiricalOperatingCharacteristics(lrocData, trts = plotT, rdrs = plotR,  opChType = "LROC")
    saveRDS(p, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotEmpiricalOperatingCharacteristics(lrocData, trts = plotT, rdrs = plotR,  opChType = "LROC"), ret)
  
})