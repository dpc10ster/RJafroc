context("PlotBinormalFit, PlotCbmFit, PlotRsmFit")

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
    ret <- PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8), OpChType = "wAFROC",
                                           lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, 
                                           legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8), OpChType = "wAFROC",
                                               lesDistr = lesDistr, lesWghtDistr = lesWghtDistr, 
                                               legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1)), ret)

})

