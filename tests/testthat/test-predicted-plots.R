contextStr <- "PlotBinormalFit, PlotCbmFit, PlotRsmFit"
context(contextStr)
test_that(contextStr, {

  fn <- paste0(test_path(), "/goodValues361/Plots/Binormal", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotBinormalFit(c(1, 2), c(0.5, 0.5))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotBinormalFit(c(1, 2), c(0.5, 0.5)), ret, check.environment = FALSE)
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
  expect_equal(PlotCbmFit(c(1, 2), c(0.5, 0.5)), ret, check.environment = FALSE)
  # end of test
  
})




test_that("Rsm1", {
  Sys.sleep(0.2)
  
  mu <- c(2, 3)
  lambda  <- c(1, 1.5) 
  nu <- c(0.6, 0.8) 
  zeta1 <- c(-3,-3) 
  
  lesDistr <- c(0.2, 0.4, 0.1, 0.3)
  relWeights <- c(0.3, 0.4, 0.2,  0.1)
  
  fn <- paste0(test_path(), "/goodValues361/Plots/Rsm1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotRsmOperatingCharacteristics(mu = mu, 
                                           lambda = lambda, 
                                           nu = nu, 
                                           zeta1 = zeta1, 
                                           OpChType = "wAFROC",
                                           lesDistr = lesDistr, 
                                           relWeights = relWeights)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotRsmOperatingCharacteristics(mu = mu, 
                                               lambda = lambda, 
                                               nu = nu, 
                                               zeta1 = zeta1, 
                                               OpChType = "wAFROC",
                                               lesDistr = lesDistr, 
                                               relWeights = relWeights), ret, check.environment = FALSE)

})


test_that("Rsm2", {
  Sys.sleep(0.2)
  
  set.seed(1)
  K2 <- 700;Lmax <- 5;Lk2 <- floor(runif(K2, 1, Lmax + 1))
  nLesPerCase <- unique(Lk2);lesDistr <- array(dim = length(nLesPerCase))
  for (i in nLesPerCase) lesDistr[i] <- sum(Lk2 == i)/K2
  
  maxLL <- max(lesDistr)

  mu <- 10;nu <- 0.99;lambda <- 1
  
  fn <- paste0(test_path(), "/goodValues361/Plots/Rsm2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotRsmOperatingCharacteristics(mu, lambda, nu, OpChType = "wAFROC", 
                                            lesDistr = lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotRsmOperatingCharacteristics(mu, lambda, nu, OpChType = "wAFROC", 
                                               lesDistr = lesDistr), ret, check.environment = FALSE)
  
})


test_that("RSM3", {
  Sys.sleep(0.2)
  # this is the small data file created by AK that gave me so much grief! Resolved two bugs; 
  # 1: abnormal cases first
  # 2. weights matrix was not very usefull
  frocCrAbnormalCasesFirst <- system.file("extdata", "toyFiles/FROC/frocCrAbnormalCasesFirst.xlsx",
                                          package = "RJafroc", mustWork = TRUE)
  x <- DfReadDataFile(frocCrAbnormalCasesFirst, newExcelFileFormat = TRUE)
  lesDistr <- UtilLesionDistr(x)[,2]

  fn <- paste0(test_path(), "/goodValues361/Plots/Rsm3", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    
    ret <- PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8),  zeta1 = c(-3,-3), OpChType = "wAFROC",
                                           lesDistr = lesDistr, legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
  expect_equal(PlotRsmOperatingCharacteristics(mu = c(2, 3), lambda = c(1, 1.5), nu = c(0.6, 0.8), zeta1 = c(-3,-3), OpChType = "wAFROC",
                                               lesDistr = lesDistr, legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1)), ret, check.environment = FALSE)
  
})

