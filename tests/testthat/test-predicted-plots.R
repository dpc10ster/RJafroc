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
# expect_equal(PlotBinormalFit(c(1, 2), c(0.5, 0.5)), ret, check.environment = FALSE)
  t <- PlotBinormalFit(c(1, 2), c(0.5, 0.5))
  expect_is(t, "ggplot")
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
# expect_equal(PlotCbmFit(c(1, 2), c(0.5, 0.5)), ret, check.environment = FALSE)
  t <- PlotCbmFit(c(1, 2), c(0.5, 0.5))
  expect_is(t, "ggplot")
  # end of test
  
})



UndoSearchParameterChange <- function (mu, lambda_i, nu_i)
{
  lambda <- lambda_i * 0
  nu <- nu_i * 0
  for (i in 1:length(lambda)) {
    x <- UtilIntrinsic2RSM(mu[i], lambda_i[i], nu_i[i])
    lambda[i] <- x$lambda
    nu[i] <- x$nu
  }
  return (list(
    lambda = lambda,
    nu = nu
  ))
}


test_that("Rsm1", {
  Sys.sleep(0.2)
  
  mu <- c(2, 3)
  lambda_i  <- c(1, 1.5) 
  nu_i <- c(0.6, 0.8) 
  zeta1 <- c(-3,-3) 
  
  x <- UndoSearchParameterChange(mu, lambda_i, nu_i)
  lambda <- x$lambda
  nu <- x$nu
  
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
# expect_equal(PlotRsmOperatingCharacteristics(mu = mu, 
#                                                lambda = lambda, 
#                                                nu = nu, 
#                                                zeta1 = zeta1, 
#                                                OpChType = "wAFROC",
#                                                lesDistr = lesDistr, 
#                                                relWeights = relWeights), ret, check.environment = FALSE)
  t <- PlotRsmOperatingCharacteristics(mu = mu, 
                                               lambda = lambda, 
                                               nu = nu, 
                                               zeta1 = zeta1, 
                                               OpChType = "wAFROC",
                                               lesDistr = lesDistr, 
                                               relWeights = relWeights)
  expect_is(t$wAFROCPlot, "ggplot")
  # end of test
  
})


test_that("Rsm2", {
  Sys.sleep(0.2)
  
  set.seed(1)
  K2 <- 700;Lmax <- 5;Lk2 <- floor(runif(K2, 1, Lmax + 1))
  nLesPerCase <- unique(Lk2);lesDistr <- array(dim = length(nLesPerCase))
  for (i in nLesPerCase) lesDistr[i] <- sum(Lk2 == i)/K2
  
  maxLL <- max(lesDistr)
  
  mu <- 10;nu_i <- 0.99;lambda_i <- 1
  
  x <- UndoSearchParameterChange(mu, lambda_i, nu_i)
  lambda <- x$lambda
  nu <- x$nu
  
  fn <- paste0(test_path(), "/goodValues361/Plots/Rsm2", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- PlotRsmOperatingCharacteristics(mu, lambda, nu, OpChType = "wAFROC", 
                                           lesDistr = lesDistr)
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
# expect_equal(PlotRsmOperatingCharacteristics(mu, lambda, nu, OpChType = "wAFROC", 
#                                              lesDistr = lesDistr), ret, check.environment = FALSE)
  t <- PlotRsmOperatingCharacteristics(mu, lambda, nu, OpChType = "wAFROC", 
                                               lesDistr = lesDistr)
  expect_is(t$wAFROCPlot, "ggplot")
  # end of test
  
})


test_that("RSM3", {
  Sys.sleep(0.2)
  # this is the small data file created by AK that gave me so much grief! Resolved two bugs; 
  # 1: abnormal cases first
  # 2. weights matrix was not very useful
  frocCrAbnormalCasesFirst <- system.file("extdata", "toyFiles/FROC/frocCrAbnormalCasesFirst.xlsx",
                                          package = "RJafroc", mustWork = TRUE)
  x <- DfReadDataFile(frocCrAbnormalCasesFirst, newExcelFileFormat = TRUE)
  lesDistr <- UtilLesionDistrVector(x)
  
  mu <- c(2, 3) 
  lambda_i <- c(1, 1.5)
  nu_i <- c(0.6, 0.8) 
  zeta1  <- c(-3,-3)
  
  x <- UndoSearchParameterChange(mu, lambda_i, nu_i)
  lambda <- x$lambda
  nu <- x$nu
  
  fn <- paste0(test_path(), "/goodValues361/Plots/Rsm3", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    
    ret <- PlotRsmOperatingCharacteristics(mu = mu, lambda = lambda, nu = nu, zeta1 = zeta1, OpChType = "wAFROC", lesDistr = lesDistr, legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))
    saveRDS(ret, file = fn)
  }
  
  ret <- readRDS(fn)
# expect_equal(PlotRsmOperatingCharacteristics(mu = mu, lambda = lambda, nu = nu,  zeta1 = zeta1, OpChType = "wAFROC", lesDistr = lesDistr, legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1)), ret, check.environment = FALSE)
  t <- PlotRsmOperatingCharacteristics(mu = mu, lambda = lambda, nu = nu,  zeta1 = zeta1, OpChType = "wAFROC", lesDistr = lesDistr, legendPosition = "bottom", nlfRange = c(0, 1), llfRange = c(0, 1))
  expect_is(t$wAFROCPlot, "ggplot")

})

