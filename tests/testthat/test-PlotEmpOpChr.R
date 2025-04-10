context("PlotEmpOpChr: ROC & FROC & mod rdrs as vectors or lists")
test_that("ROC & FROC & vectors & lists", {

  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-ROC-vectors", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p1 <- PlotEmpOpChr(dataset04, trts = seq(1,5), rdrs = seq(1,4), opChType = "wAFROC")
    saveRDS(p1, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(dataset04, trts = seq(1,5), rdrs = seq(1,4), opChType = "wAFROC")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

  plotT <- list(1, 2, c(1:2), c(1:2))
  plotR <- list(2, c(2:3), c(1:3), 1)
  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-ROC-lists", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p1 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "ROC")
    saveRDS(p1, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "ROC")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-FROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p1 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "FROC")
    saveRDS(p1, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "FROC")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-AFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p1 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC")
    saveRDS(p1, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-wAFROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p1 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC")
    saveRDS(p1, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-AFROC1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p1 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC1")
    saveRDS(p1, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "AFROC1")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

  fn <- paste0(test_path(), "/goodValues361/Plots/ds04-wAFROC1", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p1 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC1")
    saveRDS(p1, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(dataset04, trts = plotT, rdrs = plotR, opChType = "wAFROC1")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

})




context("PlotEmpOpChr: LROC")
test_that("PlotOperatingCharacteristics-LROC", {
  
  K1 <- 10;K2 <- 10;mu <- 1;lambda <- 1;nu <- 0.8;zeta1 <- -3;perCase <- rep(1, K2)
  frocData <- SimulateFrocDataset(mu, lambda, nu, zeta1, I = 2, J = 5, K1, K2, perCase, seed = 5)
  lrocData <- DfFroc2Lroc(frocData)
  
  ###### lrocData ROC plot default arguments #####################
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-ROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpOpChr(lrocData, opChType = "ROC" )
    saveRDS(p, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(lrocData, opChType = "ROC" )
  expect_equal(p2$Points, p1$Points,check.environment = FALSE)

  ###### lrocData ROC plot vector arguments #####################
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-ROC-vectors", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpOpChr(lrocData, trts = c(1,2), rdrs = seq(1,5),  opChType = "ROC")
    saveRDS(p, file = fn)
  }
  p1 <- readRDS(fn)
  p2<- PlotEmpOpChr(lrocData, trts = c(1,2), rdrs = seq(1,5),  opChType = "ROC")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)
  
  ###### lrocData ROC plot list arguments #####################
  plotT <- list(1, 2)
  plotR <- list(seq(1,5), seq(1,5)) # 5 readers
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-ROC-lists", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpOpChr(lrocData, trts = plotT, rdrs = plotR,  opChType = "ROC")
    saveRDS(p, file = fn)
  }
  p1 <- readRDS(fn)
  p2 <- PlotEmpOpChr(lrocData, trts = plotT, rdrs = plotR,  opChType = "ROC")
  # expect_is(p2$Plot, "ggplot")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)
  
  ###### lrocData LROC plot default arguments #####################
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-LROC", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpOpChr(lrocData, opChType = "LROC" )
    saveRDS(p, file = fn)
  }
  p1 <- readRDS(fn)
  p2<-PlotEmpOpChr(lrocData, opChType = "LROC" )
  # expect_is(p2$Plot, "ggplot")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

  ###### lrocData LROC plot vector arguments #####################
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-LROC-vectors", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpOpChr(lrocData, trts = c(1,2), rdrs = seq(1,5),  opChType = "LROC")
    saveRDS(p, file = fn)
  }
  p1 <- readRDS(fn)
  p2<- PlotEmpOpChr(lrocData, trts = c(1,2), rdrs = seq(1,5),  opChType = "LROC")
  # expect_is(p2$Plot, "ggplot")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)
  
  ###### lrocData LROC plot list arguments #####################
  fn <- paste0(test_path(), "/goodValues361/Plots/lrocData-LROC-lists", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    p <- PlotEmpOpChr(lrocData, trts = plotT, rdrs = plotR,  opChType = "LROC")
    saveRDS(p, file = fn)
  }
  p1 <- readRDS(fn)
  p2<-PlotEmpOpChr(lrocData, trts = plotT, rdrs = plotR,  opChType = "LROC")
  # expect_is(p2$Plot, "ggplot")
  expect_equal(p2$Points, p1$Points, check.environment = FALSE)

})