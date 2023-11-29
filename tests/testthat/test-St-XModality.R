contextStr <- "XModality compare DBM to OR: FROC, RRRC, FRRC and RRFC"
context(contextStr)
test_that(contextStr, {
  
  ds <- datasetX
  ####################################  RRRC
  DBM <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRRC")
  OR <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRRC")
  
  expect_equal(DBM$FOMs, OR$FOMs)
  
  for (i in 1:2) {  
    expect_equal(DBM$RRRC$FTests[[i]]$AvgMod1["Treatment", "FStat"], OR$RRRC$FTests[[i]]$AvgMod1["Treatment", "FStat"])
    expect_equal(DBM$RRRC$FTests[[i]]$AvgMod1["Treatment", "p"], OR$RRRC$FTests[[i]]$AvgMod1["Treatment", "p"])
    expect_equal(DBM$RRRC$ciDiffTrt[[i]]$AvgMod1, OR$RRRC$ciDiffTrt[[i]]$AvgMod1)
    expect_equal(DBM$RRRC$ciAvgRdrEachTrt[[i]]$AvgMod1, OR$RRRC$ciAvgRdrEachTrt[[i]]$AvgMod1[,-6])
    expect_equal(DBM$RRRC$FTests[[i]]$AvgMod2["Treatment", "FStat"], OR$RRRC$FTests[[i]]$AvgMod2["Treatment", "FStat"])
    expect_equal(DBM$RRRC$FTests[[i]]$AvgMod2["Treatment", "p"], OR$RRRC$FTests[[i]]$AvgMod2["Treatment", "p"])
    expect_equal(DBM$RRRC$ciDiffTrt[[i]]$AvgMod2, OR$RRRC$ciDiffTrt[[i]]$AvgMod2)
    expect_equal(DBM$RRRC$ciAvgRdrEachTrt[[i]]$AvgMod2, OR$RRRC$ciAvgRdrEachTrt[[i]]$AvgMod2[,-6])
  }
  
  
  # the variance components will not match as one is PV based and the other
  # is FOM based; the first variance component has to be removed for this
  # to work
  # 
  K <- length(ds$ratings$NL[1,1,1,,1])
  for (i in 1:2) {
    ORVC1 <- UtilDBM2ORVarCom(K, DBM$ANOVA$VarCom[[i]])[,1]
    expect_equal(OR$ANOVA$VarCom[[i]][,1][-1], ORVC1[-1])
  }

  ####################################  FRRC
  DBM <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "FRRC")
  OR <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "FRRC")
  
  expect_equal(DBM$FOMs, OR$FOMs)
  
  for (i in 1:2) {  
    expect_equal(DBM$FRRC$FTests[[i]]$AvgMod1["Treatment", "FStat"], OR$FRRC$FTests[[i]]$AvgMod1["Treatment", "FStat"])
    expect_equal(DBM$FRRC$FTests[[i]]$AvgMod1["Treatment", "p"], OR$FRRC$FTests[[i]]$AvgMod1["Treatment", "p"])
    expect_equal(DBM$FRRC$ciDiffTrt[[i]]$AvgMod1, OR$FRRC$ciDiffTrt[[i]]$AvgMod1)
    expect_equal(DBM$FRRC$ciAvgRdrEachTrt[[i]]$AvgMod1, OR$FRRC$ciAvgRdrEachTrt[[i]]$AvgMod1[,-6])
    expect_equal(DBM$FRRC$FTests[[i]]$AvgMod2["Treatment", "FStat"], OR$FRRC$FTests[[i]]$AvgMod2["Treatment", "FStat"])
    expect_equal(DBM$FRRC$FTests[[i]]$AvgMod2["Treatment", "p"], OR$FRRC$FTests[[i]]$AvgMod2["Treatment", "p"])
    expect_equal(DBM$FRRC$ciDiffTrt[[i]]$AvgMod2, OR$FRRC$ciDiffTrt[[i]]$AvgMod2)
    expect_equal(DBM$FRRC$ciAvgRdrEachTrt[[i]]$AvgMod2, OR$FRRC$ciAvgRdrEachTrt[[i]]$AvgMod2[,-6])
  }
  
  
  # the variance components will not match as one is PV based and the other
  # is FOM based; the first variance component has to be removed for this
  # to work
  # 
  K <- length(ds$ratings$NL[1,1,1,,1])
  for (i in 1:2) {
    ORVC1 <- UtilDBM2ORVarCom(K, DBM$ANOVA$VarCom[[i]])[,1]
    expect_equal(OR$ANOVA$VarCom[[i]][,1][-1], ORVC1[-1])
  }
  
  ####################################  RRFC
  DBM <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRFC")
  OR <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRFC")
  
  expect_equal(DBM$FOMs, OR$FOMs)
  
  for (i in 1:2) {  
    expect_equal(DBM$RRFC$FTests[[i]]$AvgMod1["Treatment", "FStat"], OR$RRFC$FTests[[i]]$AvgMod1["Treatment", "FStat"])
    expect_equal(DBM$RRFC$FTests[[i]]$AvgMod1["Treatment", "p"], OR$RRFC$FTests[[i]]$AvgMod1["Treatment", "p"])
    expect_equal(DBM$RRFC$ciDiffTrt[[i]]$AvgMod1, OR$RRFC$ciDiffTrt[[i]]$AvgMod1)
    expect_equal(DBM$RRFC$ciAvgRdrEachTrt[[i]]$AvgMod1, OR$RRFC$ciAvgRdrEachTrt[[i]]$AvgMod1[,-6])
    expect_equal(DBM$RRFC$FTests[[i]]$AvgMod2["Treatment", "FStat"], OR$RRFC$FTests[[i]]$AvgMod2["Treatment", "FStat"])
    expect_equal(DBM$RRFC$FTests[[i]]$AvgMod2["Treatment", "p"], OR$RRFC$FTests[[i]]$AvgMod2["Treatment", "p"])
    expect_equal(DBM$RRFC$ciDiffTrt[[i]]$AvgMod2, OR$RRFC$ciDiffTrt[[i]]$AvgMod2)
    expect_equal(DBM$RRFC$ciAvgRdrEachTrt[[i]]$AvgMod2, OR$RRFC$ciAvgRdrEachTrt[[i]]$AvgMod2[,-6])
  }
  
  
  # the variance components will not match as one is PV based and the other
  # is FOM based; the first variance component has to be removed for this
  # to work
  # 
  K <- length(ds$ratings$NL[1,1,1,,1])
  for (i in 1:2) {
    ORVC1 <- UtilDBM2ORVarCom(K, DBM$ANOVA$VarCom[[i]])[,1]
    expect_equal(OR$ANOVA$VarCom[[i]][,1][-1], ORVC1[-1])
  }
})



contextStr <- "XModality and dataset01 OR consistency test, FROC"
context(contextStr)
test_that(contextStr, {
  
  ds <- datasetX
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/OR/XModality", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRRC")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRRC")
  expect_equal(current, target)
  
  ds <- dataset01
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/OR/dataset01", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRRC")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRRC")
  expect_equal(current, target)
  
})



contextStr <- "XModality and dataset01 DBM consistency test, FROC"
context(contextStr)
test_that(contextStr, {
  
  ds <- datasetX
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/DBM/XModality", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRRC")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRRC")
  expect_equal(current, target)
  
  ds <- dataset01
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/DBM/dataset01", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRRC")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRRC")
  expect_equal(current, target)
  
})
