contextStr <- "XModality compare DBM to OR, FROC"
context(contextStr)
test_that(contextStr, {
  
  ds <- datasetX
  DBM <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "ALL")
  OR <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "ALL")
  
  expect_equal(DBM$FOMs, OR$FOMs)
  
  for (i in 1:2) {  
    expect_equal(DBM$RRRC$FTests[[i]]["Treatment", "FStat"], OR$RRRC$FTests[[i]]["Treatment", "FStat"])
    expect_equal(DBM$RRRC$FTests[[i]]["Treatment", "p"], OR$RRRC$FTests[[i]]["Treatment", "p"])
    expect_equal(DBM$RRRC$ciDiffTrt[[i]], OR$RRRC$ciDiffTrt[[i]])
    expect_equal(DBM$RRRC$ciAvgRdrEachTrt[[i]], OR$RRRC$ciAvgRdrEachTrt[[i]][,-6])
  }

  
  # the variance components will not match as one is PV based and the other
  # is FOM based; the first variance compenent has to be removed for this
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
    ret <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "ALL")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "ALL")
  expect_equal(current, target)
  
  ds <- dataset01
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/OR/dataset01", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "ALL")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "ALL")
  expect_equal(current, target)
  
})



contextStr <- "XModality and dataset01 DBM consistency test, FROC"
context(contextStr)
test_that(contextStr, {
  
  ds <- datasetX
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/DBM/XModality", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "ALL")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "ALL")
  expect_equal(current, target)
  
  ds <- dataset01
  
  fn <- paste0(test_path(), "/goodValues361/SigTest/DBM/dataset01", ".rds")
  if (!file.exists(fn)) {
    warning(paste0("File not found - generating new ",fn))
    ret <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "ALL")
    saveRDS(ret, file = fn)
  }
  
  target <- readRDS(fn)
  current <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "ALL")
  expect_equal(current, target)
  
})
