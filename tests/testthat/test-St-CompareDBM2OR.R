contextStr <- "Compare DBM to OR for dataset02, ROC"
context(contextStr)
test_that(contextStr, {

  ds <- dataset02

  ############################  RRRC
  DBM_RRRC <- St(ds, FOM = "Wilcoxon", method = "DBM", analysisOption = "RRRC")
  OR_RRRC <- St(ds, FOM = "Wilcoxon", method = "OR", analysisOption = "RRRC")

  dbmfomArray <- DBM_RRRC$fomArray
  orfomArray <- OR_RRRC$fomArray
  for (i in 1: length(dbmfomArray)){
    x <- as.numeric(dbmfomArray[[i]])
    y <- as.numeric(orfomArray[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsRRRC <- as.matrix(DBM_RRRC$RRRC$FTests)[1,4] # check p values only
  orFStatsRRRC <- as.matrix(OR_RRRC$RRRC$FTests)[1,4]
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
      x <- as.numeric(dbmFStatsRRRC[[i]])
      y <- as.numeric(orFStatsRRRC[[i]])
      expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtRRRC <- as.vector(unlist(DBM_RRRC$RRRC$ciDiffTrt[,-1])) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <-  as.vector(unlist(OR_RRRC$RRRC$ciDiffTrt[,-1]))
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciAvgRdrEachTrtRRRC <- as.vector(unlist(DBM_RRRC$RRRC$ciAvgRdrEachTrt[,-1]))
  orciAvgRdrEachTrtRRRC <- as.vector(unlist(OR_RRRC$RRRC$ciAvgRdrEachTrt[,-1]))
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  ############################  FRRC
  DBM_FRRC <- St(ds, FOM = "Wilcoxon", method = "DBM", analysisOption = "FRRC")
  OR_FRRC <- St(ds, FOM = "Wilcoxon", method = "OR", analysisOption = "FRRC")
  
  dbmFStatsFRRC <- as.matrix(DBM_FRRC$FRRC$FTests)[1,4] # check p values only
  orFStatsFRRC <- as.matrix(OR_FRRC$FRRC$FTests)[1,4]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # not exact match
  }
  
  dbmciDiffTrtFRRC <- as.vector(as.matrix(DBM_FRRC$FRRC$ciDiffTrt))[-c(3,4,5)]
  orciDiffTrtFRRC <- as.vector(as.matrix(OR_FRRC$FRRC$ciDiffTrt))[-c(3,4)]
  for (i in 1: length(dbmciDiffTrtFRRC)){
    if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }
  
  dbmciAvgRdrEachTrtFRRC <- as.vector(as.matrix(DBM_FRRC$FRRC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtFRRC <- as.vector(as.matrix(OR_FRRC$FRRC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    if (i == 5) next # skip infinity
    if (i == 6) next # skip infinity
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }
  
  ############################  RRFC
  DBM_RRFC <- St(ds, FOM = "Wilcoxon", method = "DBM", analysisOption = "RRFC")
  OR_RRFC <- St(ds, FOM = "Wilcoxon", method = "OR", analysisOption = "RRFC")
  
  dbmFStatsRRFC <- as.matrix(DBM_RRFC$RRFC$FTests)[1,4]# check p values only
  orFStatsRRFC <- as.matrix(OR_RRFC$RRFC$FTests)[1,4]
  names(orFStatsRRFC) <- NULL
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  # VarComp are not expected to be equal

  dbmciDiffTrtRRFC <- as.vector(as.matrix(DBM_RRFC$RRFC$ciDiffTrt))
  orciDiffTrtRRFC <- as.vector(as.matrix(OR_RRFC$RRFC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.vector(as.matrix(DBM_RRFC$RRFC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtRRFC <- as.vector(as.matrix(OR_RRFC$RRFC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }


})



contextStr <- "Compare DBM to OR for dataset05, FROC, HrAuc"
context(contextStr)
test_that(contextStr, {

  ds <- dataset05

  DBM_RRRC <- St(ds, FOM = "HrAuc", method = "DBM", analysisOption = "RRRC")
  OR_RRRC <- St(ds, FOM = "HrAuc", method = "OR", analysisOption = "RRRC")

  #######################################  fomArray  ###########################################
  dbmfomArray <- as.vector(as.matrix(DBM_RRRC$FOMs$foms))
  orfomArray <- as.vector(as.matrix(OR_RRRC$FOMs$foms))
  for (i in 1: length(dbmfomArray)){
    x <- dbmfomArray[i]
    y <- orfomArray[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  #######################################  FStats ###########################################
  dbmFStatsRRRC <- as.vector(as.matrix(DBM_RRRC$RRRC$FTests))[-c(3,4,6,8)]
  orFStatsRRRC <- as.vector(as.matrix(OR_RRRC$RRRC$FTests))[-c(3,4,6,8)]
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  DBM_FRRC <- St(ds, FOM = "HrAuc", method = "DBM", analysisOption = "FRRC")
  OR_FRRC <- St(ds, FOM = "HrAuc", method = "OR", analysisOption = "FRRC")
  
  dbmFStatsFRRC <- DBM_FRRC$FRRC$FTests[1,4] # check p values only
  orFStatsFRRC <- OR_FRRC$FRRC$FTests[1,4]
  for (i in 1: length(dbmFStatsFRRC)){
    x <- dbmFStatsFRRC[i]
    y <- orFStatsFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }

  DBM_RRFC <- St(ds, FOM = "HrAuc", method = "DBM", analysisOption = "RRFC")
  OR_RRFC <- St(ds, FOM = "HrAuc", method = "OR", analysisOption = "RRFC")
  
  dbmFStatsRRFC <- DBM_RRFC$RRFC$FTests[1,4]
  orFStatsRRFC <- OR_RRFC$RRFC$FTests[1,4]
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  # VarComp are not expected to be equal
  #######################################  VarComp ###########################################

  ##################################  diff and avg confidence intervals  #####################
  dbmciDiffTrtRRRC <- as.vector(as.matrix(DBM_RRRC$RRRC$ciDiffTrt))  
  orciDiffTrtRRRC <- as.vector(as.matrix(OR_RRRC$RRRC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRRC <- as.vector(as.matrix(DBM_RRRC$RRRC$ciAvgRdrEachTrt)) 
  orciAvgRdrEachTrtRRRC <- as.vector(as.matrix(OR_RRRC$RRRC$ciAvgRdrEachTrt))[-(11:12)] # remove var comp
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtFRRC <- as.vector(as.matrix(DBM_FRRC$FRRC$ciDiffTrt))[-3] # remove DF 0-1 vs trt0-trt1
  orciDiffTrtFRRC <- as.vector(as.matrix(OR_FRRC$FRRC$ciDiffTrt))#[-1])
  for (i in 1: length(dbmciDiffTrtFRRC)){
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.vector(as.matrix(DBM_FRRC$FRRC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtFRRC <- as.vector(as.matrix(OR_FRRC$FRRC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.vector(as.matrix(DBM_RRFC$RRFC$ciDiffTrt))
  orciDiffTrtRRFC <- as.vector(as.matrix(OR_RRFC$RRFC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.vector(as.matrix(DBM_RRFC$RRFC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtRRFC <- as.vector(as.matrix(OR_RRFC$RRFC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }


})


contextStr <- "Compare DBM to OR for dataset05, FROC, wAFROC"
context(contextStr)
test_that(contextStr, {

  ds <- dataset05

  DBM_RRRC <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRRC")
  OR_RRRC <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRRC")
  DBM_FRRC <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "FRRC")
  OR_FRRC <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "FRRC")
  DBM_RRFC <- St(ds, FOM = "wAFROC", method = "DBM", analysisOption = "RRFC")
  OR_RRFC <- St(ds, FOM = "wAFROC", method = "OR", analysisOption = "RRFC")
  
  dbmfomArray <- DBM_RRRC$fomArray
  orfomArray <- OR_RRRC$fomArray
  for (i in 1: length(dbmfomArray)){
    x <- as.numeric(dbmfomArray[[i]])
    y <- as.numeric(orfomArray[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsRRRC <- as.vector(as.matrix(DBM_RRRC$RRRC$FTests))[-c(3,4,6,8)]
  orFStatsRRRC <- as.vector(as.matrix(OR_RRRC$RRRC$FTests))[-c(3,4,6,8)]
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsFRRC <- DBM_FRRC$FRRC$FTests[1,4] # only check p-values
  orFStatsFRRC <- OR_FRRC$FRRC$FTests[1,4]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }

  dbmFStatsRRFC <- DBM_RRFC$RRFC$FTests[1,4] # only check p-values
  orFStatsRRFC <- OR_RRFC$RRFC$FTests[1,4]
  names(orFStatsRRFC) <- NULL
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  # VarComp are not expected to be equal

  dbmciDiffTrtRRRC <- as.vector(as.matrix(DBM_RRRC$RRRC$ciDiffTrt)) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <- as.vector(as.matrix(OR_RRRC$RRRC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRRC <- as.vector(as.matrix(DBM_RRRC$RRRC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtRRRC <- as.vector(as.matrix(OR_RRRC$RRRC$ciAvgRdrEachTrt))[-c(11,12)]  # remove var comp
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtFRRC <- as.vector(as.matrix(DBM_FRRC$FRRC$ciDiffTrt))[-c(3,4)]#[-c(1,2,4)] # remove DF and t values
  orciDiffTrtFRRC <- as.vector(as.matrix(OR_FRRC$FRRC$ciDiffTrt))[-3] # remove z
  for (i in 1: length(dbmciDiffTrtFRRC)){
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.vector(as.matrix(DBM_FRRC$FRRC$ciAvgRdrEachTrt)) #[-c(1,2,7,8)])) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtFRRC <- as.vector(as.matrix(OR_FRRC$FRRC$ciAvgRdrEachTrt)) #[-c(1,2,7,8)]))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.vector(as.matrix(DBM_RRFC$RRFC$ciDiffTrt))
  orciDiffTrtRRFC <- as.vector(as.matrix(OR_RRFC$RRFC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.vector(as.matrix(DBM_RRFC$RRFC$ciAvgRdrEachTrt)) #[-c(1,2)]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRFC <- as.vector(as.matrix(OR_RRFC$RRFC$ciAvgRdrEachTrt)) #[-c(1,2)])
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

})
