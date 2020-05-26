context("Compare DBMH to ORH")

test_that("Compare DBMH to ORH for dataset02, ROC", {

  ds <- dataset02

  dbmValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "DBMH")
  orValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "ORH")

  #######################################  fomArray  ###########################################
  dbmfomArray <- dbmValues$fomArray
  orfomArray <- orValues$fomArray
  for (i in 1: length(dbmfomArray)){
    x <- as.numeric(dbmfomArray[[i]])
    y <- as.numeric(orfomArray[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  #######################################  FStats ###########################################
  dbmFStatsRRRC <- as.matrix(dbmValues$RRRC$FTests)[1,4] # check p values only
  orFStatsRRRC <- as.matrix(orValues$RRRC$FTests)[1,4]
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
      x <- as.numeric(dbmFStatsRRRC[[i]])
      y <- as.numeric(orFStatsRRRC[[i]])
      expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsFRRC <- as.matrix(dbmValues$FRRC$FTests)[1,4] # check p values only
  orFStatsFRRC <- as.matrix(orValues$FRRC$FTests)[1,4]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # not exact match
  }

  dbmFStatsRRFC <- as.matrix(dbmValues$RRFC$FTests)[1,4]# check p values only
  orFStatsRRFC <- as.matrix(orValues$RRFC$FTests)[1,4]
  names(orFStatsRRFC) <- NULL
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  # VarComp are not expected to be equal

  ##################################  diff and avg confidence intervals  #####################
  dbmciDiffTrtRRRC <- as.vector(unlist(dbmValues$RRRC$ciDiffTrt[,-1])) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <-  as.vector(unlist(orValues$RRRC$ciDiffTrt[,-1]))
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRRC <- as.vector(unlist(dbmValues$RRRC$ciAvgRdrEachTrt[,-1]))
  orciAvgRdrEachTrtRRRC <- as.vector(unlist(orValues$RRRC$ciAvgRdrEachTrt[,-1]))
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtFRRC <- as.vector(as.matrix(dbmValues$FRRC$ciDiffTrt))[-c(3,4,5)]
  orciDiffTrtFRRC <- as.vector(as.matrix(orValues$FRRC$ciDiffTrt))[-c(3,4)]
  for (i in 1: length(dbmciDiffTrtFRRC)){
    if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.vector(as.matrix(dbmValues$FRRC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtFRRC <- as.vector(as.matrix(orValues$FRRC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    if (i == 5) next # skip infinity
    if (i == 6) next # skip infinity
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.vector(as.matrix(dbmValues$RRFC$ciDiffTrt))
  orciDiffTrtRRFC <- as.vector(as.matrix(orValues$RRFC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.vector(as.matrix(dbmValues$RRFC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtRRFC <- as.vector(as.matrix(orValues$RRFC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }


})



test_that("Compare DBMH to ORH for dataset05, FROC, HrAuc", {

  ds <- dataset05

  dbmValues <- StSignificanceTesting(ds, FOM = "HrAuc", method = "DBMH")
  orValues <- StSignificanceTesting(ds, FOM = "HrAuc", method = "ORH")

  #######################################  fomArray  ###########################################
  dbmfomArray <- as.vector(as.matrix(dbmValues$FOMs$foms))
  orfomArray <- as.vector(as.matrix(orValues$FOMs$foms))
  for (i in 1: length(dbmfomArray)){
    x <- dbmfomArray[i]
    y <- orfomArray[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  #######################################  FStats ###########################################
  dbmFStatsRRRC <- as.vector(as.matrix(dbmValues$RRRC$FTests))[-c(3,4,6,8)]
  orFStatsRRRC <- as.vector(as.matrix(orValues$RRRC$FTests))[-c(3,4,6,8)]
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsFRRC <- dbmValues$FRRC$FTests[1,4] # check p values only
  orFStatsFRRC <- orValues$FRRC$FTests[1,4]
  for (i in 1: length(dbmFStatsFRRC)){
    x <- dbmFStatsFRRC[i]
    y <- orFStatsFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }

  dbmFStatsRRFC <- dbmValues$RRFC$FTests[1,4]
  orFStatsRRFC <- orValues$RRFC$FTests[1,4]
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  # VarComp are not expected to be equal
  #######################################  VarComp ###########################################

  ##################################  diff and avg confidence intervals  #####################
  dbmciDiffTrtRRRC <- as.vector(as.matrix(dbmValues$RRRC$ciDiffTrt))  
  orciDiffTrtRRRC <- as.vector(as.matrix(orValues$RRRC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRRC <- as.vector(as.matrix(dbmValues$RRRC$ciAvgRdrEachTrt)) 
  orciAvgRdrEachTrtRRRC <- as.vector(as.matrix(orValues$RRRC$ciAvgRdrEachTrt))[-(11:12)] # remove var comp
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtFRRC <- as.vector(as.matrix(dbmValues$FRRC$ciDiffTrt))[-3] # remove DF 0-1 vs trt0-trt1
  orciDiffTrtFRRC <- as.vector(as.matrix(orValues$FRRC$ciDiffTrt))#[-1])
  for (i in 1: length(dbmciDiffTrtFRRC)){
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.vector(as.matrix(dbmValues$FRRC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtFRRC <- as.vector(as.matrix(orValues$FRRC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.vector(as.matrix(dbmValues$RRFC$ciDiffTrt))
  orciDiffTrtRRFC <- as.vector(as.matrix(orValues$RRFC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.vector(as.matrix(dbmValues$RRFC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtRRFC <- as.vector(as.matrix(orValues$RRFC$ciAvgRdrEachTrt))
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }


})



test_that("Compare DBMH to ORH for dataset05, FROC, wAFROC", {
  
  ds <- dataset05
  
  dbmValues <- StSignificanceTesting(ds, FOM = "wAFROC", method = "DBMH")
  orValues <- StSignificanceTesting(ds, FOM = "wAFROC", method = "ORH")
  
  #######################################  fomArray  ###########################################  
  dbmfomArray <- dbmValues$fomArray
  orfomArray <- orValues$fomArray
  for (i in 1: length(dbmfomArray)){
    x <- as.numeric(dbmfomArray[[i]])
    y <- as.numeric(orfomArray[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  #######################################  FStats ###########################################  
  dbmFStatsRRRC <- as.vector(as.matrix(dbmValues$RRRC$FTests))[-c(3,4,6,8)]
  orFStatsRRRC <- as.vector(as.matrix(orValues$RRRC$FTests))[-c(3,4,6,8)]
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmFStatsFRRC <- dbmValues$FRRC$FTests[1,4] # only check p-values
  orFStatsFRRC <- orValues$FRRC$FTests[1,4]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }
  
  dbmFStatsRRFC <- dbmValues$RRFC$FTests[1,4] # only check p-values
  orFStatsRRFC <- orValues$RRFC$FTests[1,4]
  names(orFStatsRRFC) <- NULL
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  # VarComp are not expected to be equal

  ##################################  diff and avg confidence intervals  #####################
  dbmciDiffTrtRRRC <- as.vector(as.matrix(dbmValues$RRRC$ciDiffTrt)) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <- as.vector(as.matrix(orValues$RRRC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciAvgRdrEachTrtRRRC <- as.vector(as.matrix(dbmValues$RRRC$ciAvgRdrEachTrt))
  orciAvgRdrEachTrtRRRC <- as.vector(as.matrix(orValues$RRRC$ciAvgRdrEachTrt))[-c(11,12)]  # remove var comp
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciDiffTrtFRRC <- as.vector(as.matrix(dbmValues$FRRC$ciDiffTrt))[-c(3,4)]#[-c(1,2,4)] # remove DF and t values
  orciDiffTrtFRRC <- as.vector(as.matrix(orValues$FRRC$ciDiffTrt))[-3] # remove z
  for (i in 1: length(dbmciDiffTrtFRRC)){
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }
  
  dbmciAvgRdrEachTrtFRRC <- as.vector(as.matrix(dbmValues$FRRC$ciAvgRdrEachTrt)) #[-c(1,2,7,8)])) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtFRRC <- as.vector(as.matrix(orValues$FRRC$ciAvgRdrEachTrt)) #[-c(1,2,7,8)]))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }
  
  dbmciDiffTrtRRFC <- as.vector(as.matrix(dbmValues$RRFC$ciDiffTrt))
  orciDiffTrtRRFC <- as.vector(as.matrix(orValues$RRFC$ciDiffTrt))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciAvgRdrEachTrtRRFC <- as.vector(as.matrix(dbmValues$RRFC$ciAvgRdrEachTrt)) #[-c(1,2)]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRFC <- as.vector(as.matrix(orValues$RRFC$ciAvgRdrEachTrt)) #[-c(1,2)])
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  
})
