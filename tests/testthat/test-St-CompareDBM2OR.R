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
  dbmFStatsRRRC <- dbmValues$RRRC$FTests
  orFStatsRRRC <- orValues$RRRC$FTests
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
      x <- as.numeric(dbmFStatsRRRC[[i]])
      y <- as.numeric(orFStatsRRRC[[i]])
      expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsFRRC <- dbmValues$FRRC$FTests[-3] # remove infinity
  orFStatsFRRC <- orValues$FRRC$FTests[-3]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }

  dbmFStatsRRFC <- dbmValues$RRFC$FTests
  orFStatsRRFC <- orValues$RRFC$FTests
  names(orFStatsRRFC) <- NULL
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  # VarComp are not expected to be equal
  #######################################  VarComp ###########################################
  # dbmVarComp <- as.vector(unlist(dbmValues$varComp))
  # orVarComp <- as.vector(unlist(orValues$varComp))
  # for (i in 1: length(dbmVarComp)){
  #   x <- dbmVarComp[i]
  #   y <- orVarComp[i]
  #   expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  # }

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

  dbmciDiffTrtFRRC <- as.vector(unlist(dbmValues$FRRC$ciDiffTrt[,-1]))
  orciDiffTrtFRRC <- as.vector(unlist(orValues$FRRC$ciDiffTrt[,-1]))
  for (i in 1: length(dbmciDiffTrtFRRC)){
    if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.vector(unlist(dbmValues$FRRC$ciAvgRdrEachTrt[,-1]))
  orciAvgRdrEachTrtFRRC <- as.vector(unlist(orValues$FRRC$ciAvgRdrEachTrt[,-1]))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    if (i == 5) next # skip infinity
    if (i == 6) next # skip infinity
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.vector(unlist(dbmValues$RRFC$ciDiffTrt[,-1]))
  orciDiffTrtRRFC <- as.vector(unlist(orValues$RRFC$ciDiffTrt[,-1]))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.vector(unlist(dbmValues$RRFC$ciAvgRdrEachTrt[,-1]))
  orciAvgRdrEachTrtRRFC <- as.vector(unlist(orValues$RRFC$ciAvgRdrEachTrt[,-1]))
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
  dbmfomArray <- dbmValues$fomArray
  orfomArray <- orValues$fomArray
  for (i in 1: length(dbmfomArray)){
    x <- as.numeric(dbmfomArray[[i]])
    y <- as.numeric(orfomArray[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  #######################################  FStats ###########################################
  dbmFStatsRRRC <- dbmValues$RRRC$FTests
  orFStatsRRRC <- orValues$RRRC$FTests
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsFRRC <- dbmValues$FRRC$FTests[-3] # remove infinity
  orFStatsFRRC <- orValues$FRRC$FTests[-3]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }

  dbmFStatsRRFC <- dbmValues$RRFC$FTests
  orFStatsRRFC <- orValues$RRFC$FTests
  names(orFStatsRRFC) <- NULL
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  # VarComp are not expected to be equal
  #######################################  VarComp ###########################################
  # dbmVarComp <- as.vector(unlist(dbmValues$varComp))
  # orVarComp <- as.vector(unlist(orValues$varComp))
  # for (i in 1: length(dbmVarComp)){
  #   x <- dbmVarComp[i]
  #   y <- orVarComp[i]
  #   expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  # }

  ##################################  diff and avg confidence intervals  #####################
  dbmciDiffTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$RRRC$ciDiffTrt))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <- as.numeric(as.vector(unlist(orValues$RRRC$ciDiffTrt))[-1])
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$RRRC$ciAvgRdrEachTrt))[-(1:2)]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(orValues$RRRC$ciAvgRdrEachTrt))[-(1:2)])
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtFRRC <- as.numeric(as.vector(unlist(dbmValues$FRRC$ciDiffTrt))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtFRRC <- as.numeric(as.vector(unlist(orValues$FRRC$ciDiffTrt))[-1])
  for (i in 1: length(dbmciDiffTrtFRRC)){
    if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.numeric(as.vector(unlist(dbmValues$FRRC$ciAvgRdrEachTrt))[-c(1,2,7,8)])
  # remove 0-1 vs trt0-trt1, remove ddf and infinity as these are bound to be different
  orciAvgRdrEachTrtFRRC <- as.numeric(as.vector(unlist(orValues$FRRC$ciAvgRdrEachTrt))[-c(1,2,7,8)])
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$RRFC$ciDiffTrt))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRFC <- as.numeric(as.vector(unlist(orValues$RRFC$ciDiffTrt))[-1])
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$RRFC$ciAvgRdrEachTrt))[-c(1,2)]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(orValues$RRFC$ciAvgRdrEachTrt))[-c(1,2)])
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
  dbmFStatsRRRC <- dbmValues$RRRC$FTests
  orFStatsRRRC <- orValues$RRRC$FTests
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmFStatsFRRC <- dbmValues$FRRC$FTests[-3] # remove infinity
  orFStatsFRRC <- orValues$FRRC$FTests[-3]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }
  
  dbmFStatsRRFC <- dbmValues$RRFC$FTests
  orFStatsRRFC <- orValues$RRFC$FTests
  names(orFStatsRRFC) <- NULL
  for (i in 1: length(dbmFStatsRRFC)){
    x <- as.numeric(dbmFStatsRRFC[[i]])
    y <- as.numeric(orFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  # VarComp are not expected to be equal
  #######################################  VarComp ###########################################
  # dbmVarComp <- as.vector(unlist(dbmValues$varComp))
  # orVarComp <- as.vector(unlist(orValues$varComp))
  # for (i in 1: length(dbmVarComp)){
  #   x <- dbmVarComp[i]
  #   y <- orVarComp[i]
  #   expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  # }
  
  ##################################  diff and avg confidence intervals  #####################
  dbmciDiffTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$RRRC$ciDiffTrt))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <- as.numeric(as.vector(unlist(orValues$RRRC$ciDiffTrt))[-1])
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$RRRC$ciAvgRdrEachTrt))[-c(1,2)]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(orValues$RRRC$ciAvgRdrEachTrt))[-c(1,2)])
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciDiffTrtFRRC <- as.numeric(as.vector(unlist(dbmValues$FRRC$ciDiffTrt))[-c(1,2,4)]) # remove 0-1 vs trt0-trt1
  orciDiffTrtFRRC <- as.numeric(as.vector(unlist(orValues$FRRC$ciDiffTrt))[-c(1,2,4)])
  for (i in 1: length(dbmciDiffTrtFRRC)){
    # if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }
  
  dbmciAvgRdrEachTrtFRRC <- as.numeric((as.vector(unlist(dbmValues$FRRC$ciAvgRdrEachTrt))[-c(1,2,7,8)])) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtFRRC <- as.numeric((as.vector(unlist(orValues$FRRC$ciAvgRdrEachTrt))[-c(1,2,7,8)]))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    # if (i == 6) next # skip infinity
    # if (i == 7) next # skip infinity
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }
  
  dbmciDiffTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$RRFC$ciDiffTrt))[-c(1,4)]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRFC <- as.numeric(as.vector(unlist(orValues$RRFC$ciDiffTrt))[-c(1,4)])
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$RRFC$ciAvgRdrEachTrt))[-c(1,2)]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(orValues$RRFC$ciAvgRdrEachTrt))[-c(1,2)])
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  
})
