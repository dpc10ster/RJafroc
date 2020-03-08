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
  dbmFStatsRRRC <- dbmValues$FTestStatsRRRC
  orFStatsRRRC <- orValues$FTestStatsRRRC
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
      x <- as.numeric(dbmFStatsRRRC[[i]])
      y <- as.numeric(orFStatsRRRC[[i]])
      expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsFRRC <- dbmValues$FTestStatsFRRC[-3] # remove infinity
  orFStatsFRRC <- orValues$FTestStatsFRRC[-3]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }

  dbmFStatsRRFC <- dbmValues$FTestStats
  orFStatsRRFC <- orValues$FTestStats
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
  dbmciDiffTrtRRRC <- as.vector(unlist(dbmValues$ciDiffTrtRRRC[,-1])) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <-  as.vector(unlist(orValues$ciDiffTrtRRRC[,-1]))
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRRC <- as.vector(unlist(dbmValues$ciAvgRdrEachTrtRRRC[,-1]))
  orciAvgRdrEachTrtRRRC <- as.vector(unlist(orValues$ciAvgRdrEachTrtRRRC[,-1]))
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtFRRC <- as.vector(unlist(dbmValues$ciDiffTrtFRRC[,-1]))
  orciDiffTrtFRRC <- as.vector(unlist(orValues$ciDiffTrtFRRC[,-1]))
  for (i in 1: length(dbmciDiffTrtFRRC)){
    if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.vector(unlist(dbmValues$ciAvgRdrEachTrtFRRC[,-1]))
  orciAvgRdrEachTrtFRRC <- as.vector(unlist(orValues$ciAvgRdrEachTrtFRRC[,-1]))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    if (i == 5) next # skip infinity
    if (i == 6) next # skip infinity
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.vector(unlist(dbmValues$ciDiffTrtRRFC[,-1]))
  orciDiffTrtRRFC <- as.vector(unlist(orValues$ciDiffTrtRRFC[,-1]))
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.vector(unlist(dbmValues$ciAvgRdrEachTrtRRFC[,-1]))
  orciAvgRdrEachTrtRRFC <- as.vector(unlist(orValues$ciAvgRdrEachTrtRRFC[,-1]))
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
  dbmFStatsRRRC <- dbmValues$FTestStatsRRRC
  orFStatsRRRC <- orValues$FTestStatsRRRC
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmFStatsFRRC <- dbmValues$FTestStatsFRRC[-3] # remove infinity
  orFStatsFRRC <- orValues$FTestStatsFRRC[-3]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }

  dbmFStatsRRFC <- dbmValues$FTestStats
  orFStatsRRFC <- orValues$FTestStats
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
  dbmciDiffTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$ciDiffTrtRRRC))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <- as.numeric(as.vector(unlist(orValues$ciDiffTrtRRRC))[-1])
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$ciAvgRdrEachTrtRRRC))[-1]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(orValues$ciAvgRdrEachTrtRRRC))[-1])
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciDiffTrtFRRC <- as.numeric(as.vector(unlist(dbmValues$ciDiffTrtFRRC))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtFRRC <- as.numeric(as.vector(unlist(orValues$ciDiffTrtFRRC))[-1])
  for (i in 1: length(dbmciDiffTrtFRRC)){
    if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }

  dbmciAvgRdrEachTrtFRRC <- as.numeric((as.vector(unlist(dbmValues$ciAvgRdrEachTrtFRRC))[-1])) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtFRRC <- as.numeric((as.vector(unlist(orValues$ciAvgRdrEachTrtFRRC))[-1]))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    if (i == 6) next # skip infinity
    if (i == 7) next # skip infinity
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }

  dbmciDiffTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$ciDiffTrtRRFC))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRFC <- as.numeric(as.vector(unlist(orValues$ciDiffTrtRRFC))[-1])
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }

  dbmciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$ciAvgRdrEachTrtRRFC))[-1]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(orValues$ciAvgRdrEachTrtRRFC))[-1])
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
  dbmFStatsRRRC <- dbmValues$FTestStatsRRRC
  orFStatsRRRC <- orValues$FTestStatsRRRC
  names(orFStatsRRRC) <- NULL
  for (i in 1: length(dbmFStatsRRRC)){
    x <- as.numeric(dbmFStatsRRRC[[i]])
    y <- as.numeric(orFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmFStatsFRRC <- dbmValues$FTestStatsFRRC[-3] # remove infinity
  orFStatsFRRC <- orValues$FTestStatsFRRC[-3]
  names(orFStatsFRRC) <- NULL
  for (i in 1: length(dbmFStatsFRRC)){
    x <- as.numeric(dbmFStatsFRRC[[i]])
    y <- as.numeric(orFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.01, scale = abs(x))
  }
  
  dbmFStatsRRFC <- dbmValues$FTestStats
  orFStatsRRFC <- orValues$FTestStats
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
  dbmciDiffTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$ciDiffTrtRRRC))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRRC <- as.numeric(as.vector(unlist(orValues$ciDiffTrtRRRC))[-1])
  for (i in 1: length(dbmciDiffTrtRRRC)){
    x <- dbmciDiffTrtRRRC[i]
    y <- orciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(dbmValues$ciAvgRdrEachTrtRRRC))[-1]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRRC <- as.numeric(as.vector(unlist(orValues$ciAvgRdrEachTrtRRRC))[-1])
  for (i in 1: length(dbmciAvgRdrEachTrtRRRC)){
    x <- dbmciAvgRdrEachTrtRRRC[i]
    y <- orciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciDiffTrtFRRC <- as.numeric(as.vector(unlist(dbmValues$ciDiffTrtFRRC))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtFRRC <- as.numeric(as.vector(unlist(orValues$ciDiffTrtFRRC))[-1])
  for (i in 1: length(dbmciDiffTrtFRRC)){
    if (i == 3) next # skip infinity
    x <- dbmciDiffTrtFRRC[i]
    y <- orciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.01, scale = abs(x)) # values are not exactly equal; tolerance found by trial and error
  }
  
  dbmciAvgRdrEachTrtFRRC <- as.numeric((as.vector(unlist(dbmValues$ciAvgRdrEachTrtFRRC))[-1])) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtFRRC <- as.numeric((as.vector(unlist(orValues$ciAvgRdrEachTrtFRRC))[-1]))
  for (i in 1: length(dbmciAvgRdrEachTrtFRRC)){
    if (i == 6) next # skip infinity
    if (i == 7) next # skip infinity
    x <- dbmciAvgRdrEachTrtFRRC[i]
    y <- orciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.001, scale = abs(x))  # values are not exactly equal
  }
  
  dbmciDiffTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$ciDiffTrtRRFC))[-1]) # remove 0-1 vs trt0-trt1
  orciDiffTrtRRFC <- as.numeric(as.vector(unlist(orValues$ciDiffTrtRRFC))[-1])
  for (i in 1: length(dbmciDiffTrtRRFC)){
    x <- dbmciDiffTrtRRFC[i]
    y <- orciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  dbmciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(dbmValues$ciAvgRdrEachTrtRRFC))[-1]) # remove 0-1 vs trt0-trt1
  orciAvgRdrEachTrtRRFC <- as.numeric(as.vector(unlist(orValues$ciAvgRdrEachTrtRRFC))[-1])
  for (i in 1: length(dbmciAvgRdrEachTrtRRFC)){
    x <- dbmciAvgRdrEachTrtRRFC[i]
    y <- orciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  
})
