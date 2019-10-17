context("Compare current to original codes: DBMH")

test_that("Compare current to original code: DBMH", {
  
  ds <- dataset02

  orgValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "DBMH", tempOrgCode = TRUE)
  newValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "DBMH", tempOrgCode = FALSE)

  #######################################  FStats ###########################################  
  orgFStatsRRRC <- c(orgValues$fRRRC, orgValues$ddfRRRC, orgValues$pRRRC)
  newFStatsRRRC <- newValues$FTestStatsRRRC
  newFStatsRRRC <- unlist(newFStatsRRRC[-2])
  names(newFStatsRRRC) <- NULL
  for (i in 1: length(orgFStatsRRRC)){
      x <- as.numeric(orgFStatsRRRC[[i]])
      y <- as.numeric(newFStatsRRRC[[i]])
      expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsFRRC <- c(orgValues$fFRRC, orgValues$ddfFRRC, orgValues$pFRRC)
  newFStatsFRRC <- newValues$FTestStatsFRRC
  newFStatsFRRC <- unlist(newFStatsFRRC[-2])
  names(newFStatsFRRC) <- NULL
  for (i in 1: length(orgFStatsFRRC)){
    x <- as.numeric(orgFStatsFRRC[[i]])
    y <- as.numeric(newFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsRRFC <- c(orgValues$fRRFC, orgValues$ddfRRFC, orgValues$pRRFC)
  newFStatsRRFC <- newValues$FTestStatsRRFC
  newFStatsRRFC <- unlist(newFStatsRRFC[-2])
  names(newFStatsRRFC) <- NULL
  for (i in 1: length(orgFStatsRRFC)){
    x <- as.numeric(orgFStatsRRFC[[i]])
    y <- as.numeric(newFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  #######################################  VarComp ###########################################
  orgVarComp <- as.vector(unlist(orgValues$varComp))
  newVarComp <- as.vector(unlist(newValues$varComp))
  for (i in 1: length(orgVarComp)){
    x <- orgVarComp[i]
    y <- newVarComp[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  ##################################  diff and avg confidence intervals  #####################
  orgciDiffTrtRRRC <- as.vector(unlist(orgValues$ciDiffTrtRRRC))[-1] # remove 0-1 vs trt0-trt1
  newciDiffTrtRRRC <- as.vector(unlist(newValues$ciDiffTrtRRRC))[-1]
  for (i in 1: length(orgciDiffTrtRRRC)){
    x <- orgciDiffTrtRRRC[i]
    y <- newciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRRC <- as.vector(unlist(orgValues$ciAvgRdrEachTrtRRRC))[-1] # remove 0-1 vs trt0-trt1
  newciAvgRdrEachTrtRRRC <- as.vector(unlist(newValues$ciAvgRdrEachTrtRRRC))[-1]
  for (i in 1: length(orgciAvgRdrEachTrtRRRC)){
    x <- orgciAvgRdrEachTrtRRRC[i]
    y <- newciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtFRRC <- as.vector(unlist(orgValues$ciDiffTrtFRRC))[-1] # remove 0-1 vs trt0-trt1
  newciDiffTrtFRRC <- as.vector(unlist(newValues$ciDiffTrtFRRC))[-1]
  for (i in 1: length(orgciDiffTrtFRRC)){
    x <- orgciDiffTrtFRRC[i]
    y <- newciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtFRRC <- as.vector(unlist(orgValues$ciAvgRdrEachTrtFRRC))[-1] # remove 0-1 vs trt0-trt1
  newciAvgRdrEachTrtFRRC <- as.vector(unlist(newValues$ciAvgRdrEachTrtFRRC))[-1]
  for (i in 1: length(orgciAvgRdrEachTrtFRRC)){
    x <- orgciAvgRdrEachTrtFRRC[i]
    y <- newciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtRRFC <- as.vector(unlist(orgValues$ciDiffTrtRRFC))[-1] # remove 0-1 vs trt0-trt1
  newciDiffTrtRRFC <- as.vector(unlist(newValues$ciDiffTrtRRFC))[-1]
  for (i in 1: length(orgciDiffTrtRRFC)){
    x <- orgciDiffTrtRRFC[i]
    y <- newciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRFC <- as.vector(unlist(orgValues$ciAvgRdrEachTrtRRFC))[-1] # remove 0-1 vs trt0-trt1
  newciAvgRdrEachTrtRRFC <- as.vector(unlist(newValues$ciAvgRdrEachTrtRRFC))[-1]
  for (i in 1: length(orgciAvgRdrEachTrtRRFC)){
    x <- orgciAvgRdrEachTrtRRFC[i]
    y <- newciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  
})

context("Compare current to original codes: ORH")

test_that("Compare current to original code: ORH", {
  
  ds <- dataset02
  
  orgValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "ORH", tempOrgCode = TRUE)
  newValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "ORH", tempOrgCode = FALSE)
  
  #######################################  FStats ###########################################  
  orgFStatsRRRC <- c(orgValues$fRRRC, orgValues$ddfRRRC, orgValues$pRRRC)
  newFStatsRRRC <- newValues$FTestStatsRRRC
  newFStatsRRRC <- unlist(newFStatsRRRC[-2]) # remove ndf
  names(newFStatsRRRC) <- NULL
  for (i in 1: length(orgFStatsRRRC)){
    x <- as.numeric(orgFStatsRRRC[[i]])
    y <- as.numeric(newFStatsRRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsFRRC <- c(orgValues$fFRRC, orgValues$ddfFRRC, orgValues$pFRRC)
  newFStatsFRRC <- newValues$FTestStatsFRRC
  newFStatsFRRC <- unlist(newFStatsFRRC[-2])
  names(newFStatsFRRC) <- NULL
  for (i in 1: length(orgFStatsFRRC)){
    x <- as.numeric(orgFStatsFRRC[[i]])
    y <- as.numeric(newFStatsFRRC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsRRFC <- c(orgValues$fRRFC, orgValues$ddfRRFC, orgValues$pRRFC)
  newFStatsRRFC <- newValues$FTestStatsRRFC
  newFStatsRRFC <- unlist(newFStatsRRFC[-2])
  names(newFStatsRRFC) <- NULL
  for (i in 1: length(orgFStatsRRFC)){
    x <- as.numeric(orgFStatsRRFC[[i]])
    y <- as.numeric(newFStatsRRFC[[i]])
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  #######################################  VarComp ###########################################
  orgVarComp <- as.vector(unlist(orgValues$varComp))
  newVarComp <- as.vector(unlist(newValues$varComp))
  for (i in 1: length(orgVarComp)){
    x <- orgVarComp[i]
    y <- newVarComp[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  ##################################  diff and avg confidence intervals  #####################
  orgciDiffTrtRRRC <- as.vector(unlist(orgValues$ciDiffTrtRRRC))[-1] # remove 0-1 vs trt0-trt1
  newciDiffTrtRRRC <- as.vector(unlist(newValues$ciDiffTrtRRRC))[-1]
  for (i in 1: length(orgciDiffTrtRRRC)){
    x <- orgciDiffTrtRRRC[i]
    y <- newciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRRC <- as.vector(unlist(orgValues$ciAvgRdrEachTrtRRRC))[-1] # remove 0-1 vs trt0-trt1
  newciAvgRdrEachTrtRRRC <- as.vector(unlist(newValues$ciAvgRdrEachTrtRRRC))[-1]
  for (i in 1: length(orgciAvgRdrEachTrtRRRC)){
    x <- orgciAvgRdrEachTrtRRRC[i]
    y <- newciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtFRRC <- as.vector(unlist(orgValues$ciDiffTrtFRRC))[-1] # remove 0-1 vs trt0-trt1
  newciDiffTrtFRRC <- as.vector(unlist(newValues$ciDiffTrtFRRC))[-1]
  for (i in 1: length(orgciDiffTrtFRRC)){
    x <- orgciDiffTrtFRRC[i]
    y <- newciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtFRRC <- as.vector(unlist(orgValues$ciAvgRdrEachTrtFRRC))[-1] # remove 0-1 vs trt0-trt1
  newciAvgRdrEachTrtFRRC <- as.vector(unlist(newValues$ciAvgRdrEachTrtFRRC))[-1]
  for (i in 1: length(orgciAvgRdrEachTrtFRRC)){
    x <- orgciAvgRdrEachTrtFRRC[i]
    y <- newciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtRRFC <- as.vector(unlist(orgValues$ciDiffTrtRRFC))[-1] # remove 0-1 vs trt0-trt1
  newciDiffTrtRRFC <- as.vector(unlist(newValues$ciDiffTrtRRFC))[-1]
  for (i in 1: length(orgciDiffTrtRRFC)){
    x <- orgciDiffTrtRRFC[i]
    y <- newciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRFC <- as.vector(unlist(orgValues$ciAvgRdrEachTrtRRFC))[-1] # remove 0-1 vs trt0-trt1
  newciAvgRdrEachTrtRRFC <- as.vector(unlist(newValues$ciAvgRdrEachTrtRRFC))[-1]
  for (i in 1: length(orgciAvgRdrEachTrtRRFC)){
    x <- orgciAvgRdrEachTrtRRFC[i]
    y <- newciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  
})

