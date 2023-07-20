contextStr <- "Compare current to original codes: DBM"
context(contextStr)
test_that(contextStr, {
  ds <- dataset02
  
  orgValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "DBM", tempOrgCode = TRUE)
  newValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "DBM", tempOrgCode = FALSE)
  
  #######################################  FStats ###########################################  
  orgFStatsRRRC <- c(orgValues$fRRRC, orgValues$ddfRRRC, orgValues$pRRRC)
  newFStatsRRRC <- c(newValues$RRRC$FTests[1,3],
                     newValues$RRRC$FTests[2,1],
                     newValues$RRRC$FTests[1,4]) # check FStat, DF(Error) and p value
  for (i in 1: length(orgFStatsRRRC)){
    x <- orgFStatsRRRC[i]
    y <- newFStatsRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsFRRC <- c(orgValues$fFRRC, orgValues$ddfFRRC, orgValues$pFRRC)
  newFStatsFRRC <- c(newValues$FRRC$FTests[1,3],
                     newValues$FRRC$FTests[2,1],
                     newValues$FRRC$FTests[1,4]) # check FStat, DF(Error) and p value
  for (i in 1: length(orgFStatsFRRC)){
    x <- orgFStatsFRRC[i]
    y <- newFStatsFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsRRFC <- c(orgValues$fRRFC, orgValues$ddfRRFC, orgValues$pRRFC)
  newFStatsRRFC <- 
    c(newValues$RRFC$FTests[1,3],
      newValues$RRFC$FTests[2,1],
      newValues$RRFC$FTests[1,4]) # check FStat, DF(Error) and p value
  for (i in 1: length(orgFStatsRRFC)){
    x <- orgFStatsRRFC[i]
    y <- newFStatsRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  #######################################  VarComp ###########################################
  orgVarComp <- as.vector(as.matrix(orgValues$varComp))
  newVarComp <- as.vector(as.matrix(newValues$ANOVA$VarCom))
  for (i in 1: length(orgVarComp)){
    x <- orgVarComp[i]
    y <- newVarComp[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  ##################################  diff and avg confidence intervals  #####################
  ##################################  DO NOT CHANGE TO NEW CLEANER CODE  ##################### 4/14/21
  orgciDiffTrtRRRC <- as.vector(as.matrix(orgValues$ciDiffTrtRRRC[,-1])) # do not change to orgValues$RRRC etc. 
  newciDiffTrtRRRC <- as.vector(as.matrix(newValues$RRRC$ciDiffTrt))
  for (i in 1: length(orgciDiffTrtRRRC)){
    x <- orgciDiffTrtRRRC[i]
    y <- newciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRRC <- as.numeric(as.vector(as.matrix(orgValues$ciAvgRdrEachTrtRRRC)))[-(1:2)] # remove 0-1 vs trt0-trt1
  newciAvgRdrEachTrtRRRC <- as.vector(as.matrix(newValues$RRRC$ciAvgRdrEachTrt))
  for (i in 1: length(orgciAvgRdrEachTrtRRRC)){
    x <- orgciAvgRdrEachTrtRRRC[i]
    y <- newciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtFRRC <- as.numeric(as.vector(as.matrix(orgValues$ciDiffTrtFRRC))[-(1)]) # remove 0-1 vs trt0-trt1
  newciDiffTrtFRRC <- as.vector(as.matrix(newValues$FRRC$ciDiffTrt))
  for (i in 1: length(orgciDiffTrtFRRC)){
    x <- orgciDiffTrtFRRC[i]
    y <- newciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtFRRC <- as.vector(as.matrix(orgValues$ciAvgRdrEachTrtFRRC[,-1]))  
  newciAvgRdrEachTrtFRRC <- as.vector(as.matrix(newValues$FRRC$ciAvgRdrEachTrt))
  for (i in 1: length(orgciAvgRdrEachTrtFRRC)){
    x <- orgciAvgRdrEachTrtFRRC[i]
    y <- newciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtRRFC <- as.vector(as.matrix(orgValues$ciDiffTrtRRFC[,-1]))
  newciDiffTrtRRFC <- as.vector(as.matrix(newValues$RRFC$ciDiffTrt))
  for (i in 1: length(orgciDiffTrtRRFC)){
    x <- orgciDiffTrtRRFC[i]
    y <- newciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRFC <- as.vector(as.matrix(orgValues$ciAvgRdrEachTrtRRFC[,-1]))
  newciAvgRdrEachTrtRRFC <- as.vector(as.matrix(newValues$RRFC$ciAvgRdrEachTrt))
  for (i in 1: length(orgciAvgRdrEachTrtRRFC)){
    x <- orgciAvgRdrEachTrtRRFC[i]
    y <- newciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  
})

contextStr <- "Compare current to original codes: ORH"
context(contextStr)
test_that(contextStr, {
  ds <- dataset02
  
  orgValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "OR", tempOrgCode = TRUE)
  newValues <- StSignificanceTesting(ds, FOM = "Wilcoxon", method = "OR", tempOrgCode = FALSE)
  
  #######################################  FStats ###########################################  
  orgFStatsRRRC <- c(orgValues$fRRRC, orgValues$ddfRRRC, orgValues$pRRRC)
  newFStatsRRRC <- 
    c(newValues$RRRC$FTests[1,3],
      newValues$RRRC$FTests[2,1],
      newValues$RRRC$FTests[1,4]) # check FStat, DF(Error) and p value
  for (i in 1: length(orgFStatsRRRC)){
    x <- orgFStatsRRRC[i]
    y <- newFStatsRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsFRRC <- c(orgValues$fFRRC, orgValues$pFRRC)
  newFStatsFRRC <- 
    c(newValues$FRRC$FTests[1,2],
      newValues$FRRC$FTests[1,4]) # check Chisq and p value
  for (i in 1: length(orgFStatsFRRC)){
    x <- orgFStatsFRRC[i]
    y <- newFStatsFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgFStatsRRFC <- c(orgValues$fRRFC, orgValues$ddfRRFC, orgValues$pRRFC)
  newFStatsRRFC <- c(newValues$RRFC$FTests[1,3],
                     newValues$RRFC$FTests[2,1],
                     newValues$RRFC$FTests[1,4])
  names(newFStatsRRFC) <- NULL
  for (i in 1: length(orgFStatsRRFC)){
    x <- orgFStatsRRFC[i]
    y <- newFStatsRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  #######################################  VarComp ###########################################
  orgVarComp <- as.vector(as.matrix(orgValues$varComp))
  newVarComp <- as.vector(as.matrix(newValues$ANOVA$VarCom))
  for (i in 1: length(orgVarComp)){
    x <- orgVarComp[i]
    y <- newVarComp[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  ##################################  diff and avg confidence intervals  #####################
  orgciDiffTrtRRRC <- as.vector(as.matrix(orgValues$ciDiffTrtRRRC[,-1]))
  newciDiffTrtRRRC <- as.vector(as.matrix(newValues$RRRC$ciDiffTrt))
  for (i in 1: length(orgciDiffTrtRRRC)){
    x <- orgciDiffTrtRRRC[i]
    y <- newciDiffTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRRC <- as.vector(as.matrix(orgValues$ciAvgRdrEachTrtRRRC[,-1]))
  newciAvgRdrEachTrtRRRC <- as.vector(as.matrix(newValues$RRRC$ciAvgRdrEachTrt))
  for (i in 1: length(orgciAvgRdrEachTrtRRRC)){
    x <- orgciAvgRdrEachTrtRRRC[i]
    y <- newciAvgRdrEachTrtRRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtFRRC <- as.numeric(as.vector(as.matrix(orgValues$ciDiffTrtFRRC))[-c(1,4)]) 
  newciDiffTrtFRRC <- as.vector(as.matrix(newValues$FRRC$ciDiffTrt))
  for (i in 1: length(orgciDiffTrtFRRC)){
    x <- orgciDiffTrtFRRC[i]
    y <- newciDiffTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtFRRC <- as.numeric(as.vector(as.matrix(orgValues$ciAvgRdrEachTrtFRRC)))[-c(1,2,7,8)]
  newciAvgRdrEachTrtFRRC <- as.vector(as.matrix(newValues$FRRC$ciAvgRdrEachTrt))[-c(5,6)]
  for (i in 1: length(orgciAvgRdrEachTrtFRRC)){
    x <- orgciAvgRdrEachTrtFRRC[i]
    y <- newciAvgRdrEachTrtFRRC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciDiffTrtRRFC <- as.vector(as.matrix(orgValues$ciDiffTrtRRFC[,-1]))
  newciDiffTrtRRFC <- as.vector(as.matrix(newValues$RRFC$ciDiffTrt))
  for (i in 1: length(orgciDiffTrtRRFC)){
    x <- orgciDiffTrtRRFC[i]
    y <- newciDiffTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  orgciAvgRdrEachTrtRRFC <- as.vector(as.matrix(orgValues$ciAvgRdrEachTrtRRFC[,-1]))
  newciAvgRdrEachTrtRRFC <- as.vector(as.matrix(newValues$RRFC$ciAvgRdrEachTrt))
  for (i in 1: length(orgciAvgRdrEachTrtRRFC)){
    x <- orgciAvgRdrEachTrtRRFC[i]
    y <- newciAvgRdrEachTrtRRFC[i]
    expect_equal(x, y, tolerance = 0.00001, scale = abs(x))
  }
  
  
})

