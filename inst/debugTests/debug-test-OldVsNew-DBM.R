library(RJafroc)
library(testthat)

dropRowColumnNames <- function (df) {
  rownames(df) <- NULL
  colnames(df) <- NULL
  return (df)
}

FOM_arr <- c("Wilcoxon", "HrAuc", "wAFROC","AFROC", "wAFROC1","AFROC1",
             "MaxLLF","MaxNLF","MaxNLFAllCases", "HrSp", "HrSe")

dataset_arr <- c("dataset02", "dataset05")

for (d in 1:length(dataset_arr)) {
  for (f in 1:length(FOM_arr)) {
    cat("d = ", d, ", f = ", f, "\n")
    
    dataset <- get(dataset_arr[d])
    FOM <- FOM_arr[f]
    
    if ((dataset$descriptions$type == "ROC") && (FOM != "Wilcoxon")) {
      
      # for ROC data, only Wilcoxon FOM is allowed
      expect_error(St(dataset, FOM = FOM, method = "DBM"))
      
    } else if ((dataset$descriptions$type == "FROC") && (FOM == "Wilcoxon")) {
      
      # for FROC data, Wilcoxon FOM is NOT allowed
      expect_error(St(dataset, FOM = FOM, method = "DBM"))
      
    } else {
      d <- 1; f <- 1
      ##### RRRC  ############################
      x1 <- StOldCode(dataset, FOM = FOM, method = "DBM", analysisOption = "RRRC")
      x2 <- St(dataset, FOM = FOM, method = "DBM", analysisOption = "RRRC")
      
      # $fomArray
      old <- dropRowColumnNames(x1$fomArray)
      new <- dropRowColumnNames(x2$FOMs$foms)
      expect_equal(old, new)
      
      # $anovaY
      old <- dropRowColumnNames(x1$anovaY[,-1])
      new <- dropRowColumnNames(x2$ANOVA$TRCanova)
      expect_equal(old, new)
      
      # $anovaYi
      old <- dropRowColumnNames(x1$anovaYi[,-1])
      new <- dropRowColumnNames(x2$ANOVA$IndividualTrt)
      expect_equal(old, new)
      
      # $varComp
      old <- dropRowColumnNames(x1$varComp)
      new <- dropRowColumnNames(x2$ANOVA$VarCom)
      expect_equal(old, new)
      
      # $fRRRC
      old <- dropRowColumnNames(x1$fRRRC)
      new <- dropRowColumnNames(x2$RRRC$FTests$FStat[1])
      expect_equal(old, new)
      
      # $ddfRRRC
      old <- dropRowColumnNames(x1$ddfRRRC)
      new <- dropRowColumnNames(x2$RRRC$FTests$DF[2])
      expect_equal(old, new)
      
      # $pRRRC
      old <- dropRowColumnNames(x1$pRRRC)
      new <- dropRowColumnNames(x2$RRRC$FTests$p[1])
      expect_equal(old, new)
      
      # $ciDiffTrtRRRC
      old <- dropRowColumnNames(x1$ciDiffTrtRRRC[,-1])
      new <- dropRowColumnNames(x2$RRRC$ciDiffTrt)
      expect_equal(old, new)
      
      # $ciAvgRdrEachTrtRRRC
      old <- dropRowColumnNames(x1$ciAvgRdrEachTrtRRRC[,-1])
      new <- dropRowColumnNames(x2$RRRC$ciAvgRdrEachTrt)
      expect_equal(old, new)
      
      ##### FRRC ############################
      x1 <- StOldCode(dataset, FOM = FOM, method = "DBM", analysisOption = "FRRC")
      x2 <- St(dataset, FOM = FOM, method = "DBM", analysisOption = "FRRC")
      
      # $fomArray
      old <- dropRowColumnNames(x1$fomArray)
      new <- dropRowColumnNames(x2$FOMs$foms)
      expect_equal(old, new)
      
      # $anovaY
      old <- dropRowColumnNames(x1$anovaY[,-1])
      new <- dropRowColumnNames(x2$ANOVA$TRCanova)
      expect_equal(old, new)
      
      # $anovaYi
      old <- dropRowColumnNames(x1$anovaYi[,-1])
      new <- dropRowColumnNames(x2$ANOVA$IndividualTrt)
      expect_equal(old, new)
      
      # $varComp
      old <- dropRowColumnNames(x1$varComp)
      new <- dropRowColumnNames(x2$ANOVA$VarCom)
      expect_equal(old, new)
      
      # $fFRRC
      old <- dropRowColumnNames(x1$fFRRC)
      new <- dropRowColumnNames(x2$FRRC$FTests$FStat[1])
      expect_equal(old, new)
      
      # $ddfFRRC
      old <- dropRowColumnNames(x1$ddfRRRC) # sic
      new <- dropRowColumnNames(x2$RRRC$FTests$DF[2])
      expect_equal(old, new)
      
      # $pFRRC
      old <- dropRowColumnNames(x1$pFRRC)
      new <- dropRowColumnNames(x2$FRRC$FTests$p[1])
      expect_equal(old, new)
      
      # $ciDiffTrtFRRC
      old <- dropRowColumnNames(x1$ciDiffTrtFRRC[,-1])
      new <- dropRowColumnNames(x2$FRRC$ciDiffTrt)
      expect_equal(old, new)
      
      # $ciAvgRdrEachTrtFRRC
      old <- dropRowColumnNames(x1$ciAvgRdrEachTrtFRRC[,-1])
      new <- dropRowColumnNames(x2$FRRC$ciAvgRdrEachTrt)
      expect_equal(old, new)
      
      # $ssAnovaEachRdr
      # dont need
      
      # $msAnovaEachRdr
      old <- dropRowColumnNames(x1$msAnovaEachRdr[,-1])
      new <- dropRowColumnNames(x2$ANOVA$IndividualRdr)
      expect_equal(old, new)
      
      # $ciDiffTrtEachRdr
      old <- dropRowColumnNames(x1$ciDiffTrtEachRdr[,-(1:2)])
      new <- dropRowColumnNames(x2$FRRC$ciDiffTrtEachRdr)
      expect_equal(old, new)
      
      ##### FRRC ############################
      x1 <- StOldCode(dataset, FOM = FOM, method = "DBM", analysisOption = "RRFC")
      x2 <- St(dataset, FOM = FOM, method = "DBM", analysisOption = "RRFC")
      
      # $fomArray
      old <- dropRowColumnNames(x1$fomArray)
      new <- dropRowColumnNames(x2$FOMs$foms)
      expect_equal(old, new)
      
      # $anovaY
      old <- dropRowColumnNames(x1$anovaY[,-1])
      new <- dropRowColumnNames(x2$ANOVA$TRCanova)
      expect_equal(old, new)
      
      # $anovaYi
      old <- dropRowColumnNames(x1$anovaYi[,-1])
      new <- dropRowColumnNames(x2$ANOVA$IndividualTrt)
      expect_equal(old, new)
      
      # $varComp
      old <- dropRowColumnNames(x1$varComp)
      new <- dropRowColumnNames(x2$ANOVA$VarCom)
      expect_equal(old, new)
      
      # $fFRRC
      old <- dropRowColumnNames(x1$fRRFC)
      new <- dropRowColumnNames(x2$RRFC$FTests$FStat[1])
      expect_equal(old, new)
      
      # $ddfFRRC
      old <- dropRowColumnNames(x1$ddfRRFC)
      new <- dropRowColumnNames(x2$RRFC$FTests$DF[2])
      expect_equal(old, new)
      
      # $pFRRC
      old <- dropRowColumnNames(x1$pRRFC)
      new <- dropRowColumnNames(x2$RRFC$FTests$p[1])
      expect_equal(old, new)
      
      # $ciDiffTrtRRFC
      old <- dropRowColumnNames(x1$ciDiffTrtRRFC[,-1])
      new <- dropRowColumnNames(x2$RRFC$ciDiffTrt)
      expect_equal(old, new)
      
      # $ciAvgRdrEachTrtRRFC
      old <- dropRowColumnNames(x1$ciAvgRdrEachTrtRRFC[,-1])
      new <- dropRowColumnNames(x2$RRFC$ciAvgRdrEachTrt)
      expect_equal(old, new)
    }
  }
}
