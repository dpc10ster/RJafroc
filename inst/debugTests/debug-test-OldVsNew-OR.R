library(RJafroc)
library(testthat)

dropRowColumnNames <- function (df) {
  rownames(df) <- NULL
  colnames(df) <- NULL
  return (df)
}

FOM_arr <- c("Wilcoxon", "HrAuc", "wAFROC","AFROC", "wAFROC1","AFROC1",
             "MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")

dataset_arr <- c("dataset02", "dataset05")


for (d in 1:length(dataset_arr)) {
  for (f in 1:length(FOM_arr)) {
    cat("d = ", d, ", f = ", f, "\n")
    
    dataset <- get(dataset_arr[d])
    FOM <- FOM_arr[f]
    
    if ((dataset$descriptions$type == "ROC") && (FOM != "Wilcoxon")) {
      
      # for ROC data, only Wilcoxon FOM is allowed
      expect_error(StSignificanceTesting(dataset, FOM = FOM, method = "OR"))
      
    } else if ((dataset$descriptions$type == "FROC") && (FOM == "Wilcoxon")) {
      
      # for FROC data, Wilcoxon FOM is NOT allowed
      expect_error(StSignificanceTesting(dataset, FOM = FOM, method = "OR"))
      
    } else {
      
      ##### RRRC  ############################
      x1 <- SigTestOldCode(dataset, FOM = FOM, method = "OR", analysisOption = "RRRC")
      x2 <- StSignificanceTesting(dataset, FOM = FOM, method = "OR", analysisOption = "RRRC")
      
      # $fomArray
      old <- dropRowColumnNames(x1$fomArray)
      new <- dropRowColumnNames(x2$FOMs$foms)
      expect_equal(old, new)
      
      # $msT
      old <- dropRowColumnNames(x1$msT)
      new <- dropRowColumnNames(x2$ANOVA$TRanova[1,3])
      expect_equal(old, new)
      
      # $msR
      old <- dropRowColumnNames(x1$msR)
      new <- dropRowColumnNames(x2$ANOVA$TRanova[2,3])
      expect_equal(old, new)
      
      # $msRSingle1
      old <- dropRowColumnNames(x1$msRSingle)[1]
      new <- dropRowColumnNames(x2$ANOVA$IndividualTrt[1,2])
      expect_equal(old, new)
      
      # $msRSingle2
      old <- dropRowColumnNames(x1$msRSingle)[2]
      new <- dropRowColumnNames(x2$ANOVA$IndividualTrt[2,2])
      expect_equal(old, new)
      
      # $msTR
      old <- dropRowColumnNames(x1$msTR)
      new <- dropRowColumnNames(x2$ANOVA$TRanova[3,3])
      expect_equal(old, new)
      
      # $varComp
      old <- dropRowColumnNames(x1$varComp)
      new <- dropRowColumnNames(x2$ANOVA$VarCom[-2])
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
      old <- dropRowColumnNames(x1$ciAvgRdrEachTrtRRRC[-1])
      new <- dropRowColumnNames(x2$RRRC$ciAvgRdrEachTrt[-6])
      expect_equal(old, new)
      
      ##### FRRC ############################
      x1 <- SigTestOldCode(dataset, FOM = FOM, method = "OR", analysisOption = "FRRC")
      x2 <- StSignificanceTesting(dataset, FOM = FOM, method = "OR", analysisOption = "FRRC")
      
      # $fomArray
      old <- dropRowColumnNames(x1$fomArray)
      new <- dropRowColumnNames(x2$FOMs$foms)
      expect_equal(old, new)
      
      # $msT
      old <- dropRowColumnNames(x1$msT)
      new <- dropRowColumnNames(x2$ANOVA$TRanova[1,3])
      expect_equal(old, new)
      
      # $msTR
      old <- dropRowColumnNames(x1$msTR)
      new <- dropRowColumnNames(x2$ANOVA$TRanova[3,3])
      expect_equal(old, new)
      
      # $varComp
      old <- dropRowColumnNames(x1$varComp)
      new <- dropRowColumnNames(x2$ANOVA$VarCom[-2])
      expect_equal(old, new)
      
      # $fFRRC
      old <- dropRowColumnNames(x1$fFRRC)
      new <- dropRowColumnNames(x2$FRRC$FTests[1,2])
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
      old <- dropRowColumnNames(x1$ciDiffTrtFRRC[,-c(1,4)])
      new <- dropRowColumnNames(x2$FRRC$ciDiffTrt)
      expect_equal(old, new)
      
      # $ciAvgRdrEachTrtFRRC
      old <- dropRowColumnNames(x1$ciAvgRdrEachTrtFRRC[,-c(1,4)])
      new <- dropRowColumnNames(x2$FRRC$ciAvgRdrEachTrt[,-3])
      expect_equal(old, new)
      
      # $ciDiffTrtEachRdr
      old <- dropRowColumnNames(x1$ciDiffTrtEachRdr[c(3,4,6,7,8,9)])
      new <- dropRowColumnNames(x2$FRRC$ciDiffTrtEachRdr[c(1,2,3,4,5,6)])
      expect_equal(old, new)
      
      # $varCovEachRdr
      old <- dropRowColumnNames(x1$varCovEachRdr[2])
      new <- dropRowColumnNames(x2$ANOVA$IndividualRdr[3])
      expect_equal(old, new)
      
      ##### FRRC ############################
      x1 <- SigTestOldCode(dataset, FOM = FOM, method = "OR", analysisOption = "RRFC")
      x2 <- StSignificanceTesting(dataset, FOM = FOM, method = "OR", analysisOption = "RRFC")
      
      # $fomArray
      old <- dropRowColumnNames(x1$fomArray)
      new <- dropRowColumnNames(x2$FOMs$foms)
      expect_equal(old, new)
      
      # $msT
      old <- dropRowColumnNames(x1$msT)
      new <- dropRowColumnNames(x2$ANOVA$TRanova[1,3])
      expect_equal(old, new)
      
      # $msTR
      old <- dropRowColumnNames(x1$msTR)
      new <- dropRowColumnNames(x2$ANOVA$TRanova[3,3])
      expect_equal(old, new)
      
      # $varComp
      old <- dropRowColumnNames(x1$varComp)
      new <- dropRowColumnNames(x2$ANOVA$VarCom[1])
      expect_equal(old, new)
      
      # $fFRRC
      old <- dropRowColumnNames(x1$fRRFC)
      new <- dropRowColumnNames(x2$RRFC$FTests$F[1])
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
