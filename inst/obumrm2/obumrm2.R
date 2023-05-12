fileName1 <- system.file(
  "extdata", "toyFiles/roc/roc1.xlsx", package = "RJafroc", mustWork = TRUE)
fileName2 <- system.file(
  "extdata", "toyFiles/roc/roc2.xlsx", package = "RJafroc", mustWork = TRUE)

ds1 <- DfReadDataFile(fileName1, newExcelFileFormat = T)
ds2 <- DfReadDataFile(fileName2, newExcelFileFormat = T)

varCom1 <- UtilVarComponentsOR(ds1, FOM = "Wilcoxon")
varCom2 <- UtilVarComponentsOR(ds2, FOM = "Wilcoxon")
