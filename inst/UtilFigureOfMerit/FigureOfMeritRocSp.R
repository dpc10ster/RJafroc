library(RJafroc)
rm(list = ls())

rocSp <- system.file("extdata", "toyFiles/ROC/rocSp.xlsx",
                      package = "RJafroc", mustWork = TRUE)
x1 <- DfReadDataFile(rocSp, newExcelFileFormat = TRUE)

fom1 <- UtilFigureOfMerit(x1, FOM = "Wilcoxon")

rocSp2 <- system.file("extdata", "toyFiles/ROC/rocSpVaryK.xlsx",
                      package = "RJafroc", mustWork = TRUE)
x2 <- DfReadDataFile(rocSp2, newExcelFileFormat = TRUE)

fom2 <- UtilFigureOfMerit(x2, FOM = "Wilcoxon")

frocSp2 <- system.file("extdata", "toyFiles/FROC/FrocDataSpVaryK1K2.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x3 <- DfReadDataFile(frocSp2, newExcelFileFormat = TRUE)
x31 <- DfFroc2Roc(x3)

fom3 <- UtilFigureOfMerit(x31, FOM = "Wilcoxon")
