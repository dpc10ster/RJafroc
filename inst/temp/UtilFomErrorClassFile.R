library(RJafroc)
rm(list = ls())

fileName <- system.file("extdata", "/toyFiles/FROC/frocLocatClass.xlsx", package = "RJafroc", mustWork = TRUE)
#fileName <- system.file("extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)

x1 <- DfReadDataFile(fileName = fileName, newExcelFileFormat = T)
print(UtilFigureOfMerit(x1, FOM = "wAFROC1"))

x2 <- DfReadDataFile(fileName = fileName, newExcelFileFormat = F)
print(UtilFigureOfMerit(x2, FOM = "wAFROC1"))



