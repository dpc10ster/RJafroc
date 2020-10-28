library(RJafroc)
rm(list = ls())
fileName <- system.file("extdata", "/toyFiles/FROC/frocLocatClass.xlsx", package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(fileName = fileName, newExcelFileFormat = T)

print(UtilFigureOfMerit(x, FOM = "wAFROC1"))


# fileName <- system.file(
#   "extdata", "/toyFiles/FROC/frocCr.xlsx", package = "RJafroc", mustWork = TRUE)
# 
# x <- DfReadDataFile(fileName = fileName)
# 
# UtilFigureOfMerit(x, FOM = "wAFROC1")


