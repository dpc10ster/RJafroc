library(RJafroc)
rm(list = ls())

# crossed toy roc data file
# rocCr <- system.file("extdata", "toyFiles/ROC/rocCr.xlsx",
#                        package = "RJafroc", mustWork = TRUE)
# x <- DfReadDataFile(rocCr, newExcelFileFormat = TRUE)
# f <- UtilFigureOfMerit(x, FOM = "Wilcoxon")
# 

# crossed real froc data file
frocCr <- system.file("extdata", "FrocData.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr, newExcelFileFormat = TRUE)
f <- UtilFigureOfMerit(x, FOM = "HrAuc")

# simulated real froc data file corresponding to FED dataset
frocSp <- system.file("extdata", "toyFiles/FROC/FrocDataSp.xlsx",
                     package = "RJafroc", mustWork = TRUE)
x2 <- DfReadDataFile(frocSp, newExcelFileFormat = TRUE)

f2 <- UtilFigureOfMerit(x2, FOM = "HrAuc")
