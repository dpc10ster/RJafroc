library(RJafroc)
rm(list = ls())

# froc <- system.file("extdata", "FrocData.xlsx",
#                       package = "RJafroc", mustWork = TRUE)
# x1 <- DfReadDataFile(froc, newExcelFileFormat = TRUE)
# 
# fom1 <- UtilFigureOfMerit(x1, FOM = "HrAuc")
# 
# frocSp <- system.file("extdata", "toyFiles/FROC/FrocDataSp.xlsx",
#                       package = "RJafroc", mustWork = TRUE)
# x2 <- DfReadDataFile(frocSp, newExcelFileFormat = TRUE)
# 
# fom2 <- UtilFigureOfMerit(x2, FOM = "HrAuc")
# 
frocSp2 <- system.file("extdata", "toyFiles/FROC/FrocDataSpVaryK1K2.xlsx",
                      package = "RJafroc", mustWork = TRUE)
x3 <- DfReadDataFile(frocSp2, newExcelFileFormat = TRUE)

fom3 <- UtilFigureOfMerit(x3, FOM = "wAFROC")
st1 <- StSignificanceTesting(x3, FOM = "wAFROC", method = "ORH")

st2 <- StSignificanceTesting(datasetFROCSp, FOM = "wAFROC", method = "ORH")
testthat::expect_equal(st1, st2)

testthat::expect_error(StSignificanceTesting(datasetFROCSp, FOM = "wAFROC", method = "DBMH"))