library(RJafroc)
rm(list = ls())

frocSp <- system.file("extdata", "toyFiles/FROC/FrocDataSpVaryK1K2.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSp, newExcelFileFormat = TRUE)

st <- StSignificanceTesting(x, FOM = "wAFROC", method = "OR")
