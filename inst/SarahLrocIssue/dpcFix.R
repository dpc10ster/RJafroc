library(RJafroc)

fn <- system.file("extdata", "/toyFiles/LROC/lroc.xlsx", package = "RJafroc", mustWork = TRUE)

lrocData <- DfReadDataFile(fn, newExcelFileFormat = TRUE, lrocForcedMark = TRUE)

p <- PlotEmpOpChrs(lrocData, opChType = "LROC")

print(p)
