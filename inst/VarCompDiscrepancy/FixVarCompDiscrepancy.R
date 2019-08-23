rm(list = ls())
fileName <- system.file("extdata", "FrocData.xlsx",
package = "RJafroc", mustWork = TRUE)
ds <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
resDBMH<- StSignificanceTesting(ds, FOM = "HrAuc")
resOR <- StSignificanceTesting(ds, FOM = "HrAuc", method = "ORH")
