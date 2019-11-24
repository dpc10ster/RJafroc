fileName1 <- system.file("extdata", "toyFiles/ROC/SplitPlot.xlsx",
package = "RJafroc", mustWork = TRUE)
dsNew1 <- DfReadDataFile(fileName1, newExcelFileFormat = TRUE)

fileName2 <- system.file("extdata", "toyFiles/ROC/OK.xlsx",
                        package = "RJafroc", mustWork = TRUE)
dsNew2 <- DfReadDataFile(fileName2, newExcelFileFormat = FALSE)


fileName <- system.file("extdata", "toyFiles/FROC/OK.xlsx", 
                        package = "RJafroc", mustWork = TRUE)
ds1New <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
# ds1Old <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
# testthat::expect_equal(ds1New, ds1Old)

fileName <- system.file("extdata", "toyFiles/ROC/OK.xlsx", 
                        package = "RJafroc", mustWork = TRUE)
dsNew <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
dsOld <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
testthat::expect_equal(dsNew, dsOld)

fileName <- system.file("extdata", "toyFiles/FROC/OK.xlsx", 
                        package = "RJafroc", mustWork = TRUE)
ds1New <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
ds1Old <- DfReadDataFile(fileName, newExcelFileFormat = FALSE)
testthat::expect_equal(ds1New, ds1Old)


fileName <- system.file(
  "extdata", "/toyFiles/ROC/SplitPlot.xlsx", package = "RJafroc", mustWork = TRUE)
temp <- DfReadDataFile(fileName, newExcelFileFormat = TRUE)
