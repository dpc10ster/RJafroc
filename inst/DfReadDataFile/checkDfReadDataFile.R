# assign("last.warning", NULL, envir = baseenv())
library(RJafroc)
rm(list = ls())

FrocDataSp <- system.file("extdata", "toyFiles/FROC/FrocDataSp.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(FrocDataSp, newExcelFileFormat = TRUE)

# fed <- system.file("extdata", "FrocData.xlsx",
#                        package = "RJafroc", mustWork = TRUE)
# x <- DfReadDataFile(fed, newExcelFileFormat = TRUE)
# 
rocCr1R <- system.file("extdata", "toyFiles/ROC/rocCr1R.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocCr1R, newExcelFileFormat = TRUE)

rocCr1T <- system.file("extdata", "toyFiles/ROC/rocCr1T.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocCr1T, newExcelFileFormat = TRUE)
#testthat::expect_error(DfReadDataFile(rocCr1T, newExcelFileFormat = TRUE)) # ??

rocCr <- system.file("extdata", "toyFiles/ROC/rocCr.xlsx",
                     package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocCr, newExcelFileFormat = TRUE)

x1 <- DfReadDataFile(rocCr, newExcelFileFormat = FALSE)
testthat::expect_equal(x, x1)
rm(x1)

rocSp <- system.file("extdata", "toyFiles/ROC/rocSp.xlsx",
package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(rocSp, newExcelFileFormat = TRUE)

msngCell <- system.file("extdata", "toyFiles/ROC/missing1CellRow5.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(msngCell, newExcelFileFormat = TRUE))

msngCells <- system.file("extdata", "toyFiles/ROC/missing2CellsRow5and7.xlsx", 
                        package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(msngCells, newExcelFileFormat = TRUE))

msngRow <- system.file("extdata", "toyFiles/ROC/missingEntireRow5.xlsx", 
                         package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(msngRow, newExcelFileFormat = TRUE))

nonIntegerCells <- system.file("extdata", "toyFiles/ROC/non-integerCellsRows5and7.xlsx", 
                         package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(nonIntegerCells, newExcelFileFormat = TRUE))

nonNumericWeight <- system.file("extdata", "toyFiles/ROC/non-numeric-weight.xlsx", 
                                package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(nonNumericWeight, newExcelFileFormat = TRUE))

incWeights <- system.file("extdata", "toyFiles/ROC/incorrectWeights.xlsx", 
                          package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(incWeights, newExcelFileFormat = TRUE))

incSheetName <- system.file("extdata", "toyFiles/ROC/incorrectSheetName.xlsx", 
                            package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(incSheetName, newExcelFileFormat = TRUE))

dupRow <- system.file("extdata", "toyFiles/ROC/duplicatedRow.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(dupRow, newExcelFileFormat = TRUE))

# following uses non-integer caseIDs; it will not work
rocSpText <- system.file("extdata", "toyFiles/ROC/rocSpText.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(rocSpText, newExcelFileFormat = TRUE))

###########################################################################
frocCr013LesionsOldFormat <- system.file("extdata", "toyFiles/FROC/frocCr013LesionsOldFormat.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr013LesionsOldFormat, newExcelFileFormat = FALSE)


frocCr013Lesions <- system.file("extdata", "toyFiles/FROC/frocCr013Lesions.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
x2 <- DfReadDataFile(frocCr013Lesions, newExcelFileFormat = TRUE)
testthat::expect_equal(x, x2)

magnus <- system.file("extdata", "datasets/Magnus.xls", 
                      package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(magnus, newExcelFileFormat = FALSE))

frocCr <- system.file("extdata", "toyFiles/FROC/frocCr.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr, newExcelFileFormat = TRUE)

frocOld <- system.file("extdata", "toyFiles/FROC/frocCr.xlsx", 
                       package = "RJafroc", mustWork = TRUE)
x1 <- DfReadDataFile(frocOld, newExcelFileFormat = FALSE)
testthat::expect_equal(x,x1)
rm(x1)

frocCrAbnormalCasesFirst <- system.file("extdata", "toyFiles/FROC/frocCrAbnormalCasesFirst.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCrAbnormalCasesFirst, newExcelFileFormat = TRUE)

frocCrAbnormalCasesFirst <- system.file("extdata", "toyFiles/FROC/frocCrAbnormalCasesFirstOldFormat.xlsx",
                                        package = "RJafroc", mustWork = TRUE)
x2 <- DfReadDataFile(frocCrAbnormalCasesFirst, newExcelFileFormat = FALSE)
testthat::expect_equal(x,x2)

frocCr1R <- system.file("extdata", "toyFiles/FROC/frocCr1R.xlsx",
                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr1R, newExcelFileFormat = TRUE)

frocCr1T <- system.file("extdata", "toyFiles/FROC/frocCr1T.xlsx",
                       package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCr1T, newExcelFileFormat = TRUE)
#testthat::expect_error(DfReadDataFile(frocCr1T, newExcelFileFormat = TRUE)) # ??

frocCr2BlankRows <- system.file("extdata", "toyFiles/FROC/frocCr2BlankRows.xlsx",
                                package = "RJafroc", mustWork = TRUE)
x1 <- DfReadDataFile(frocCr2BlankRows, newExcelFileFormat = TRUE)
# testthat::expect_equal(x, x1)
# rm(x1)

frocSp <- system.file("extdata", "toyFiles/FROC/frocSp.xlsx", 
                      package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSp, newExcelFileFormat = TRUE)

frocSpZeroWgts <- system.file("extdata", "toyFiles/FROC/frocSpZeroWgts.xlsx", 
                              package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSpZeroWgts, newExcelFileFormat = TRUE)

frocSpDelRowsFP <- system.file("extdata", "toyFiles/FROC/frocSpDelRowsFP.xlsx", 
                               package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocSpDelRowsFP, newExcelFileFormat = TRUE)

# incCaseIdsFP <- system.file("extdata", "toyFiles/FROC/incorrectCaseIDsInFP.xlsx", 
#                             package = "RJafroc", mustWork = TRUE)
# testthat::expect_error(DfReadDataFile(incCaseIdsFP, newExcelFileFormat = TRUE))
# 
incCaseIdsTP <- system.file("extdata", "toyFiles/FROC/incorrectCaseIDsInTP.xlsx", 
                            package = "RJafroc", mustWork = TRUE)
testthat::expect_error(DfReadDataFile(incCaseIdsTP, newExcelFileFormat = TRUE))

