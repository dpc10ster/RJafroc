rm(list = ls())
frocCrAbnormalCasesFirst <- system.file("extdata", "toyFiles/FROC/frocCrAbnormalCasesFirst.xlsx",
                                        package = "RJafroc", mustWork = TRUE)
x <- DfReadDataFile(frocCrAbnormalCasesFirst, newExcelFileFormat = TRUE)

lesDistr <- UtilLesionDistribution(x)
lesWghts <- UtilLesionWeightsDistr(x)
