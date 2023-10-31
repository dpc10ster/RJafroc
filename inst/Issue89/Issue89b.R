ds1 <- DfReadDataFile(fileName = "inst/Issue89/frocCr.xlsx", newExcelFileFormat = TRUE)
st1 <- St(ds1, FOM = "AFROC", method = "OR", analysisOption = "RRRC")

ds2 <- DfReadDataFile(fileName = "inst/Issue89/froc2.xlsx", newExcelFileFormat = TRUE)
st2 <- St(ds2, FOM = "AFROC", method = "OR", analysisOption = "RRRC")

testthat::expect_equal(ds1$ratings, ds2$ratings)
testthat::expect_equal(ds1$lesions, ds2$lesions)
testthat::expect_equal(ds1$descriptions$type, ds2$descriptions$type)
testthat::expect_equal(ds1$descriptions$truthTableStr, ds2$descriptions$truthTableStr)
testthat::expect_equal(ds1$descriptions$design, ds2$descriptions$design)
testthat::expect_equal(ds1$descriptions$modalityID, ds2$descriptions$modalityID)
testthat::expect_equal(ds1$descriptions$readerID, ds2$descriptions$readerID)

testthat::expect_equal(st1, st2)
