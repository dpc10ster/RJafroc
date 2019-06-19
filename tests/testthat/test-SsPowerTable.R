context("SsPowerTable tests")
library(RJafroc)

test_that("SsPowerTable should error on unknown method", {
expect_error(SsPowerTable(dataset02, method = "UNKNOWN"),"Incorrect method." )
})

test_that("SsPowerTable should error on unknown dataset", {
expect_error(SsPowerTable(datasetXTESTX, method = "DBMH"),"not found" )
})