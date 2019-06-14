context("gpfMyFOM tests")
library(RJafroc)

test_that("gpfMyFOM should error on unknown FOM", {
expect_error(gpfMyFOM(FOM="TESTFOM"),"is not an available figure of merit." )
})
