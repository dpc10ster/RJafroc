context("StSignificanceTestingCrossedModalities tests")
library(RJafroc)

test_that("StSignificanceTestingCrossedModalities: no warnings/erros using datasetCrossedModality", {
expect_silent(StSignificanceTestingCrossedModalities(datasetCrossedModality, 1))
})
