save(datasetCrossedModality, file = "~/GitHub/RJafroc/data/datasetCrossedModality.RData")

## read the raw data file in extdata directory
crossedFileName <- system.file("extdata", "CrossedModalitiesData.xlsx",
package = "RJafroc", mustWork = TRUE)
crossedData <- DfReadCrossedModalities(crossedFileName)
retCrossed1 <- StSignificanceTestingCrossedModalities(crossedData, 1)
## read the built in dataset
retCrossed2 <- StSignificanceTestingCrossedModalities(datasetCrossedModality, 1)

library(testthat)
expect_equal(crossedData, datasetCrossedModality)

devtools::check(run_dont_test =  TRUE)
