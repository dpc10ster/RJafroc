# check new vs. old data file read codes
# this is actually implemented in testthat
# but is kind of easier to run from here, as all Run / Source options are available
ds <- DfReadDataFile("~/GitHub/RJafroc/inst/extdata/toyFiles/ROC/OK.xlsx")
dsNew <- DfReadDataFile("~/GitHub/RJafroc/inst/extdata/toyFiles/FROC/OK.xlsx")
dsOld <- DfReadDataFile("~/GitHub/RJafroc/inst/extdata/toyFiles/FROC/OK.xlsx", newExcelFileFormat = FALSE)
library(testthat)
expect_equal(dsNew, dsOld)


#overwrite crossed modality data file
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
devtools::build_vignettes()

devtools::check_failures("~/GitHub/RJafroc.Rcheck")

library(pkgdown)
pkgdown::build_site()
# must push Git after this has run to put on website
# https://dpc10ster.github.io/RJafrocRJafrocBook/
# 


