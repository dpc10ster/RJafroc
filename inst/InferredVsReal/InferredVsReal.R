library(RJafroc)
library(testthat)

ds <- DfExtractDataset(DfFroc2Roc(dataset04), trts = c(4,5))
infd_ds <- DfExtractDataset(DfFroc2Roc(dataset04), trts = c(4,5))
expect_equal(ds$ratings$NL, infd_ds$ratings$NL)
expect_equal(ds$ratings$LL, infd_ds$ratings$LL)

real_ds <- dataset14

xds <- datasetCrossedModality

xds$ratings$NL <- array(dim = c(2,2,4,200,1))
xds$ratings$NL[1,,,,1] <- infd_ds$ratings$NL
xds$ratings$NL[2,,,,1] <- real_ds$ratings$NL

xds$ratings$LL <- array(dim = c(2,2,4,100,1))
xds$ratings$LL[1,,,,1] <- infd_ds$ratings$LL
xds$ratings$LL[2,,,,1] <- real_ds$ratings$LL

xds$lesions$perCase <- array(1,dim = 100)
xds$lesions$IDs <- array(1,dim = 100)
xds$lesions$weights <- array(1,dim = c(100,1))

xds$descriptions$type <- "combined inferred and real FED datasets"
xds$descriptions$type <- "ROC"
xds$descriptions$name <- "FED inferred plus real x-mod"
xds$descriptions$modalityID1 <- c("infd", "real")
xds$descriptions$modalityID2 <- c("trt4", "trt5")
xds$descriptions$readerID <- c("rdr1","rdr2","rdr3","rdr4")

st <- StSignificanceTestingCrossedModalities(xds, avgIndx = 2, FOM <- "Wilcoxon", analysisOption = "RRRC")
