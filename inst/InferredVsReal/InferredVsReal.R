library(RJafroc)

# start with original Federica FROC dataset
ds <- dataset04
# convert to ROC and extract modalities 4 and 4
infd_ds <- DfExtractDataset(DfFroc2Roc(ds), trts = c(4,5))

# Federica real ROC dataset
real_ds <- dataset14

# load crossed modality dataset; this serves as template
fn <- "~/GitHub/datasets/CrossedModalitiesDataFile.xlsx"
xds <- DfReadCrossedModalities(fn)

# fix ratings list
xds$ratings$NL <- array(dim = c(2,2,4,200,1))
xds$ratings$NL[1,,,,1] <- infd_ds$ratings$NL
xds$ratings$NL[2,,,,1] <- real_ds$ratings$NL

xds$ratings$LL <- array(dim = c(2,2,4,100,1))
xds$ratings$LL[1,,,,1] <- infd_ds$ratings$LL
xds$ratings$LL[2,,,,1] <- real_ds$ratings$LL

# fix lesion list
xds$lesions$perCase <- array(1,dim = c(100))
xds$lesions$IDs <- array(1,dim = c(100,1))
xds$lesions$weights <- array(1,dim = c(100,1))

# fix descriptions list
xds$descriptions$fileName <- c("combined", "dataset04 & ", "dataset14")
xds$descriptions$type <- "ROC"
xds$descriptions$name <- "FED inferred plus real"
xds$descriptions$design <- "FCTRL-X-MOD"
xds$descriptions$modalityID1 <- c("infd", "real")
xds$descriptions$modalityID2 <- c("trt4", "trt5")
xds$descriptions$readerID <- c("rdr1","rdr2","rdr3","rdr4")

st <- StSignificanceTestingCrossedModalities(xds, avgIndx = 2, FOM <- "Wilcoxon", analysisOption = "RRRC")

print(st)
