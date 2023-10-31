library(RJafroc)
library(testthat)

flag <- FALSE
# start with original Federica FROC dataset
if (flag) ds <- dataset04 else ds <- dataset14
# convert to ROC and extract modalities 4 and 4
if (flag) infd_ds <- DfExtractDataset(DfFroc2Roc(ds), trts = c(4,5)) else infd_ds <- ds

# Federica real ROC dataset
real_ds <- dataset14

# load crossed modality dataset; this serves as template
fn <- "~/GitHub/datasets/XModDataFile.xlsx"
xds <- DfReadXModalities(fn)

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

st <- StXMod(xds, avgIndx = 2, FOM <- "Wilcoxon", analysisOption = "RRRC")

if (flag) {
  # extract first level of outermost modality
  
  x1 <- xds$ratings$NL[1,,,,];dim(x1) <- c(2,4,200,1)
  xds$ratings$NL <- x1
  
  x1 <- xds$ratings$LL[1,,,,];dim(x1) <- c(2,4,100,1)
  xds$ratings$LL <- x1
  
  xds$descriptions$fileName <- "Fed Infd ROC"
  xds$descriptions$name <- "Fed Infd ROC"
  xds$descriptions$design <- "FCTRL"
  xds$descriptions$modalityID1 <- NULL
  xds$descriptions$modalityID2 <- NULL
  xds$descriptions$modalityID <- c("4", "5")
  xds$descriptions$readerID <- c("1", "2", "3", "4")
  xds$descriptions$truthTableStr <- dataset14$descriptions$truthTableStr
  
  st_infd_xmod <- St(xds, FOM = "Wilcoxon", analysisOption = "RRRC") # using xmod code
  st_infd_std <- St(infd_ds, FOM = "Wilcoxon", analysisOption = "RRRC") # standard code
  
  expect_equal(st_infd_xmod$FOMs, st_infd_std$FOMs)
  expect_equal(st_infd_xmod$ANOVA, st_infd_std$ANOVA)
  expect_equal(st_infd_xmod$RRRC, st_infd_std$RRRC)
  
} else {
  # extract second level of outermost modality
  
  x1 <- xds$ratings$NL[2,,,,];dim(x1) <- c(2,4,200,1)
  xds$ratings$NL <- x1
  
  x1 <- xds$ratings$LL[2,,,,];dim(x1) <- c(2,4,100,1)
  xds$ratings$LL <- x1
  
  xds$descriptions$fileName <- "Fed Infd ROC"
  xds$descriptions$name <- "FEDERICA-REAL-ROC"
  xds$descriptions$design <- "FCTRL"
  xds$descriptions$modalityID1 <- NULL
  xds$descriptions$modalityID2 <- NULL
  xds$descriptions$modalityID <- c("4", "5")
  xds$descriptions$readerID <- c("1", "2", "3", "4")
  xds$descriptions$truthTableStr <- dataset14$descriptions$truthTableStr
  
  st_infd_xmod <- St(xds, FOM = "Wilcoxon", analysisOption = "RRRC") # using xmod code
  st_infd_std <- St(infd_ds, FOM = "Wilcoxon", analysisOption = "RRRC") # standard code
  
  expect_equal(st_infd_xmod$FOMs, st_infd_std$FOMs)
  expect_equal(st_infd_xmod$ANOVA, st_infd_std$ANOVA)
  expect_equal(st_infd_xmod$RRRC, st_infd_std$RRRC)
}