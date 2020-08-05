# fixed missing truthTableStr in datasetCadLroc: 8/4/20
# fixed missing truthTableStr in datasetCadSimuFroc: 8/4/20
# 
library(RJafroc)
rm(list = ls())

generatetruthTableStr <- function(I, J, K1, K2, perCase){
  truthTableStr <- array(dim = c(I, J, K1+K2, max(x$lesions$perCase)+1))
  truthTableStr[,,1:K1,1] <- 1
  for (k2 in 1:K2) {
    truthTableStr[,,k2+K1,(1:perCase[k2])+1] <- 1
  }
  return(truthTableStr)
}


fileNames <- c("dataset01", "dataset02", "dataset03", "dataset04", "dataset05", "dataset06",
               "dataset07", "dataset08", "dataset09", "dataset10", "dataset11", "dataset12",
               "dataset13", "dataset14", 
               "datasetBinned123", "datasetBinned124", "datasetBinned125",
               "datasetCadLroc", "datasetCadSimuFroc", 
               "datasetCrossedModality",
               "datasetDegenerate", "datasetFROCSpC", "datasetROI")

# following is not used
binned <- list()
for (i in 1:length(fileNames)) {
  binned[[i]] <- isBinnedDataset(get(fileNames[i]))
}
rm("binned")
dataTypes <- c("FROC", "ROC", "ROC", "FROC", "FROC", "FROC",
               "FROC", "ROC", "ROC", "ROC", "FROC", "ROC",
               "FROC", "ROC", "ROC", "ROC", "ROC", "LROC", 
               "FROC", "FROC", "ROC", "FROC", "ROI")

design <- rep("FCTRL", length(fileNames))
design[20] <- "FCTRL-X-MOD"
design[22] <- "SPLIT-PLOT-C"

dataNames <- c("TONY", "VAN-DYKE", "FRANKEN", "FEDERICA", "THOMPSON", "MAGNUS",
               "LUCY-WARREN", "PENEDO", "NICO-CAD-ROC", "RUSCHIN", "DOBBINS-1", "DOBBINS-2",
               "DOBBINS-3", "FEDERICA-REAL-ROC", 
               "SIM-CORCBM-SEED-123", 
               "SIM-CORCBM-SEED-124", 
               "SIM-CORCBM-SEED-125",
               "NICO-CAD-LROC", 
               "SIM-CAD-FROC", 
               "THOMPSON-X-MOD",
               "SIM-DEGENERATE", 
               "SIM-FROC-SPLIT-PLOT-C", 
               "SIM-ROI")

df <- data.frame(fileNames = fileNames,
                 dataTypes = dataTypes,
                 design = design,
                 dataNames = dataNames,
                 stringsAsFactors = FALSE)

writeFile <- F

for (i in 1:length(df[,1])) {
  if (i != 19) next # CAD LROC dataset
  cat(sprintf("fixing  %s\n", df[i,1]))
  x <- get(df[i,1])
  I <- length(x$ratings$NL[,1,1,1])
  J <- length(x$ratings$NL[1,,1,1])
  K <- length(x$ratings$NL[1,1,,1])
  K2 <- length(x$ratings$LL[1,1,,1])
  K1 <- K - K2
  ratings <- x$ratings
  lesions <- x$lesions
  descriptions <- x$descriptions
  perCase <- x$lesions$perCase
  truthTableStr <- array(dim = c(I, J, K1+K2, max(perCase)+1))
  truthTableStr[,,1:K1,1] <- 1
  for (k2 in 1:K2) {
    truthTableStr[,,k2+K1,(1:perCase[k2])+1] <- 1
  }
  
  descriptions$truthTableStr <- truthTableStr
  
  x <- list(ratings = ratings,
            lesions = lesions,
            descriptions = descriptions)
  
  assign(df[i,1],x)
  
  rm(x)
  
  fn <- paste0("~/GitHub/RJafroc/data/", df[i,1], ".RData")
  if (writeFile) save(list = df[i,1], file = fn)
  
}

# clean up for good display in Environment Panel
rm(list = c("lesions", "ratings", "descriptions", "dataNames",
            "dataTypes", "design", "fileNames", "fn",
            "i", "writeFile", "df"))



