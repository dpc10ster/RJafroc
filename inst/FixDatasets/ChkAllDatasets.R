library(RJafroc)
rm(list = ls())

fileNames <- c("dataset01", "dataset02", "dataset03", "dataset04", "dataset05", "dataset06",
               "dataset07", "dataset08", "dataset09", "dataset10", "dataset11", "dataset12",
               "dataset13", "dataset14", 
               "datasetBinned123", "datasetBinned124", "datasetBinned125",
               "datasetCadLroc", "datasetCadSimuFroc", 
               "datasetXModality",
               "datasetDegenerate", "datasetFROCSp", "datasetROI")

binned  <- c(rep(TRUE, 4), FALSE, rep(TRUE, 3), FALSE, rep(TRUE, 8), 
             rep(FALSE, 3), rep(TRUE, 2), FALSE)

dataTypes <- c("FROC", "ROC", "ROC", "FROC", "FROC", "FROC",
               "FROC", "ROC", "ROC", "ROC", "FROC", "ROC",
               "FROC", "ROC", "ROC", "ROC", "ROC", "LROC", 
               "FROC", "FROC", "ROC", "FROC", "ROI")

design <- rep("FCTRL", length(fileNames))
design[20] <- "FCTRL-X-MOD"
design[22] <- "SPLIT-PLOT"

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
               "SIM-FROC-SPLIT-PLOT", 
               "SIM-ROI")

df <- data.frame(fileNames = fileNames,
                 dataTypes = dataTypes,
                 design = design,
                 dataNames = dataNames,
                 stringsAsFactors = FALSE)

for (i in 1:length(df[,1])) {
  
  cat(sprintf("checking  %s\n", df[i,1]))
  x <- get(df[i,1])
  
  if (!isValidDataset(x)) stop ("need check here")
}


