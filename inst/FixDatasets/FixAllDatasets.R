library(RJafroc)
rm(list = ls())

generatetruthTableStr <- function(I, J, K, x){
  truthTableStr <- array(dim = c(I, J, K, max(x$lesions$perCase)+1))
  truthTableStr[,,1:K1,1] <- 1
  for (k2 in 1:K2) {
    truthTableStr[,,k2+K1,(1:x$lesionVector[k2])+1] <- 1
  }
}

stop("This should not be used any more; only used for initial conversion from old to new dataset; use TweakAllDatasets.R instead")

fileNames <- c("dataset01", "dataset02", "dataset03", "dataset04", "dataset05", "dataset06",
               "dataset07", "dataset08", "dataset09", "dataset10", "dataset11", "dataset12",
               "dataset13", "dataset14", 
               "datasetBinned123", "datasetBinned124", "datasetBinned125",
               "datasetCadLroc", "datasetCadSimuFroc", 
               "datasetX",
               "datasetDegenerate", "TBD", "datasetROI")

binned  <- c(rep(TRUE, 4), FALSE, rep(TRUE, 3), FALSE, rep(TRUE, 8), 
             rep(FALSE, 3), rep(TRUE, 2), FALSE)

dataTypes <- c("FROC", "ROC", "ROC", "FROC", "FROC", "FROC",
               "FROC", "ROC", "ROC", "ROC", "FROC", "ROC",
               "FROC", "ROC", "ROC", "ROC", "ROC", "LROC", 
               "FROC", "FROC", "ROC", "FROC", "ROI")

design <- rep("FCTRL", length(fileNames))
design[20] <- "FCTRL-X-MOD"
design[22] <- "TBD"

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
               "SIM-FROC-TBD", 
               "SIM-ROI")

df <- data.frame(fileNames = fileNames,
                 dataTypes = dataTypes,
                 design = design,
                 dataNames = dataNames,
                 stringsAsFactors = FALSE)

writeFile <- FALSE

for (i in 1:length(df[,1])) {
  
  cat(sprintf("fixing  %s\n", df[i,1]))
  x <- get(df[i,1])
  
  # this code cannot run on already converted datasets  
  if (length(x) == 3) next
  
  if (x$dataType != df[i,2]) stop("data types do not match")
  
  if (i %in% c(15,16,17)) {
    # CORCBM datasets
    ratings <- list(NL = x$NL,
                    LL = x$LL,
                    LL_IL = NA)
    
    lesions <- list(perCase = x$lesionVector,
                    IDs = x$lesionID,
                    weights = x$lesionWeight)
    
    descriptions <- list(binned = df[i,5],
                         fileName = df[i,1],
                         type = df[i,2],
                         name = df[i,4],
                         truthTableStr = NA,
                         design = df[i,3],
                         modalityID = x$modalityID,
                         readerID = x$readerID)
  } else if (i %in% seq(1,14)) {
    ratings <- list(NL = x$NL,
                    LL = x$LL,
                    LL_IL = NA)
    
    lesions <- list(perCase = x$lesionVector,
                    IDs = x$lesionID,
                    weights = x$lesionWeight)
    
    if (is.null(x$descriptions$truthTableStr)) stop("check here")
    if (length(x$descriptions$truthTableStr) < 10) stop("check here")
    
    descriptions <- list(binned = df[i,5],
                         fileName = df[i,1],
                         type = df[i,2],
                         name = df[i,4],
                         truthTableStr = x$descriptions$truthTableStr, # this is really needed here
                         design = df[i,3],
                         modalityID = x$modalityID,
                         readerID = x$readerID)
  } else if (i == 18) {
    # LROC data Nico
    ratings <- list(NL = x$NL,
                    LL = x$LLCl,
                    LL_IL = x$LLIl)
    
    lesions <- list(perCase = x$lesionVector,
                    IDs = x$lesionID,
                    weights = x$lesionWeight)
    
    descriptions <- list(binned = df[i,5],
                         fileName = df[i,1],
                         type = df[i,2],
                         name = df[i,4],
                         truthTableStr = NA,
                         design = df[i,3],
                         modalityID = x$modalityID,
                         readerID = x$readerID)
  } else if (i == 19) {
    # simulated CAD FROC
    ratings <- list(NL = x$NL,
                    LL = x$LL,
                    LL_IL = NA)
    
    lesions <- list(perCase = x$lesionVector,
                    IDs = x$lesionID,
                    weights = x$lesionWeight)
    
    descriptions <- list(binned = df[i,5],
                         fileName = df[i,1],
                         type = df[i,2],
                         name = df[i,4],
                         truthTableStr = NA,
                         design = df[i,3],
                         modalityID = x$modalityID,
                         readerID = x$readerID)
  } else if (i == 20) {
    # crossed modality
    ratings <- list(NL = x$NL,
                    LL = x$LL,
                    LL_IL = NA)
    
    lesions <- list(perCase = x$lesionVector,
                    IDs = x$lesionID,
                    weights = x$lesionWeight)
    
    descriptions <- list(binned = df[i,5],
                         fileName = df[i,1],
                         type = df[i,2],
                         name = df[i,4],
                         truthTableStr = NA,
                         design = df[i,3],
                         modalityID1 = x$modalityID1,
                         modalityID2 = x$modalityID2,
                         readerID = x$readerID)
  } else if (i == 21) {
    # degenerate ROC
    ratings <- list(NL = x$NL,
                    LL = x$LL,
                    LL_IL = NA)
    
    lesions <- list(perCase = x$lesionVector,
                    IDs = x$lesionID,
                    weights = x$lesionWeight)
    
    descriptions <- list(binned = df[i,5],
                         fileName = df[i,1],
                         type = df[i,2],
                         name = df[i,4],
                         truthTableStr = NA,
                         design = df[i,3],
                         modalityID = x$modalityID,
                         readerID = x$readerID)
  } else if (i == 23) {
    # simulated ROI
    ratings <- list(NL = x$NL,
                    LL = x$LL,
                    LL_IL = NA)
    
    lesions <- list(perCase = x$lesionVector,
                    IDs = x$lesionID,
                    weights = x$lesionWeight)
    
    descriptions <- list(binned = df[i,5],
                         fileName = df[i,1],
                         type = df[i,2],
                         name = df[i,4],
                         truthTableStr = NA,
                         design = df[i,3],
                         modalityID = x$modalityID,
                         readerID = x$readerID)
  } else stop("incorrect index i")
  
  if (length(ratings) != 3) stop("need to check here")
  if (length(lesions) != 3) stop("need to check here")
  if (descriptions$design != "FCTRL-X-MOD") {
    if (length(descriptions) != 8) stop("need to check here")
  } else {
    if (length(descriptions) != 9) stop("need to check here")
  }
  
  x <- list(ratings = ratings,
            lesions = lesions,
            descriptions = descriptions)
  
  # check binning status
  z <- length(unique(c(x$ratings$LL[is.finite(x$ratings$LL)], x$ratings$NL[is.finite(x$ratings$NL)])))
  if ((z <= 6) && (x$descriptions$binned != TRUE)) stop ("need check here")
  # cat(sprintf("... unique ratings = %d\n", z))
  
  assign(df[i,1],x)
  
  rm(x)
  
  fn <- paste0("~/GitHub/RJafroc/data/", df[i,1], ".RData")
  if (writeFile) save(list = df[i,1], file = fn)
  
}

# clean up for good display in Environment Panel
rm(list = c("lesions", "ratings", "descriptions", "dataNames", 
            "dataTypes", "design", "fileNames", "fn", "binned", 
            "i", "z", "writeFile", "df"))



