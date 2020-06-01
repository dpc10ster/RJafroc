library(RJafroc)
rm(list = ls())


FixCrossedModalityDataset <- function(x, dataSetName) {
  
  # cat("skipping dataset: ", dataSetName, "\n")
  I1 <- length(x$NL[,1,1,1,1])
  I2 <- length(x$NL[1,,1,1,1])
  J <- length(x$NL[1,1,,1,1])
  K <- length(x$NL[1,1,1,,1])
  K2 <- length(x$LL[1,1,1,,1])
  K1 <- K - K2
  
  truthTableStr <- array(dim = c(I1, I2, J, K, max(x$lesionVector)+1))
  truthTableStr[,,,1:K1,1] <- 1
  for (k2 in 1:K2) {
    truthTableStr[,,,k2+K1,(1:x$lesionVector[k2])+1] <- 1
  }
  
  if (is.null(x$design)) {
    x <- list(
      NL = x$NL,
      LL = x$LL,
      lesionVector = x$lesionVector,
      lesionWeight = x$lesionWeight,
      dataType = x$dataType,
      modalityID1 = x$modalityID1,
      modalityID2 = x$modalityID2,
      readerID = x$readerID,
      design = "CROSSED-MODALITY",
      normalCases = seq(1, K1),
      abnormalCases = seq(1+K1, K1+K2),
      truthTableStr = truthTableStr,
      datasetName = "John Thompson Crossed Modality Dataset"
    )
    if (length(x) != 13) stop("Incorrect length dataset")
  } 
  
  return(x)
}

writeFile <- FALSE

dataStr <- c("dataset01", "dataset02", "dataset03", "dataset04", "dataset05", "dataset06",
             "dataset07", "dataset08", "dataset09", "dataset10", "dataset11", "dataset12",
             "dataset13", "dataset14", 
             "datasetBinned123", "datasetBinned124", "datasetBinned125",
             "datasetCadLroc", "datasetCadSimuFroc", 
             "datasetCrossedModality",
             "datasetDegenerate", "datasetFROCSp", "datasetROI")


for (i in 1:length(dataStr)) {
  
  cat("\n", dataStr[i])
  
  x <- eval(parse(text=dataStr[i]))
  
  if (length(x) == 13) { # already correct length
    if (!(x$design %in% c("CROSSED-MODALITY", "FACTORIAL", "SPLIT-PLOT"))) stop("Neded fix here")
    
  }
  
  if (length(dim(x$NL)) == 5) { # fix crossed dataset
    x <- FixCrossedModalityDataset(x, dataStr[i])
    fn <- paste0("~/GitHub/RJafroc/data/", dataStr[i], ".RData")
    if (writeFile) save(dataStr[i], file = fn)
    next
  }
  
  I <- length(x$modalityID)
  J <- length(x$readerID)
  K <- length(x$NL[1,1,,1])
  K2 <- length(x$LL[1,1,,1])
  K1 <- K - K2
  if (length(x) == 13) {
    if (x$design == "CROSSED") {
      cat(": changing CROSSED to FACTORIAL\n")
      x$design <- "FACTORIAL"
    }
  } else {
    cat(": dataset length = ", length(x), "\n")
  }
  # truthTableStr <- array(dim = c(I, J, K, max(x$lesionVector)+1))
  # truthTableStr[,,1:K1,1] <- 1
  # for (k2 in 1:K2) {
  #   truthTableStr[,,k2+K1,(1:x$lesionVector[k2])+1] <- 1
  # }
  fn <- paste0("~/GitHub/RJafroc/data/", dataStr[i], ".RData")
  if (writeFile) save(dataStr[i], file = fn)
}            

