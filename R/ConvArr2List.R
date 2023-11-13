ConvArr2List <- function(dataset, foms) {
  
  if (length(dim(dataset$ratings$NL)) != 5) stop ("ConvArr2List: expected a crossed modality dataset")
  modalityID1 <- dataset$descriptions$modalityID1
  modalityID2 <- dataset$descriptions$modalityID2
  readerID <- dataset$descriptions$readerID
  
  fomArray <- list()
  modalityID <- list()
  modalityID[[1]] <- modalityID2
  modalityID[[2]] <- modalityID1
  for (AvgIndx in 1:2) {
    fomArray[[AvgIndx]] <- apply(foms, (1:length(dim(foms)))[-AvgIndx], mean) # average over first modality and all readers
    rownames(fomArray[[AvgIndx]]) <- paste0("trt", modalityID[[AvgIndx]])
    colnames(fomArray[[AvgIndx]]) <- paste0("rdr", readerID)
  }
  names(fomArray) <- c("AvgMod1", "AvgMod2")
  
  return(fomArray)
  
}