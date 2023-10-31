ConvArr2List <- function(dataset, foms) {
  
  if (length(dim(dataset$ratings$NL)) != 5) stop ("ConvArr2List: expected a crossed modality dataset")
  modalityID1 <- dataset$descriptions$modalityID1
  modalityID2 <- dataset$descriptions$modalityID2
  readerID <- dataset$descriptions$readerID
  
  # learnApply(foms)
  # https://stackoverflow.com/questions/23302072/use-apply-on-a-multi-dimension-array
  
  fomArray <- list()
  modalityID <- list()
  modalityID[[1]] <- modalityID2
  modalityID[[2]] <- modalityID1
  for (AvgIndx in 1:2) {
    fomArray[[AvgIndx]] <- apply(foms, (1:3)[-AvgIndx], mean) # average over first modality and all readers
    rownames(fomArray[[AvgIndx]]) <- paste("trt", sep = "-", modalityID[[AvgIndx]])
    colnames(fomArray[[AvgIndx]]) <- paste("rdr", sep = "-", readerID)
  }
  names(fomArray) <- c("AvgMod1", "AvgMod2")
  
  return(fomArray)
  
}