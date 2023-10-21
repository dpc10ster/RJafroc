FomAvgXModality <- function(dsX, foms) {
  
  modalityID1 <- dsX$descriptions$modalityID1
  modalityID2 <- dsX$descriptions$modalityID2
  readerID <- dsX$descriptions$readerID
  
  # learnApply(foms)
  # https://stackoverflow.com/questions/23302072/use-apply-on-a-multi-dimension-array
  
  fomArray <- list()
  modalityID <- list()
  modalityID[[1]] <- modalityID2
  modalityID[[2]] <- modalityID1
  for (i in 1:2) {
    fomArray[[i]] <- apply(foms, (1:3)[-i], mean) # average over first modality and all readers
    rownames(fomArray[[i]]) <- paste("trt", sep = "-", modalityID[[i]])
    colnames(fomArray[[i]]) <- paste("rdr", sep = "-", readerID)
  }
  
  return(fomArray)
  
}