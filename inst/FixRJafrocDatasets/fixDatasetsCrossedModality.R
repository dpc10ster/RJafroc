rm(list = ls())
load("~/GitHub/RJafroc/data/datasetCrossedModality.RData")
ds <- datasetCrossedModality

NL <- ds$NL
LLCl <- ds$LLCl
LLIl <- ds$LLIl
lesionNum <- ds$lesionNum
lesionID <- ds$lesionID
lesionWeight <- ds$lesionWeight
dataType <- ds$dataType
modalityID1 <- ds$modalityID1
modalityID2 <- ds$modalityID2
readerID <- ds$readerID

ds <- list(
  NL = NL,
  LLCL = LLCl,
  LLIl = LLIl,
  lesionVector = lesionNum,
  lesionID = lesionID,
  lesionWeight = lesionWeight,
  dataType = dataType,
  modalityID1 = modalityID1,
  modalityID2 = modalityID2,
  readerID = readerID
)

datasetCrossedModality <- ds

save("datasetCrossedModality", file = "~/GitHub/RJafroc/data/datasetCrossedModality.RData")
