rm(list = ls())
load("~/GitHub/RJafroc/data/datasetCadLroc.RData")
ds <- datasetCadLroc

NL <- ds$NL
LLCl <- ds$LLCl
LLIl <- ds$LLIl
lesionNum <- ds$lesionNum
lesionID <- ds$lesionID
lesionWeight <- ds$lesionWeight
dataType <- ds$dataType
modalityID <- ds$modalityID
readerID <- ds$readerID

ds <- list(
  NL = NL,
  LLCl = LLCl,
  LLIl = LLIl,
  lesionVector = lesionNum,
  lesionID = lesionID,
  lesionWeight = lesionWeight,
  dataType = dataType,
  modalityID = modalityID,
  readerID = readerID,
  datasetName = "Ignore"
)

datasetCadLroc <- ds

save("datasetCadLroc", file = "~/GitHub/RJafroc/data/datasetCadLroc.RData")
