rm(list = ls())
load("~/GitHub/RJafroc/data/datasetROI.RData")
ds <- datasetROI

NL <- ds$NL
LL <- ds$LL
lesionNum <- ds$lesionNum
lesionID <- ds$lesionID
lesionWeight <- ds$lesionWeight
dataType <- ds$dataType
modalityID <- ds$modalityID
readerID <- ds$readerID

ds <- list(
  NL = NL,
  LL = LL,
  lesionVector = lesionNum,
  lesionID = lesionID,
  lesionWeight = lesionWeight,
  dataType = dataType,
  modalityID = modalityID,
  readerID = readerID
)

datasetROI <- ds

save("datasetROI", file = "~/GitHub/RJafroc/data/datasetROI.RData")
