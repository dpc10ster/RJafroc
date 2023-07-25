library(RJafroc)
fileName <- system.file(
"extdata", "/HrIssue/FRoc-Prostate.xlsx", package = "RJafroc", mustWork = TRUE)
dataset <- DfReadDataFile(fileName)
#dataset <- DfFroc2Roc(dataset)
#

st <- StSignificanceTesting(dataset, FOM = "HrSe", analysisOption = "RRRC")
