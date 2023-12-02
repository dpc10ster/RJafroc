library(RJafroc)

fileName <- system.file(
  "extdata", "/toyFiles/ROC/rocSpAZP.xlsx", package = "RJafroc", mustWork = TRUE)
dsSp <- DfReadDataFile(fileName)

StSP(dsSp, FOM = "Wilcoxon")

StSP(dsSp, FOM = "Wilcoxon")

