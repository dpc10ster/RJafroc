rm(list = ls())

fn <- "~/GitHub/datasets/XModDataFile.xlsx"
ds <- DfReadCrossedModalities(fn)

datasetXModality <- ds

save("datasetXModality", file = "~/GitHub/RJafroc/data/datasetXModality.RData")
