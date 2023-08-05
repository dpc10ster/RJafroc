rm(list = ls())

fn <- "~/GitHub/datasets/CrossedModalitiesDataFile.xlsx"
ds <- DfReadCrossedModalities(fn)

datasetXModality <- ds

save("datasetXModality", file = "~/GitHub/RJafroc/data/datasetXModality.RData")
