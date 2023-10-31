rm(list = ls())

fn <- "~/GitHub/datasets/XModDataFile.xlsx"
ds <- DfReadXModalities(fn)

datasetXModality <- ds

save("datasetXModality", file = "~/GitHub/RJafroc/data/datasetXModality.RData")
