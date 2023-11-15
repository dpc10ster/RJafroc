rm(list = ls())

fn <- "~/GitHub/datasets/XModDataFile.xlsx"
datasetX <- DfReadXModalities(fn)

save("datasetX", file = "~/GitHub/RJafroc/data/datasetX.RData")
