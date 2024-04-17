library(RJafroc)
fileName <- system.file("extdata", "/toyFiles/ROC/rocSpA-unbalanced.xlsx",
                        package = "RJafroc", mustWork = TRUE)
dsSpA <- DfReadSP(fileName)
ret <- StSP(dsSpA, FOM = "Wilcoxon")
