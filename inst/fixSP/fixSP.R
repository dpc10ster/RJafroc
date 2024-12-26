#library(RJafroc)
fileName <- system.file("extdata", "/toyFiles/ROC/rocSpA-unbalanced.xlsx",
#fileName <- system.file("extdata", "/toyFiles/ROC/rocSpA.xlsx",
                        package = "RJafroc", mustWork = TRUE)
dsSpA <- DfReadSP_A(fileName)
ret <- StSP(dsSpA, FOM = "Wilcoxon")
