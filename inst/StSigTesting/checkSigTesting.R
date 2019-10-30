library(RJafroc)
rm(list = ls())

lrocDataset <- DfFroc2Lroc(dataset05)

retFom <- UtilFigureOfMerit(lrocDataset, FOM = "PCL", FPFValue = 1)
retStPcl_10 <- StSignificanceTesting(lrocDataset, FOM = "PCL", FPFValue = 1)

retSt_Wilc <- StSignificanceTesting(lrocDataset, FOM = "Wilcoxon")
retStPcl_02 <- StSignificanceTesting(lrocDataset, FOM = "PCL")
