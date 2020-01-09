library(RJafroc)
rm(list = ls())

lrocDataset <- DfFroc2Lroc(dataset05)

retFom <- UtilFigureOfMerit(lrocDataset, FOM = "PCL", FPFValue = 1)
retStPcl_10 <- StSignificanceTesting(lrocDataset, FOM = "PCL", FPFValue = 1)

retSt_Wilc <- StSignificanceTesting(lrocDataset, FOM = "Wilcoxon")
retStPcl_02 <- StSignificanceTesting(lrocDataset, FOM = "PCL")


SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "DBMH", list(varYTR = 1, varYTC = 2, varYEps = 3))

SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "ORH", list(KStar = 60, varTR = 1, cov1 = 2, cov2 = 3, cov3 = 4, varEps = 5))

SsSampleSizeKGivenJ(dataset02, J = 6, method = "DBMH")

SsSampleSizeKGivenJ(dataset02, J = 6, method = "ORH")