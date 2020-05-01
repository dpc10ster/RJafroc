library(RJafroc)
rm(list = ls())
rocData1R <- DfExtractDataset(dataset02, rdrs = 1)
rocData1T <- DfExtractDataset(dataset02, trts = 1)

retRRRC_DBMH <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH", option = "RRRC")
retFRRC_DBMH <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH", option = "FRRC")
retRRFC_DBMH <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "DBMH", option = "RRFC")

retFRRC_1R_DBMH <- StSignificanceTesting(rocData1R, FOM = "Wilcoxon", method = "DBMH", option = "FRRC")

retRRRC_ORH <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH", option = "RRRC")
retFRRC_ORH <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH", option = "FRRC")
retRRFC_ORH <- StSignificanceTesting(dataset02, FOM = "Wilcoxon", method = "ORH", option = "RRFC")

retFRRC_1R_ORH <- StSignificanceTesting(rocData1R, FOM = "Wilcoxon", method = "ORH", option = "FRRC")

ret <- StSignificanceTesting(dataset04, FOM = "HrAuc", method = "DBMH")
# lrocDataset <- DfFroc2Lroc(dataset05)
# 
# retFom <- UtilFigureOfMerit(lrocDataset, FOM = "PCL", FPFValue = 1)
# retStPcl_10 <- StSignificanceTesting(lrocDataset, FOM = "PCL", FPFValue = 1)
# 
# retSt_Wilc <- StSignificanceTesting(lrocDataset, FOM = "Wilcoxon")
# retStPcl_02 <- StSignificanceTesting(lrocDataset, FOM = "PCL")
# 
# 
# SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "ORH", list(varYTR = 1, varYTC = 2, varYEps = 3))
# 
# SsSampleSizeKGivenJ(dataset = NULL, J = 6, effectSize = 0.05, method = "ORH", list(KStar = 60, varTR = 1, cov1 = 2, cov2 = 3, cov3 = 4, varEps = 5))
# 
# SsSampleSizeKGivenJ(dataset02, J = 6, method = "DBMH")
# 
# SsSampleSizeKGivenJ(dataset02, J = 6, method = "ORH")