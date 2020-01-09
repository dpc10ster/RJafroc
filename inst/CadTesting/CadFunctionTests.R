library(RJafroc)
rm(list = ls())

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 0.05, method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 1, method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 0.05, method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 1, method = "1T-RRFC")


StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 0.05, method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 1, method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 0.05, method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 1, method = "1T-RRRC")


StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 0.05, method = "2T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 0.2, method = "2T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", FPFValue = 1, method = "2T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 0.05, method = "2T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 0.2, method = "2T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", FPFValue = 1, method = "2T-RRRC")

# test with FROC dataset simulated from LROC data by SimulateFrocFromLrocData.R 
StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRRC")
