library(RJafroc)
rm(list = ls())

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRFC")

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.05)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 0.2)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRFC", FPFValue = 1)

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.05)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 0.2)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRFC", FPFValue = 1)


StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "1T-RRRC")

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.05)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 0.2)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "1T-RRRC", FPFValue = 1)

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.05)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 0.2)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "1T-RRRC", FPFValue = 1)


StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "Wilcoxon", method = "2T-RRRC")

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.05)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 0.2)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "PCL", method = "2T-RRRC", FPFValue = 1)

StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.05)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 0.2)
StSignificanceTestingCadVsRadiologists(datasetCadLroc, FOM = "ALROC", method = "2T-RRRC", FPFValue = 1)

# test with FROC dataset simulated from LROC data by SimulateFrocFromLrocData.R 
StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRFC")
StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRFC")

StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "HrAuc", method = "1T-RRRC")
StSignificanceTestingCadVsRadiologists(datasetCadSimuFroc, FOM = "wAFROC", method = "1T-RRRC")
