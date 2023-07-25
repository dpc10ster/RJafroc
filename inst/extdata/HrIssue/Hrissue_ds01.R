library(RJafroc)
stNew <- StSignificanceTesting(dataset01, FOM = "HrSe", analysisOption = "RRRC")
stOld <- StSignTestOldCode(dataset01, FOM = "HrSe", analysisOption = "RRRC")
