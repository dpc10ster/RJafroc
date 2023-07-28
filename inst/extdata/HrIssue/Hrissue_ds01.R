library(RJafroc)
stNew <- SignificanceTesting(dataset01, FOM = "HrSe", analysisOption = "RRRC")
stOld <- SignTestOldCode(dataset01, FOM = "HrSe", analysisOption = "RRRC")
