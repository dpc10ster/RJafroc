library("RJafroc")
library("readxl")
library("xlsx")
library(dplyr)
x <- DfReadDataFile(fileName = "inst/Issue89/frocCr.xlsx", newExcelFileFormat = TRUE)
ret <- St(x, FOM = "AFROC", method = "OR", analysisOption = "RRRC")
ret$RRRC$ciDiffTrt


truth <- readxl::read_xlsx("inst/Issue89/frocCr.xlsx", sheet = "TRUTH")
tp <- readxl::read_xlsx("inst/Issue89/frocCr.xlsx", sheet = "TP")
fp <- readxl::read_xlsx("inst/Issue89/frocCr.xlsx", sheet = "FP")

xlsx::write.xlsx(as.data.frame(tp), file = "inst/Issue89/froc1.xlsx",
           sheetName = "TP", append = FALSE, row.names = F, showNA = F)
xlsx::write.xlsx(as.data.frame(fp), file = "inst/Issue89/froc1.xlsx", 
           sheetName="FP", append=TRUE, row.names = F)
xlsx::write.xlsx(as.data.frame(truth), file = "inst/Issue89/froc1.xlsx",
           sheetName="TRUTH", append=TRUE, row.names = F)
x <- DfReadDataFile(fileName = "inst/Issue89/froc1.xlsx", newExcelFileFormat = TRUE)
ret1 <- St(x, FOM = "AFROC", method = "OR", analysisOption = "RRRC")

#reorder tp and fp

tp <- tp %>% arrange(desc(tp))
fp <- fp %>% arrange(desc(fp))
head(tp)
head(fp)

xlsx::write.xlsx(as.data.frame(tp), file = "inst/Issue89/froc2.xlsx",
           sheetName = "TP", append = FALSE, row.names = F, showNA = F)
xlsx::write.xlsx(as.data.frame(fp), file = "inst/Issue89/froc2.xlsx", 
           sheetName="FP", append=TRUE, row.names = F)
xlsx::write.xlsx(as.data.frame(truth), file = "inst/Issue89/froc2.xlsx",
           sheetName="TRUTH", append=TRUE, row.names = F)
x <- DfReadDataFile(fileName = "inst/Issue89/froc2.xlsx", newExcelFileFormat = TRUE)
ret2 <- St(x, FOM = "AFROC", method = "OR", analysisOption = "RRRC")

ret1$RRRC
ret2$RRRC
testthat::expect_equal(ret1$RRRC, ret2$RRRC)
