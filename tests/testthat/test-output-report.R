context("Output report")

# options not needed as output file produces all three options DPC 6/30/19
test_that("UtilOutputReport text format", {
  
  dataset_arr <- list(dataset02, dataset05)
  dataset_arr_str <- c("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc") #, "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBMH", "ORH")
  ## cycle through all representative datasets, FOMs, methods 
  for (d in 1:length(dataset_arr)) {
    dataset <- dataset_arr[[d]]
    for (i in 1:length(FOM_arr)) {
      for (j in 1:length(method_arr)) {
        if ((dataset$dataType == "ROC") && (FOM_arr[i] != "Wilcoxon")) {
          
          # for ROC data, only Wilcoxon FOM is allowed
          expect_error(UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], overwrite = TRUE))
          
        } else if ((dataset$dataType == "FROC") && (FOM_arr[i] == "Wilcoxon")) {
          
          # for FROC data, Wilcoxon FOM is NOT allowed
          expect_error(UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], overwrite = TRUE))
          
        } else {
          
          fn <- paste0(test_path(), "/goodValues/OutputReport/",dataset_arr_str[d], 
                       FOM_arr[i], method_arr[j], ".txt") # MUST be text format !! dpc 6/30/19
          if (!file.exists(fn)) {
            UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileName = fn, overwrite = TRUE)
          }
          
          fn2 <- paste0(test_path(), "/goodValues/OutputReport/",dataset_arr_str[d], 
                        FOM_arr[i], method_arr[j], "Temp.txt")
          UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileName = fn2, overwrite = TRUE)
          xx <- file(fn, open = "rt")
          xx1 <- readLines(xx);xx1 <- xx1[20:length(xx1)] # skip date related stuff which will change
          yy <- file(fn2, open = "rt")
          yy1 <- readLines(yy);yy1 <- yy1[20:length(yy1)] # skip date related stuff which will change
          close(xx);close(yy)
          expect_equivalent(xx1, yy1) # works!
          unlink(fn2)
          # end of test
          
        }  
      }
    }
  }
  
})


test_that("UtilOutputReportExcel", {

  fn <- paste0(test_path(), "/goodValues/OutputReport/", "dataset03", ".xlsx")
  if (!file.exists(fn)) {
    UtilOutputReport(dataset03, ReportFileName = fn, ReportFileFormat = "xlsx", overwrite = TRUE)
  }

  fn1 <- paste0(test_path(), "/goodValues/OutputReport/", "dataset03", "1.xlsx")
  UtilOutputReport(dataset03, ReportFileName = fn1, ReportFileFormat = "xlsx", overwrite = TRUE)
  for (i in 2:6) { # there are 3 worksheets in Excel file; skip summary sheet which has date stuff
    dfGood <- readWorkbook(fn, i) # check each sheet individually
    dfCurrent <- readWorkbook(fn1, i)    # do:
    expect_equivalent(dfGood, dfCurrent)# works!
  }
  unlink(fn1)
  # end of test
  
})


test_that("UtilOutputReportError", {
  fn <- system.file("extdata", "includedRocData.lrc",
                    package = "RJafroc", mustWork = TRUE)
  expect_error(
    UtilOutputReport(DataFileName = fn, overwrite = TRUE)) # If DataFileName is specified, then DataFileFormat must be specified
  
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData")) # If DataFileName is specified, then DataFileFormat must be specified
  
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData", DataFileFormat = "txt")) # data file format has to be JAFROC or MRMC,  or iMRMC
  
  expect_error(
    UtilOutputReport(dataset02, DataFileName = "MyData.tx1", DataFileFormat = "MRMC")) # data file must be *.lrc or excel
  
})

