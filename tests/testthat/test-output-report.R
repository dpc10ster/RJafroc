context("Output report")

# options not needed as output file produces all three options DPC 6/30/19
test_that("UtilOutputReport text format", {
  
  dataset_arr <- list(dataset02, dataset05)
  dataset_arr_str <- c("dataset02", "dataset05")
  FOM_arr <- c("Wilcoxon", "HrAuc", "wAFROC1","AFROC1","MaxLLF","MaxNLF","MaxNLFAllCases", "ExpTrnsfmSp", "HrSp", "HrSe")
  method_arr <- c("DBMH", "ORH")
  options_arr <- c("RRRC", "FRRC", "RRFC")
  ## cycle through all representative datasets, FOMs, methods and options
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
          
          fn1 <- paste0(test_path(), "/goodValues/OutputReport/",dataset_arr_str[d], 
                       FOM_arr[i], method_arr[j], ".txt")
          fn <- paste0(test_path(), "/goodValues/OutputReport/", dataset_arr_str[d], 
                       FOM_arr[i], method_arr[j])
          if (!file.exists(fn)) {
            ret <- UtilOutputReport(
              dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileName = fn1, overwrite = TRUE)
            saveRDS(ret, file = fn)
          }
          unlink(fn1)
          ret <- readRDS(fn)
          expect_equal(
            UtilOutputReport(dataset, FOM = FOM_arr[i], method = method_arr[j], ReportFileName = fn1, overwrite = TRUE), ret)
          unlink(fn1)
          # end of test
          
        }  
      }
    }
  }
})


# test_that("UtilOutputReportExcel", {
#   
#   # fn <- paste0(test_path(), "/goodValues/OutputReport/", "03", "Excel")
#   # if (!file.exists(fn)) {
#   #   ret <- UtilOutputReport(dataset03, ReportFileFormat = "xlsx", overwrite = TRUE)
#   #   saveRDS(ret, file = fn)
#   # }
#   # 
#   # ret <- readRDS(fn)
#   # expect_equal(UtilOutputReport(dataset03, ReportFileFormat = "xlsx", overwrite = TRUE), ret)
#   # # end of test
#   
# })

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

